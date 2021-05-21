Esame fondamenti di scienza dei dati e laboratorio
================

## Obiettivo

L’avvento di internet e la sua diffusione in tutto il mondo ha
trasformato completamente il cinema. Se, solo pochi anni fa, per
guardare un film si doveva andare al cinema o acquistare i DvD, adesso
sono disponibili dei servizi che attraverso un pagamento mensile offrono
all’utente centinaia e migliaia di contenuti. L’obiettivo di questa
relazione è quello di capire se ha davvero senso averli tutti oppure se
ce ne sono alcuni migliori di altri. Andremo ad analizzare quale (se
esiste) sia il migliore servizio di streaming e in seguito vedremo
quello più utilizzato attraverso un sondaggio. Infine, tutta questa
concorrenza causa problemi ai consumatori?

## Dataset

In questa progetto andremo ad utilizzare 2 dataset che contengono
l’elenco di tutti i film e serie tv presenti su 4 piattaforme. La fonte
dei dati è: <https://github.com/AlvisioPirozzi/Net-Prime-Dis-Hu-scraper>
che contiene gli script in python e in javascript utilizzati per lo
scraping dei dati. Dato che la maggior parte delle piattaforme ha
recentemente bloccato l’utilizzo di questi script chiudendo gli account
di chi li utilizza i dati si fermano a fine 2020. Verranno analizzate
tutte le serie tv e i film presenti su: Netflix, PrimeVideo, Disney+ e
Hulu. ![Loghi](img/loghi/loghi.png)

``` r
# Lettura Dataset
film <- read.csv("films.csv")
serie <- read.csv("tv_shows.csv")
```

## Normalizzazione Database

Prima di iniziare ad analizzare i dataset andremo ad eliminare tutti i
dati non necessari per il nostro scopo così da diminuire i tempi di
compilazione ed evitare eventuali problemi in seguito:

Le ulteriori operazioni da eseguire saranno: 1. **Titoli minuscoli**:
Per evitare problemi nei confronti tra servizi rendo tutti i tioli in
minoscolo. 1. **Unione dataset**: creazione unico dataset che comprende
sia film che serie tv 1. **ridenominazione**: per aumentare la chiarezza
ed eliminare gli spazi per problemi di compattibilità con SQL. Esempio:
“prime video” diventa “prime\_video”.

``` r
names(film) <- tolower(names(film)) #titoli in minuscolo
names(serie) <- tolower(names(serie))
data <- bind_rows(film, serie)  #unione dataset
data <- data[,c(3,4,6:17)] #cancello colonne che non verranno usate
colnames(data)[7] <- "prime_video"
colnames(data)[8] <- "disney+"
data$type[data$type == 0] <- "film" #se il tipo è 0 è un film. se 1 una serie
data$type[data$type == 1] <- "serie"
```

Infine, il dataset completo sarà composto da 22355 righe

## Quale servizio ha più contenuti?

``` r
options(repr.plot.width = 16, repr.plot.height = 8)
platform_summary <- data[,5:9] %>% group_by(type) %>% summarize_all(sum)
platform_summary <-  melt(as.data.table(platform_summary), id.vars="type")
colnames(platform_summary) <- c("type", "platform", "quantity")
platform_summary$platform <- gsub("_"," ",platform_summary$platform)
ggplot(platform_summary, aes(fill=type, x = reorder(platform, - quantity), y = quantity)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#0000ff", "#E50914")) +
  theme_classic() +
  ggtitle("Numero di film/serie per servizio") +
  xlab("Servizio") +
  ylab("Numero") +
  geom_text(aes(label=quantity), position=position_dodge(width=0.9), vjust=-0.25, size=5) +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())
```

![](github_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
data$availability <- rowSums(data[,5:8])
table(data$availability)
```

    ## 
    ##     1     2     3 
    ## 21362   951    42

``` r
cat("The following percentage of titles are unique:", "\n" ,
             "Netflix - ", percent(nrow(data[data$netflix == 1 & data$availability == 1,])/nrow(data[data$netflix == 1,])), "\n",
             "Prime Video - ", percent(nrow(data[data$prime_video == 1 & data$availability == 1,])/nrow(data[data$prime_video == 1,])),"\n",
             "HULU - ", percent(nrow(data[data$hulu == 1 & data$availability == 1,])/nrow(data[data$hulu == 1,])),"\n",
             "Disney+ - ", percent(nrow(data[data$`disney+` == 1 & data$availability == 1,])/nrow(data[data$`disney+` == 1,])))
```

    ## The following percentage of titles are unique: 
    ##  Netflix -  90% 
    ##  Prime Video -  94% 
    ##  HULU -  79% 
    ##  Disney+ -  92%

``` r
# plot new content quantity per platform
new_content_platform <- sqldf("SELECT 
              year,
              case 
                  when netflix = 1 then 'netflix'
                  when hulu = 1 then 'hulu'
                  when prime_video = 1 then 'prime_video'
                  else 'disney+'
              end as platform,
              count(1) as quantity,
              round(avg(imdb), 1) as avg_imdb_rating
      FROM 
              data
     
      GROUP BY
              year,
              case 
                  when netflix = 1 then 'netflix'
                  when hulu = 1 then 'hulu'
                  when prime_video = 1 then 'prime_video'
                  else 'disney+'
              end")
ggplot(new_content_platform, aes(fill=platform, x = year, y = quantity)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#fbbe4f", "#99cc33", "#E50914", "#00A8E1")) +
  theme_classic() +
  #geom_line(color="red")+
  ggtitle("Nuovi film/serie per anno") +
  xlab("Anno") +
  ylab("Quantità") +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())
```

![](github_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# plot new content quantity per platform
new_content_platform <- sqldf("SELECT 
              year,
              case 
                  when netflix = 1 then 'netflix'
                  when hulu = 1 then 'hulu'
                  when prime_video = 1 then 'prime_video'
                  else 'disney+'
              end as platform,
              count(1) as quantity,
              round(avg(imdb), 1) as avg_imdb_rating
      FROM 
              data
      WHERE
              year >= 2000 AND year 
              
     
      GROUP BY
              year,
              case 
                  when netflix = 1 then 'netflix'
                  when hulu = 1 then 'hulu'
                  when prime_video = 1 then 'prime_video'
                  else 'disney+'
              end")
ggplot(new_content_platform, aes(fill=platform, x = year, y = quantity)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#fbbe4f", "#99cc33", "#E50914", "#00A8E1")) +
  theme_classic() +
  ggtitle("Nuovi film/serie per anno") +
  xlab("Anno") +
  ylab("Quantità") +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())
```

![](github_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
#install.packages('gifski')
#install.packages('png')
new_content_platform <- sqldf("SELECT 
              year,
              case 
                  when netflix = 1 then 'netflix'
                  when hulu = 1 then 'hulu'
                  when prime_video = 1 then 'prime_video'
                  else 'disney+'
              end as platform,
              count(1) as quantity,
              round(avg(imdb), 1) as avg_imdb_rating
      FROM 
              data
      WHERE
              year >= 1995 AND year 
              
     
      GROUP BY
              year,
              case 
                  when netflix = 1 then 'netflix'
                  when hulu = 1 then 'hulu'
                  when prime_video = 1 then 'prime_video'
                  else 'disney+'
              end")

g <-ggplot(new_content_platform, aes(fill=platform, x = platform, y = quantity)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#fbbe4f", "#99cc33", "#E50914", "#00A8E1")) +
  theme_classic() +
  ggtitle("Nuovi film/serie per anno") +
  xlab("") +
  ylab("Quantità") +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

anim <- g + 
  transition_states(year,
                    transition_length = 0,
                    state_length = 1) + 
  ggtitle('{closest_state}')
  

anim
```

![](github_files/figure-gfm/unnamed-chunk-9-1.gif)<!-- -->

``` r
genres <- unique(unlist(strsplit(as.character(data$genres), ",")))
genres <- genres[!genres %in% NA] # remove missing genre

# Create dataframe and calculate median rating per genre
genre_rating <- data.frame(genre=character(),
                           average_rating=double(), 
                           netflix_rating=double(),
                           hulu_rating=double(), 
                           prime_rating=double(),
                           disney_rating=double(),
                           total_quantity = integer(),
                           netflix_quantity = integer(),
                           hulu_quantity = integer(),
                           prime_video_quantity = integer(),
                           disney_quantity = integer(),
                           stringsAsFactors=FALSE) 

for(i in 1:length(genres)) {
  
  genre <- genres[i]
  
  temp <- data[data$genres %like% genre,]
  
  # average of all
  genre_rating[i,1] <- genre
  genre_rating[i,2] <- round(mean(temp$imdb, na.rm = T), 1)
  genre_rating[i,3] <- round(mean(temp$imdb[temp$netflix == 1], na.rm = T), 1)
  genre_rating[i,4] <- round(mean(temp$imdb[temp$hulu == 1], na.rm = T), 1)
  genre_rating[i,5] <- round(mean(temp$imdb[temp$prime_video == 1], na.rm = T), 1)
  genre_rating[i,6] <- round(mean(temp$imdb[temp$`disney+` == 1], na.rm = T), 1)
  genre_rating[i,7] <- nrow(temp)
  genre_rating[i,8] <- nrow(temp[temp$netflix == 1, ])
  genre_rating[i,9] <- nrow(temp[temp$hulu == 1, ])
  genre_rating[i,10] <- nrow(temp[temp$prime_video == 1, ])
  genre_rating[i,11] <- nrow(temp[temp$`disney+` == 1, ])
}

#cancello i generi con meno di 105 contenuti
genre_rating_bassi <- genre_rating[c(27, 22, 23, 21, 19, 6, 12, 7,16,9,11,14), ]
genre_rating <- genre_rating[-c(26, 25, 24, 27, 22, 23, 21, 19, 6, 12, 7,16,9,11,14), ]

ggplot(genre_rating_bassi, aes(x =  reorder(genre, -total_quantity), y = total_quantity)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_bar(stat="identity", fill="#0000FF") +
  theme_classic() +
  labs(title="") +
  xlab("") +
  ylab("Quantità") +
  geom_text(aes(label=total_quantity), colour = "yellow", position=position_dodge(width=1), hjust=+1.07, size=5) +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  coord_flip()
```

![](github_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
ggplot(genre_rating, aes(x =  reorder(genre, -total_quantity), y = total_quantity)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_bar(stat="identity", fill="#0000FF") +
  theme_classic() +
  labs(title="") +
  xlab("") +
  ylab("Quantità") +
 geom_text(aes(label=total_quantity), colour = "yellow", position=position_dodge(width=1), hjust=+1.07, size=5) +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank()) +
  coord_flip() 
```

![](github_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

``` r
 g2<- ggplot(genre_rating, aes(x =  reorder(genre, -total_quantity), y = average_rating)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_bar(stat="identity", fill="#FFA500") +
  theme_classic() +
  labs(title="") +
  xlab("") +
  ylab("") +
  ylim(0, 8) +
  geom_text(aes(label=average_rating), position=position_dodge(width=1), vjust=0.25,hjust=-0.25) +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5)) +
  coord_flip()

g1 <- ggplot(genre_rating_bassi, aes(x =  reorder(genre, -total_quantity), y = average_rating)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_bar(stat="identity", fill="#FFA500") +
  theme_classic() +
  labs(title="IMDB voti") +
  xlab("") +
  ylab("") +
  ylim(0, 8) +
  geom_text(aes(label=average_rating), position=position_dodge(width=1), vjust=0.25,hjust=-0.25) +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5)) +
  coord_flip()

grid.arrange(g1, g2, ncol=2)
```

![](github_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
imdb_ratings_platform <- sqldf("SELECT 
              type,
              case 
                  when netflix = 1 then 'netflix'
                  when hulu = 1 then 'hulu'
                  when prime_video = 1 then 'prime_video'
                  else 'disney+'
              end as platform,
              round(avg(imdb), 1) as avg_imdb_rating
      FROM 
              data
      GROUP BY
              type,
              case 
                  when netflix = 1 then 'netflix'
                  when hulu = 1 then 'hulu'
                  when prime_video = 1 then 'prime_video'
                  else 'disney+'
              end")

ggplot(imdb_ratings_platform, aes(fill=type, x =  reorder(platform, -avg_imdb_rating), y = avg_imdb_rating)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#fbbe4f", "#E50914")) +
  theme_classic() +
  ggtitle("IMDb rating of Content by Platform") +
  xlab("Platform") +
  ylab("Rating") +
  coord_cartesian(ylim=c(2,8)) +
  geom_text(aes(label=avg_imdb_rating), position=position_dodge(width=0.9), vjust=-0.25, size=5) +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())
```

![](github_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

\#\#PARTE 2

``` r
sondaggio <- read.csv("Risultati_sondaggio.csv")
names(sondaggio)[4] <- "Disney+"
```

``` r
somma_servizi <- sondaggio[,2:12] %>% summarize_all(sum)


inverso <- data.frame(colnames(sondaggio[,2:12]), t(somma_servizi)[1:11])
names(inverso)[1] <- "Servizio"
names(inverso)[2] <- "Numero"

ggplot(inverso, aes(x =  reorder(Servizio, -Numero), y = Numero)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_bar(stat="identity", fill="#FFA500") +
  theme_classic() +
  labs(title="Risultati sondaggio") +
  xlab("") +
  ylab("") +
  geom_text(aes(label=Numero), position=position_dodge(width=1), vjust=0.25,hjust=-0.25) +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5)) +
  coord_flip()
```

![](github_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

\#\#Ipotesi sul target di servizio per una età specifica

``` r
# plot new content quantity per platform
eta<- sqldf("SELECT 
              eta,
              count(1) as Risposte
      FROM 
              sondaggio
    
      GROUP BY
              eta")
ggplot(eta, aes( x = eta, y = Risposte, colour="Red")) + 
  geom_line( colour="black")+
  #geom_point(Color="black") +
  geom_smooth()+
  theme_classic() +
  ggtitle("Età di chi ha risposto al sondaggio") +
  xlab("Età") +
  scale_x_continuous(breaks = seq(15, 75, by = 5))+
  ylab("Quantità") +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](github_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
# plot new content quantity per platform
names(sondaggio)[4] <- "Disney"

eta_servizi <- sqldf("SELECT 
              eta,
              case 
                  when Netflix = 1 then 'Netflix'
                  when Hulu = 1 then 'Hulu'
                  when PrimeVideo = 1 then 'PrimeVideo'
                  When Disney = 1 then 'Disney'
                  When TimVision = 1 then 'TimVision'
                  When NowTV = 1 then 'NowTV'
                  When Infinity = 1 then 'Infinity'
                  When Dplay = 1 then 'Dplay'
                  When Chili = 1 then 'Chili'
                    When AppleTV = 1 then 'AppleTV'
                    When Crunchyroll = 1 then 'Crunchyroll'
                    else 'Nessuno'
              end as Piattaforma,
              count(1) as Quantita
              
      FROM 
              sondaggio
     
      GROUP BY
              eta,
              case 
                  when Netflix = 1 then 'Netflix'
                  when Hulu = 1 then 'Hulu'
                  when PrimeVideo = 1 then 'PrimeVideo'
                   When Disney = 1 then 'Disney'
                   When TimVision = 1 then 'TimVision'
                  When NowTV = 1 then 'NowTV'
                  When Infinity = 1 then 'Infinity'
                  When Dplay = 1 then 'Dplay'
                  When Chili = 1 then 'Chili'
                    When AppleTV = 1 then 'AppleTV'
                    When Crunchyroll = 1 then 'Crunchyroll'
                    else 'Nessuno'
              end")


eta_servizi<-eta_servizi[!(eta_servizi$Piattaforma=="Nessuno"),]


ggplot(eta_servizi, aes(fill=Piattaforma, x = eta, y = Quantita)) + 
  #geom_bar(position="dodge", stat="identity") +
  geom_point(aes(col=Piattaforma, size=Quantita)) +
  theme_classic() +
  scale_x_continuous(breaks = seq(15, 75, by = 5))+
  ggtitle("Servizi usati in base all'età") +
  xlab("Età") +
  ylab("Quantità") +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())
```

![](github_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
library(gganimate)
#install.packages('gifski')
#install.packages('png')

g <- ggplot(eta_servizi, aes(fill=Piattaforma, x = eta, y = Quantita)) + 
  geom_point(aes(col=Piattaforma, size=3)) +
  theme_classic() +
  
  scale_x_continuous(breaks = seq(15, 75, by = 5))+
  #ggtitle("Netflix in base all'età") +
  xlab("Età") +
 
  ylab("Quantità") +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

anim <- g + 
  transition_states(Piattaforma,
                    transition_length = 0,
                    state_length = 1) + 
  ggtitle('{closest_state}')
  

anim
```

![](github_files/figure-gfm/unnamed-chunk-17-1.gif)<!-- -->

``` r
eta_servizi_netflix<-eta_servizi[(eta_servizi$Piattaforma=="Netflix"),]
eta_servizi_netflix2<-eta_servizi_netflix[(eta_servizi_netflix$Quantita>1 ),]
gg1 <- ggplot(eta_servizi_netflix, aes(fill=Piattaforma, x = eta, y = Quantita)) + 
  #geom_bar(position="dodge", stat="identity") +
  geom_point(aes(col=Piattaforma, size=Quantita)) +
  theme_classic() +
  geom_smooth(method="loess", se=F) +
  geom_encircle(aes(x=eta, y=Quantita), 
              data=eta_servizi_netflix2, 
              color="red", 
               size=2, 
              alpha=0.2,
               expand=0.1) +
  scale_x_continuous(breaks = seq(15, 75, by = 5))+
  ggtitle("Netflix in base all'età") +
  xlab("Età") +
  ylab("Quantità") +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())
```

``` r
eta_servizi_Prime<-eta_servizi[(eta_servizi$Piattaforma=="PrimeVideo"),]
eta_servizi_Prime2<-eta_servizi_Prime[(eta_servizi_Prime$Quantita>1 ),]
gg2 <- ggplot(eta_servizi_Prime, aes(fill=Piattaforma, x = eta, y = Quantita)) + 
  #geom_bar(position="dodge", stat="identity") +
  geom_point(aes(col=Piattaforma, size=Quantita)) +
  theme_classic() +
  geom_smooth(method="loess", se=F) +
  geom_encircle(aes(x=eta, y=Quantita), 
              data=eta_servizi_Prime2, 
              color="red", 
               size=2, 
              alpha=0.2,
               expand=0.1) +
  scale_x_continuous(breaks = seq(15, 75, by = 5))+
  ggtitle("PrimeVideo in base all'età") +
  xlab("Età") +
  ylab("Quantità") +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())


gg1
```

    ## `geom_smooth()` using formula 'y ~ x'

![](github_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
gg2
```

    ## `geom_smooth()` using formula 'y ~ x'

![](github_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
gg1 <- ggplot(eta_servizi_netflix, aes(fill=Piattaforma, x = eta, y = Quantita)) + 
  #geom_bar(position="dodge", stat="identity") +
  #geom_point(aes(col=Piattaforma, size=Quantita)) +
  theme_classic() +
  geom_smooth(method="loess", se=F) +
 
  scale_x_continuous(breaks = seq(15, 75, by = 5))+
  ggtitle("Netflix in base all'età") +
  xlab("Età") +
  ylab("Quantità") +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

gg2 <- ggplot(eta_servizi_Prime, aes(fill=Piattaforma, x = eta, y = Quantita)) + 
  #geom_bar(position="dodge", stat="identity") +
  #geom_point(aes(col=Piattaforma, size=Quantita)) +
  theme_classic() +
  geom_smooth(method="loess", se=F) +
  scale_x_continuous(breaks = seq(15, 75, by = 5))+
  scale_y_continuous(breaks = seq(0, 3, by = 1))+
  ggtitle("PrimeVideo in base all'età") +
  xlab("Età") +
  ylab("Quantità") +
  theme(text = element_text(size=20), plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())


grid.arrange(gg1, gg2, ncol=1)
```

    ## `geom_smooth()` using formula 'y ~ x'
    ## `geom_smooth()` using formula 'y ~ x'

![](github_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->