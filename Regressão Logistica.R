install.packages("pacman")

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, stringr, tidyr, magrittr, hrbrthemes, car, tidyverse, psych, MASS,  DescTools, QuantPsyc) #pacotes basicos

dados<-read.csv('dataset.csv', sep = ',', dec = '.', stringsAsFactors = TRUE)

glimpse(dados)

selected_columns <- c("popularity", "duration_ms", "explicit", "danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "track_genre")

remove_outliers <- function(data, col) {
  quartiles <- quantile(col, probs=c(.25, .75), na.rm = FALSE)
  IQR <- IQR(col)
  
  Lower <- quartiles[1] - 1.5*IQR
  Upper <- quartiles[2] + 1.5*IQR 
  
  return(subset(data, col > Lower & col < Upper))
}

dados <- remove_outliers(dados, dados$popularity)
dados <- remove_outliers(dados, dados$danceability)
dados <- remove_outliers(dados, dados$duration_ms)
dados <- remove_outliers(dados, dados$tempo)
dados <- remove_outliers(dados, dados$loudness)
dados <- remove_outliers(dados, dados$valence)


#Retirados as entradas com valor 0 na popularidade
dados <- dados %>% filter(popularity>0)

#criando variaveis binárias a partir de quantitativas
dados <- dados %>% mutate (popular = ifelse(popularity > 40, "Sim", "Não"))
dados <- dados %>% mutate (dançavel = ifelse(danceability > 0.58, "Sim", "Não"))
dados <- dados %>% mutate (happy = ifelse(valence > 0.48, "Sim", "Não"))

#transformando as variáveis binárias em fator
dados$popular <- as.factor(dados$popular)
dados$dançavel <- as.factor(dados$dançavel)
dados$happy <- as.factor(dados$happy)
dados$explicit <- as.factor(dados$explicit)
dados$key <- as.factor(dados$key)
dados$mode <- as.factor(dados$mode)
dados$time_signature <- as.factor(dados$time_signature)

model <- glm(happy ~ popularity + danceability, family = binomial(link = 'logit'), data = dados)

summary(stdres(mod))


#ggplot(dados, aes(x=acousticness, y=happy)) + geom_point() +
 # stat_smooth(method="glm", color="green", se=FALSE, 
  #            method.args = list(family=binomial))
summary(dados)

View(dados)


