#Direcionado o R para o Diretorio a ser trabalhado
setwd('C:/Users/Desktop')

#Limpa o Ambiente Global
rm(list=ls())

#Inicio do Script
#Analise de Regressão com o R


#Pacotes a serem utilizados

library(kableExtra)
library(urca)
library(foreign)
library(readxl)
library(mFilter)
library(forecast)
library(dplyr)
library(tsutils)
library(xts)
library(ggthemes)
library(FinTS)
library(scales)
library(quantmod)
library(patchwork)#unir graficos ggplot
library(httr)
library(jsonlite)
library(optimr)
library(deSolve)
library(tidyverse)
library(lubridate)
library("growthrates")
library(growthmodels)
library(ggplot2)
library(greybox)##pacote para função MIS
library(nlme)
library(car)
library(ggpubr)
library(tidyquant)
library(dotwhisker)
library(splines2)
library(foreign)
library(vcd)
library(tsModel)
library(Epi)
library(readr)
library(lmtest)
library(outliers)
library(pillar)
library(psych)
library(QuantPsyc)
library(scatterplot3d)
library(rstatix)
library(AER)
library(stargazer)
library(robustbase)
library(gmm)
library(ggpmisc)
library(dlookr)
library(corrplot)
library(tseries)
library(quantmod)
library(gridExtra)
library(dygraphs)
library(zoo)#Para calcular a média móvel
library(greybox)
library(magrittr)# pipe operations
library(DT)


#Entrando dados
dados <- read_excel("C:/Users/obitos.xlsx")
attach(dados)



#Selecionando todos os casos
obitospe <- dados %>% filter(obitos>=1)


# Gerando a variável novos casos
day1 <- min(obitos)
obitospe %<>% mutate(novos.obitos = obitos - lag(obitos, n=1))

#Calculando  media movel de 14 dias
media_movel_pe <- obitospe %<>%
  select(c(date,novos.obitos)) %<>%
  mutate(m.movel.pe.14=rollmean(novos.obitos,k=14,fill=NA)) 


# Tabela com os valores
mm_table <- media_movel_pe[,-1]
datatable(round(tail(mm_table, 20),1), option = list(
  autoWidth = TRUE,
  columnDefs = list(list(className = 'dt-center', targets = "_all"))))

#Gráfico dos novos casos e Média Móvel
mycolor1 <- "lightblue4"
mycolor2 <- "red"
covidpe <- ggplot(data=media_movel_pe) + #estetica vai valer para todos os geom's  
  geom_col(aes(x=date, y=novos.obitos, fill="Casos de Covid-19"), lwd=1)+
  scale_fill_manual(values=mycolor1)+
  geom_line(aes(x=date, y=m.movel.pe.14, colour="Média Móvel de 14 dias"), size=2)+
  scale_colour_manual(values=mycolor2)+
  labs(x=NULL,y='Covid-19 em Pernambuco',  #Titulos dos eixos
       col=NULL)+
  theme_minimal()+ #Definindo tema
  theme(legend.position = 'bottom', 
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) # Definindo posicao da legenda

covidpe


#Setando como série temporal
dados <- ts(dados, start=c(2020,04), frequency = 365) #definindo como serie temporal

#Rotular
dbgg <- dados[,c(2)]

date <- seq(as.Date('2020-04-03'), to=as.Date("2022-10-05"),by='1 day')
dbgg2 <- tibble(date,dbgg)

ggplot(data=dbgg2) +
  geom_line(aes(x=date, y=dbgg), colour="darkblue", linewidth=1.0) +
  xlab('Meses do Ano') + ylab('Número de casos') +
  ggtitle('Evolução de óbitos por COVID19')


#Correlograma da série
#Visualizando da série, da FAC e da FACp 
ggtsdisplay(dbgg, main='dados')


#TESTE DE RAIZ UNITÁRIA
##Correlograma das series
#Visualizando as duas funcoes - FAC e FACP

#Analise dos testes de raiz unitaria em nivel
#Definicao do numero maximo de lags
print(12*(length(dbgg)/100)^(1/4))

#Estatistica de Teste DFGLS para DCP
dbgg.dfgls <- ur.ers(dbgg, type = c("DF-GLS"),
                     model = c("trend"),
                     lag.max = 2)
summary(dbgg.dfgls)


#ANALISE NA 1 DIFERENÇA
#Diferenciacao da Serie
ddbgg <- diff(dbgg)

##Correlograma das series na 1 Diferença
#Visualizando as duas funcoes - FAC e FACP

ggtsdisplay(ddbgg, main='Evolução de óbitos por COVID19')

# Teste de Raiz Unitária
ddbgg.dfgls <- ur.ers(ddbgg, type = c("DF-GLS"),
                      model = c("constant"),
                      lag.max = 2)
summary(ddbgg.dfgls)


#Estimação do ARIMA
auto.arima <- auto.arima(dbgg, max.p=10, max.d=1, max.q=10, trace=TRUE)

summary(auto.arima)
coeftest(auto.arima)

#Visualizacao das duas series - observada e a estimada
plot(dbgg)
lines(fitted(auto.arima), col='red')
legend('topleft', col=c('black','red'), lty=c(1,1), lwd=c(2,2),
       legend=c('DBGG', 'DBGG-Estimado'))

#Analise dos residuos do ARIMA
plot(auto.arima$residuals)

#Teste de Ljung Box para autocorrelacao nos residuos
#Hipotese nula do teste é que os residuos nao sao autocorrelacionados

Box.test(auto.arima$residuals, lag=20,
         type="Ljung-Box", fitdf=length(coef(auto.arima)))

# FAC e FACp dos resíduos
ggtsdisplay(auto.arima$residuals, main='FAC e FACp dos resíduos')

#Teste de Normalidade dos residuos
shapiro.test(auto.arima$residuals)

#E possivel fazer previsoes com os resultados
forecast.arima <- forecast(auto.arima, h=12, level=c(20, 40, 60)) 
#level é o intervalo de confianca da previsao

forecast.arima

#Grafico da previsao
autoplot(forecast.arima)+
  ggtitle('Previsão do Arima (6,1,0)')

#Avaliacao da Previsao
end(dbgg)

fim <- c(2022,10)
amostra <- window(dbgg, end=fim)
observado <- window(dbgg, start=fim+c(0,1))

#Reestimacao do modelo
arima2 <- Arima(amostra, order=c(6,1,0))
coeftest(arima2)

summary(arima2)

#previsao com o novo modelo
forecast.arima2 <- forecast(arima2, h=length(observado), level=20)$mean

#Comparação do observado com o estimado
previsao <- tibble(forecast.arima2,observado)
data2 <- c("Março/2020","Setembro/2021", "Outubro/2021", "Novembro/2021", 
           "Dezembro/2021", "Janeiro/2022")
previsao <- tibble(data2, previsao)

kable(previsao, align='ccc', col.names = c("Data Previsão", 
                                           "Previsão com o Modelo Estimado", 
                                           "Valores Observados")) %>% 
  kable_styling(full_width=TRUE, position = "center")








































#Estimar a regressão com intercepto (sem, inserir -1)
regressao1 <- lm(obitos ~ dia, data=dados)
plot(dia, obitos)
abline(regressao1, col="red")

#Resultados da Regressão
summary(regressao1)
confint(regressao1) #Intervalo de confiança dos betas estimados
anova(regressao1)

#Intervalo de confiança dos valores previstos
IP <- predict(regressao1, interval="predict")

plot(dia, obitos)
abline(regressao1, col="red")
matlines(obitos, IP[ , c("lwr","upr")], col = "blue")

#Salvando os resíduos do modelo e os valores estimados
residuos1 <- regressao1$residuals
ajustados1 <- regressao1$fitted.values
plot(ajustados1, residuos1)

#Geração de variavel quadratica dos residuos
residuos_quad1 = (residuos1)^2
plot(residuos1, residuos_quad1)

#Outras analises de residuos
qqnorm(residuos1)
qqline(residuos1)
hist(residuos1)

# Teste de Jarque-Bera e Shapiro
# Lembre-se da hipotese nula (H_0): normalidade da serie
jarque.bera.test(residuos1)
shapiro.test(residuos1)

#Setando como serie temporal

dados <- ts(dados, start=c(2020,04), end=c(2022,10), frequency = 365)
ggtsdisplay(dados, main='dados')

plot(obitos, main='Obitos em Pernambuco',
    xlab='Dias', ylab='Quantidade', lwd=1)

#Calculo da taxa de crescimento e da tendencia
lobitos <- log(obitos)
regress1 <- lm(lobitos ~ seq(along = obitos)) #taxa de Crescimento

trend <- 1:length(obitos)
regress2 <- lm(lobitos ~ trend)
regress3 <- lm(lobitos ~ trend + I(trend^2)) #tendencia quadratica

#Série sem Tendencia deterministica
obitos.std <- ts(residuals(regress2)+mean(fitted(regress2)),
                 start=c(2020,04), end=c(2022,10), frequency = 365)
autoplot(obitos.std)

par(mfrow=c(1,2))
plot(obitos)
plot(obitos.std)

q(save='yes')


# Rotular
colnames(dados) <- c(1:50)

dbgg <- divida[,c(3)] #divida bruta do governo geral

date <- seq(as.Date('2006-12-01'),to=as.Date("2022-01-01"),by='1 month')
dbgg2 <- tibble(date,dbgg)
