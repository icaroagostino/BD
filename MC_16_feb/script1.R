# ----------------------------------------

#       Script_1 desenvolvido por Ícaro Agostino
#           Email: icaroagostino@gmail.com
# Disponivél em https://github.com/icaroagostino/BD

# ----------------------------------------

# Sabado 16 de fevereiro

# chamando bibliotecas

library(tseries) #manipular ST
library(forecast) #previsao
library(ggplot2) #graficos
library(lmtest) #teste

# algumas funções uteis

rm(list=ls()) # limpando memória
args(Arima) # exibe os argumentos da função
help(Arima) # ou f1 em cima da função

# atalhos uteis

# definindo pasta de trabalho

# importando dados

# dados do excel

# dados em txt

# dados <- read.table("uti.txt", h = T) # lendo dados

# dados da internet

dados <- read.table("https://raw.githubusercontent.com/icaroagostino/BD/master/MC_16_feb/uti.txt", h = T)

attach(dados) # transf. em objeto

uti <- ts(uti, start = 1992, frequency = 12) # transf. em st

# Graficos uteis

autoplot(uti) # serie original
ggtsdisplay(uti) # serie + ACF + pACF
ggtsdisplay(uti, main="Utilização da capacidade instalada na indústria")
ggtsdisplay(uti, plot.type = "histogram")
ggtsdisplay(uti, plot.type = "scatter")

# Decomposicao

decompose(uti)
autoplot(decompose(uti))
uti %>% decompose %>% autoplot

# pratica 1 ----------------------------------------

# ARIMA (Ciclo de BJ)

# Modelando Inflação Ipca

bd1 <- read.table("ipca.txt", header = T)

attach(bd1)

ip <- ts(ipca, start = 2012, frequency = 12)

# Analise grafica

autoplot(ip)
ggtsdisplay(ip)

# testar estacionariedade

kpss.test(ip)
kpss.test(diff(ip)) # d = 1

# ajustar modelos concorrentes

mod1_ip <- Arima(ip, order = c(1,0,0)) %>% print
mod2_ip <- Arima(ip, order = c(2,0,0)) %>% print
mod3_ip <- Arima(ip, order = c(0,0,1)) %>% print

# Checar residuos

checkresiduals(mod1_ip)
checkresiduals(mod2_ip)
checkresiduals(mod3_ip)

# Decidir

autoplot(ip) + autolayer(mod1_ip$fitted)

# Realizar previsoes

autoplot(forecast(mod1_ip, h = 6))

mod1_ip %>% forecast(h = 6) %>% autoplot()

# pratica 2 ----------------------------------------

# Modelando Inflação Igpm

bd2 <- read.table("igpm.txt", header = T)

attach(bd2)

ig <- ts(igpm, start = 2012, frequency = 12)

# Analise grafica

autoplot(ig)
ggtsdisplay(ig)

# testar estacionariedade

kpss.test(ig)
kpss.test(diff(ig)) # d = 1

# ajustar modelos concorrentes

mod1_ig <- Arima(ig, order = c(1,0,0))
mod2_ig <- Arima(ig, order = c(1,0,1))
mod3_ig <- Arima(ig, order = c(0,0,1))

# Checar residuos

checkresiduals(mod1_ig)
checkresiduals(mod2_ig)
checkresiduals(mod3_ig)

# Decidir

autoplot(ig) + autolayer(mod1_ig$fitted)

# Realizar previsoes

autoplot(forecast(mod1_ig, h = 6))

mod1_ig %>% forecast(h = 6) %>% autoplot()

# ----------------------------------------

# Compilar ctrl+shit+k

# Citacao

citation() # citação do R
citation('forecast') # citação dos pacotes
citation('tseries')