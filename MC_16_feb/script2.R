# ----------------------------------------

#       Script_2 desenvolvido por Ícaro Agostino
#           Email: icaroagostino@gmail.com
# Disponivél em https://github.com/icaroagostino/BD

# ----------------------------------------

# Sabado 16 de fevereiro

# chamando bibliotecas

library(tseries) #manipular ST
library(forecast) #previsao
library(ggplot2) #graficos
library(lmtest) #teste

# Series Sazonais - UTI

bd3 <- read.table("uti.txt", header = T)

attach(bd3)

uti <- ts(uti, start = 1992, frequency = 12)

# Dividindo a st em 2 partes

uti_in <- window(uti, end = c(2017,12)) # treino
uti_out <- window(uti, start = c(2018,01)) # teste

# ----------------------------------------

autoplot(uti_in) # serie treino
ggtsdisplay(uti_in) # serie + ACF + pACF

mod1_uti <- auto.arima(uti_in) %>% print

autoplot(uti) # serie original

prev1 <- forecast(mod1_uti, h = 11) # previsao

autoplot(prev1) + autolayer(uti_out) # comparacao

checkresiduals(mod1_uti) # checando o modelo

# ----------------------------------------

# TSstudio para visualização

library(TSstudio)

test_forecast(uti, prev1, uti_in, uti_out)

ts_plot(uti)
ts_seasonal(uti)
ts_heatmap(uti)
ts_polar(uti)
ts_surface(uti)
ts_lags(uti)
ts_acf(uti)
ts_pacf(uti)
ts_decompose(uti)
check_res(fit)
test_forecast(uti, prev1, uti_in, uti_out)

# ----------------------------------------

# pratica desafio

# ----------------------------------------

# Um pouco sobre redes neurais (ANN)

mod2_uti <- nnetar(uti_in)

prev2 <- forecast(mod2_uti, PI = T, h = 11) # previsao

autoplot(prev2) + autolayer(uti_out) # comparacao

checkresiduals(mod2_uti) # checando o modelo

test_forecast(uti, prev2, uti_in, uti_out)

# ARIMA x (ANN)

accuracy(prev1, uti_out)
accuracy(prev2, uti_out)