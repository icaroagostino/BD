# Bonus

library(fpp2)

# lidando com valores faltantes (NA)

# The gold data contains daily morning gold prices
# from 1 January 1985 to 31 March 1989. This series
# was provided to us as part of a consulting project;
# it contains 34 missing values as well as one
# apparently incorrect value.

autoplot(gold)

# preenche os NA por interpolação

gold2 <- na.interp(gold)

# antes e depois

autoplot(gold2, series="Interpolated") +
  autolayer(gold, series="Original") +
  scale_colour_manual(
    values=c(`Interpolated`="red",`Original`="gray"))

# lidando com outliers

# The tsoutliers() function is designed to identify
# outliers, and to suggest potential replacement
# values. In the gold data shown in Figure 12.9,
# there is an apparently outlier on day 770

tsoutliers(gold) # encontrando

gold[768:772] # olhando valores vizinhos

gold %>% autoplot() # série original

# Another useful function is tsclean() which
# identifies and replaces outliers, and also
# replaces missing values.

gold2 %>% tsclean() %>% autoplot()

gold2 %>%
  tsclean() %>%
  auto.arima() %>%
  forecast(h=50) %>%
  autoplot()

# exemplo retirado de:
# Forecasting: Principles and Practice (2018)

# link: https://otexts.com/fpp2/missing-outliers.html