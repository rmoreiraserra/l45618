
# LIBRARIES & PATHS --------------------------------------------------------------------------------------------


install.packages("fpp3")                                      
install.packages("summarytools")                              
install.packages("devtools")
devtools::install_github("jessevent/crypto", force = TRUE)    
install.packages("tsoutliers")

library(fpp3)
library(summarytools)
library(crypto)
library(tsoutliers)

# DATA COLLECTION ----------------------------------------------------------------------------------------------

crypto <- crypto_global_market() %>%
  transmute(date = as.Date(timestamp), volume, market = market_cap)


# BITCOIN ------------------------------------------------------------------------------------------------------

BTC_ <- crypto_history(coin = "BTC", start_date = "20130429", end_date = "20200829")[,c(-1:-3,-5:-8)]

## Daily Data --------------------------------------------------------------------------------------------------

BTC.daily <- BTC %>% 
              as_tsibble(index = date) %>% 
                rename(quote = close) %>%
                  mutate(log_quote = log(quote),
                         log_return = difference(log_quote))


plot(x = BTC.daily$date, y = BTC.daily$quote, main = "BTC Quote (USD)", xlab = "Year", ylab = NA, type = "l")   # price plot
plot(x = BTC.daily$date, y = BTC.daily$log_quote, main = "BTC Log Quote", xlab = "Year", ylab = NA, type = "l") # log price plot


# market dominance

BTC.daily <- left_join(BTC.daily, crypto, by = "date") %>%
              rename(market = market.x,
                     volume = volume.x,
                     market.overall = market.y,
                     volume.overall = volume.y) %>%
                mutate(market.dominance = ifelse(market/market.overall > 1, NA, market/market.overall * 100))


par(mar = c(5, 4, 4, 4) + 0.3)                                                                                         
plot(x = BTC.daily$date, y = BTC.daily$market.dominance, main = "BTC Market Dominance vs Quote", xlab = NA, ylab = "BTC Market Dominance (%)", type ="l")
par(new = TRUE)                                                                                                          
plot(x = BTC.daily$date, y = BTC.daily$market.overall/1000000000, axes = F, xlab = NA, ylab = NA, type = "l", col = 3)
axis(side = 4, at = pretty(range(BTC.daily$market.overall/1000000000)))                                                
mtext("Overall Market Cap (G USD)", side = 4, line = 3)         




## Weekly Data -------------------------------------------------------------------------------------------------

BTC.weekly <- BTC %>%
                rename(quote = close) %>%
                  mutate(week = as.numeric(date-date[1]) %/% 7,
                         last = !duplicated(week,fromLast=TRUE)) 

BTC.weekly.last <- BTC.weekly %>% filter(last == TRUE) %>% distinct(date, week)

BTC.weekly <- BTC.weekly %>%
                select(quote, week) %>%
                  group_by(week) %>%
                    summarise(quote = mean(quote)) %>%
                      ungroup()
BTC.weekly <- left_join(BTC.weekly, BTC.weekly.last, by = "week") %>% select(-week) %>% select(2,1)

BTC.weekly <- BTC.weekly %>% as_tsibble(index = date)

BTC.weekly <- BTC.weekly %>%
                mutate(log_quote = log(quote),
                       log_return = difference(log_quote))


# price plot
plot(x = BTC.weekly$date, y = BTC.weekly$quote, main = "BTC Quote (USD)", xlab = "Year", ylab = NA, type = "l")
# log price plot
plot(x = BTC.weekly$date, y = BTC.weekly$log_quote, main = "BTC Log Quote", xlab = "Year", ylab = NA, type = "l")
# log return plot (first difference of log price)
plot(x = BTC.weekly$date, y = BTC.weekly$log_return, main = "BTC Log Return", xlab = "Year", ylab = NA, type = "l")



### Descriptive Statistics ------------------------------------------------------------------------------------- 


summary <- BTC.weekly %>% descr(log_return)
BTC.summary <- data.frame(BTC = summary[c(1:3,7,11,13),])

hist(BTC.weekly$log_return, breaks = 50, main = "Histogram - BTC Log Return", xlim = c(-0.4, 0.6), xlab = NA, ylab = NA)



# ---------------------------------------------- Stationarity ------------------------------------------------ #


BTC.KPSS.LQ <- BTC.weekly %>% features(log_quote, unitroot_kpss) # KPSS unit root test 
BTC.KPSS.LR <- BTC.weekly %>% features(log_return, unitroot_kpss) # KPSS unit root test

BTC.URoots <- BTC.weekly %>% features(log_quote, unitroot_ndiffs) # testing for number of unit roots
BTC.SDiff <- BTC.weekly %>% features(log_quote, unitroot_nsdiffs) # testing for seasonal differences required


BTC.stationarity <- bind_cols(CRYPTO = "BTC",BTC.KPSS.LQ, BTC.URoots, BTC.SDiff)



# --------------------------------------------- Autocorrelation ---------------------------------------------- #


# Ljung-Box - test for autocorrelation in y
BTC.weekly %>% features(log_return, box_pierce, lag = 10)
BTC.weekly %>% features(log_return, ljung_box, lag = 10)


# ACF and PACF
BTC.weekly %>% ACF(log_quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - BTC Log Quote")
BTC.weekly %>% PACF(log_quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - BTC Log Quote")


BTC.weekly %>% ACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - BTC Log Return")
BTC.weekly %>% PACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - BTC Log Return")



# ------------------------------------------------ Normality ------------------------------------------------- #

JarqueBera.test(BTC.weekly$log_return)



### Exponential Smoothing --------------------------------------------------------------------------------------


BTC.ETS.accuracy <- BTC.weekly %>%
  stretch_tsibble(.init = 10) %>%
  model("SES - ETS(A,N,N)" = ETS(log_quote ~ error("A") + trend("N") + season("N")),
        "Holt - ETS(A,A,N)" = ETS(log_quote ~ error("A") + trend("A") + season("N")),
        "Damped - ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 1) %>%
  accuracy(BTC.weekly) %>%
  rename(BTC = .model) %>% select(c(1:2,4:5,8))

BTC.fit.ETS <- BTC.weekly %>% model(ETS(log_return)) # select model with less AICc
report(BTC.fit.ETS) 

# SES - simple exp smooth

BTC.fit.ses <- BTC.weekly %>% model("SES - ETS(A,N,N)" = ETS(log_quote ~ error("A") + trend("N") + season("N")))
report(BTC.fit.ses)
BTC.fit.ses.report <- tidy(BTC.fit.ses)
BTC.fit.ses.report <- reshape2::dcast(BTC.fit.ses.report, .model ~ term)
BTC.fit.ses.report <- cbind(BTC.fit.ses.report,BTC.fit.ses[[1]][[1]][["fit"]][["fit"]])[,-c(9:11)]

BTC.fit.ses.aug <- BTC.fit.ses %>% augment()

# - Box-Pierce / Ljung-Box
BTC.fit.ses.BoxP <- BTC.fit.ses.aug %>% features(.resid, box_pierce, lag = 10, dof = 1)
BTC.fit.ses.LBox <- BTC.fit.ses.aug %>% features(.resid, ljung_box, lag = 10, dof = 1)


# TES - trend exp smooth - Holt

BTC.fit.trend <- BTC.weekly %>% model("Holt - ETS(A,A,N)" = ETS(log_quote ~ error("A") + trend("A") + season("N")))
BTC.fit.trend.report <- tidy(BTC.fit.trend)
BTC.fit.trend.report <- reshape2::dcast(BTC.fit.trend.report, .model ~ term)
BTC.fit.trend.report <- cbind(BTC.fit.trend.report,BTC.fit.trend[[1]][[1]][["fit"]][["fit"]])[,-c(11:13)]

BTC.fit.trend.aug <- BTC.fit.trend %>% augment()

# - Box-Pierce / Ljung-Box
BTC.fit.trend.BoxP <- BTC.fit.trend.aug %>% features(.resid, box_pierce, lag = 10, dof = 2)
BTC.fit.trend.LBox <- BTC.fit.trend.aug %>% features(.resid, ljung_box, lag = 10, dof = 2)


# Damped

BTC.fit.damped <- BTC.weekly %>% model("Damped - ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N")))
BTC.fit.damped.report <- tidy(BTC.fit.damped)
BTC.fit.damped.report <- reshape2::dcast(BTC.fit.damped.report, .model ~ term)
BTC.fit.damped.report <- cbind(BTC.fit.damped.report,BTC.fit.damped[[1]][[1]][["fit"]][["fit"]])[,-c(12:14)]

BTC.fit.damped.aug <- BTC.fit.damped %>% augment()

# Box-Pierce / Ljung-Box
BTC.fit.damped.BoxP <- BTC.fit.damped.aug %>% features(.resid, box_pierce, lag = 10, dof = 3)
BTC.fit.damped.LBox <- BTC.fit.damped.aug %>% features(.resid, ljung_box, lag = 10, dof = 3)

BTC.fit.ETS.report <- bind_rows(BTC.fit.damped.report, BTC.fit.trend.report, BTC.fit.ses.report)[c(3:1),c(1,2,5:3,6:11)] 

BTC.fit.ETS.BoxP <- bind_rows(BTC.fit.ses.BoxP, BTC.fit.trend.BoxP, BTC.fit.damped.BoxP)
BTC.fit.ETS.LBox <- bind_rows(BTC.fit.ses.LBox, BTC.fit.trend.LBox, BTC.fit.damped.LBox)
BTC.fit.ETS.Autoc <- left_join(BTC.fit.ETS.BoxP[,-2], BTC.fit.ETS.LBox[,-2], by = ".model")

BTC.fit.ETS.report <- left_join(BTC.fit.ETS.report, BTC.fit.ETS.Autoc, by = ".model") %>% rename(Bitcoin = .model)


components(BTC.fit.ETS) %>% autoplot() + ggtitle("ETS(A,Ad,N) components") + xlab("Year")


# ------------------------------------------------ Residuals ------------------------------------------------ #

BTC.fit.ETS.aug <- BTC.fit.ETS %>% augment()

BTC.fit.ETS.aug %>% autoplot(.resid) + xlab("Year") + ylab(NULL) + ggtitle("Residuals from ETS(A,Ad,N)")
BTC.fit.ETS.aug %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 50) + ggtitle("Histogram of residuals") + xlab(NULL) + ylab(NULL)
BTC.fit.ETS.aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals") + xlab("Lag") + ylab(NULL)
BTC.fit.ETS.aug %>% PACF(.resid) %>% autoplot() + ggtitle("PACF of residuals") + xlab("Lag") + ylab(NULL)




### ARIMA ------------------------------------------------------------------------------------------------------

# model selection

BTC.fit.arima1 <- BTC.weekly %>% model("ARIMA(1,1,0) w/drift" = ARIMA(log_quote ~ pdq(c(0:5),c(0:2),c(0:5)) + PDQ(0,0,0),approximation = FALSE, stepwise = FALSE))
report(BTC.fit.arima1)
arima1 <- tidy(BTC.fit.arima1) %>% 
                          mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
                            select(-c(std.error, statistic, p.value))

BTC.fit.arima2 <- BTC.weekly %>% model("ARIMA(3,1,2) w/drift" = ARIMA(log_quote ~ pdq(c(0,2:5),c(0:2),c(0:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(BTC.fit.arima2)
arima2 <- tidy(BTC.fit.arima2) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

BTC.fit.arima3 <- BTC.weekly %>% model("ARIMA(2,1,0) w/drift" = ARIMA(log_quote ~ pdq(c(0,2,4,5),c(0:2),c(0:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(BTC.fit.arima3)
arima3 <- tidy(BTC.fit.arima3) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

arima1 <- reshape2::dcast(arima1, .model ~ term) %>% mutate(AICc = BTC.fit.arima1[[1]][[1]][["fit"]][["fit"]][["AICc"]])
arima2 <- reshape2::dcast(arima2, .model ~ term) %>% mutate(AICc = BTC.fit.arima2[[1]][[1]][["fit"]][["fit"]][["AICc"]])
arima3 <- reshape2::dcast(arima3, .model ~ term) %>% mutate(AICc = BTC.fit.arima3[[1]][[1]][["fit"]][["fit"]][["AICc"]])

BTC.arima <- bind_rows(arima2, arima3, arima1) %>% select(1,5,2:8)


# ------------------------------------------------ Residuals ------------------------------------------------ #

BTC.fit.arima1.aug <- BTC.fit.arima1 %>% augment()

BTC.fit.arima1.aug %>% autoplot(.resid) + xlab("Year") + ylab(NULL) + ggtitle("Residuals from ARIMA (1,1,0) w/ drift")
BTC.fit.arima1.aug %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 50) + ggtitle("Histogram of residuals") + xlab(NULL) + ylab(NULL)
BTC.fit.arima1.aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals") + xlab("Lag") + ylab(NULL)

# Box-Pierce / Ljung-Box
BTC.fit.arima1.BoxP <- BTC.fit.arima1 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 2)
BTC.fit.arima1.LBox <- BTC.fit.arima1 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 2)
BTC.fit.arima2.BoxP <- BTC.fit.arima2 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 6)
BTC.fit.arima2.LBox <- BTC.fit.arima2 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 6)
BTC.fit.arima3.BoxP <- BTC.fit.arima3 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 3)
BTC.fit.arima3.LBox <- BTC.fit.arima3 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 3)

BTC.fit.arima.BoxP <- bind_rows(BTC.fit.arima1.BoxP, BTC.fit.arima2.BoxP, BTC.fit.arima3.BoxP)
BTC.fit.arima.LBox <- bind_rows(BTC.fit.arima1.LBox, BTC.fit.arima2.LBox, BTC.fit.arima3.LBox)
BTC.fit.arima.Autoc <- left_join(BTC.fit.arima.BoxP[,-2], BTC.fit.arima.LBox[,-2], by = ".model")

BTC.fit.arima.report <- left_join(BTC.arima, BTC.fit.arima.Autoc, by = ".model") %>% rename(Bitcoin = .model)



# ETHEREUM -----------------------------------------------------------------------------------------------------

ETH <- crypto_history(coin = "ETH", start_date = "20150810", end_date = "20200829")[,c(-1:-3,-5:-8)]

## Daily Data --------------------------------------------------------------------------------------------------

ETH.daily <- ETH %>% 
  as_tsibble(index = date) %>% 
  rename(quote = close) %>%
  mutate(log_quote = log(quote),
         log_return = difference(log_quote))


plot(x = ETH.daily$date, y = ETH.daily$quote, main = "ETH Quote (USD)", xlab = "Year", ylab = NA, type = "l") # price plot
plot(x = ETH.daily$date, y = ETH.daily$log_quote, main = "ETH Log Quote", xlab = "Year", ylab = NA, type = "l") # price plot
ETH.daily %>% ACF(quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - ETH Quote") # price ACF plot


hist(x = ETH.daily$log_return, breaks = 100, main = "Histogram - ETH Log Return" , xlim = c(-0.3, 0.3), 
     xlab = NA, ylab = NA) # histogram log return


ETH.daily <- left_join(ETH.daily, crypto, by = "date") %>%
  rename(market = market.x,
         volume = volume.x,
         market.overall = market.y,
         volume.overall = volume.y) %>%
  mutate(market.dominance = ifelse(market/market.overall > 1, NA, market/market.overall * 100))


# market dominance
par(mar = c(5, 4, 4, 4) + 0.3)                                                                                           # Additional space for second y-axis
plot(x = ETH.daily$date, y = ETH.daily$market.dominance, main = "ETH Market Dominance vs Quote", xlab = NA, ylab = "ETH Market Dominance (%)", type ="l")        # Create first plot
par(new = TRUE)                                                                                                          # Add new plot
plot(x = ETH.daily$date, y = ETH.daily$market.overall/1000000000, axes = F, xlab = NA, ylab = NA, type = "l", col = 3)
axis(side = 4, at = pretty(range(ETH.daily$market.overall/1000000000)))                                                  # Add second axis
mtext("Overall Market Cap (G USD)", side = 4, line = 3)         


# log price plot
plot(x = ETH.daily$date, y = ETH.daily$log_quote, main = "ETH Log Quote", xlab = NA, ylab = NA, type = "l")


## Weekly Data -------------------------------------------------------------------------------------------------

ETH.weekly <- ETH %>%
  rename(quote = close) %>%
  mutate(week = as.numeric(date-date[1]) %/% 7,
         last = !duplicated(week,fromLast=TRUE)) 

ETH.weekly.last <- ETH.weekly %>% filter(last == TRUE) %>% distinct(date, week)

ETH.weekly <- ETH.weekly %>%
  select(quote, week) %>%
  group_by(week) %>%
  summarise(quote = mean(quote)) %>%
  ungroup()
ETH.weekly <- left_join(ETH.weekly, ETH.weekly.last, by = "week") %>% select(-week) %>% select(2,1)

ETH.weekly <- ETH.weekly %>% as_tsibble(index = date)

ETH.weekly <- ETH.weekly %>%
  mutate(log_quote = log(quote),
         log_return = difference(log_quote))


# price plot
plot(x = ETH.weekly$date, y = ETH.weekly$quote, main = "ETH Quote (USD)", xlab = "Year", ylab = NA, type = "l")
# log price plot
plot(x = ETH.weekly$date, y = ETH.weekly$log_quote, main = "ETH Log Quote", xlab = "Year", ylab = NA, type = "l")
# log return plot (first difference of log price)
plot(x = ETH.weekly$date, y = ETH.weekly$log_return, main = "ETH Log Return", xlab = "Year", ylab = NA, type = "l")



### Descriptive Statistics ------------------------------------------------------------------------------------- 


summary <- ETH.weekly %>% descr(log_return)
ETH.summary <- data.frame(ETH = summary[c(1:3,7,11,13),])

hist(ETH.weekly$log_return, breaks = 40 ,main = "Histogram - ETH Log Return", xlim = c(-0.4, 0.7), xlab = NA, ylab = NA)

# ---------------------------------------------- Stationarity ------------------------------------------------ #



ETH.KPSS.LQ <- ETH.weekly %>% features(log_quote, unitroot_kpss) # KPSS unit root test 
ETH.KPSS.LR <- ETH.weekly %>% features(log_return, unitroot_kpss) # KPSS unit root test

ETH.URoots <- ETH.weekly %>% features(log_quote, unitroot_ndiffs) # testing for number of unit roots
ETH.SDiff <- ETH.weekly %>% features(log_quote, unitroot_nsdiffs) # testing for seasonal differences required


ETH.stationarity <- bind_cols(CRYPTO = "ETH",ETH.KPSS.LQ, ETH.URoots, ETH.SDiff)

# --------------------------------------------- Autocorrelation ---------------------------------------------- #

# Ljung-Box - test for autocorrelation in y
ETH.weekly %>% features(log_return, box_pierce, lag = 10)
ETH.weekly %>% features(log_return, ljung_box, lag = 10)


ETH.weekly %>% features(log_quote, feat_acf)
ETH.weekly[-1,] %>% features(log_return, mean)

ETH.weekly %>% ACF(log_quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - ETH Log Quote")
ETH.weekly %>% PACF(log_quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - ETH Log Quote")


ETH.weekly %>% ACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - ETH Log Return")
ETH.weekly %>% PACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - ETH Log Return")



### Exponential Smoothing --------------------------------------------------------------------------------------

ETH.ETS.accuracy <- ETH.weekly %>%
  stretch_tsibble(.init = 10) %>%
  model("SES - ETS(A,N,N)" = ETS(log_quote ~ error("A") + trend("N") + season("N")),
        "Holt - ETS(A,A,N)" = ETS(log_quote ~ error("A") + trend("A") + season("N")),
        "Damped - ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 1) %>%
  accuracy(ETH.weekly) %>%
  rename(ETH = .model) %>% select(c(1:2,4:5,8))


ETH.fit.ETS <- ETH.weekly %>% model(ETS(log_quote)) # select model with less AICc
report(ETH.fit.ETS) 

# SES - simple exp smooth

ETH.fit.ses <- ETH.weekly %>% model("SES - ETS(A,N,N)" = ETS(log_quote ~ error("A") + trend("N") + season("N"), opt_crit = "mse"))
report(ETH.fit.ses)
ETH.fit.ses.report <- tidy(ETH.fit.ses)
ETH.fit.ses.report <- reshape2::dcast(ETH.fit.ses.report, .model ~ term)
ETH.fit.ses.report <- cbind(ETH.fit.ses.report,ETH.fit.ses[[1]][[1]][["fit"]][["fit"]])[,-c(9:11)]

ETH.fit.ses.aug <- ETH.fit.ses %>% augment()
ETH.fit.ses.aug %>% ACF(.resid) %>% autoplot() + ggtitle("ETH - ETS(A,N,N) - ACF of residuals") + xlab("Lag") + ylab(NULL)

# Box-Pierce / Ljung-Box
ETH.fit.ses.BoxP <- ETH.fit.ses.aug %>% features(.resid, box_pierce, lag = 10, dof = 1)
ETH.fit.ses.LBox <- ETH.fit.ses.aug %>% features(.resid, ljung_box, lag = 10, dof = 1)


# TES - trend exp smooth - Holt

ETH.fit.trend <- ETH.weekly %>% model("Holt - ETS(A,A,N)" = ETS(log_quote ~ error("A") + trend("A") + season("N")))
ETH.fit.trend.report <- tidy(ETH.fit.trend)
ETH.fit.trend.report <- reshape2::dcast(ETH.fit.trend.report, .model ~ term)
ETH.fit.trend.report <- cbind(ETH.fit.trend.report,ETH.fit.trend[[1]][[1]][["fit"]][["fit"]])[,-c(11:13)]

ETH.fit.trend.aug <- ETH.fit.trend %>% augment()

# Box-Pierce / Ljung-Box
ETH.fit.trend.BoxP <- ETH.fit.trend.aug %>% features(.resid, box_pierce, lag = 10, dof = 2)
ETH.fit.trend.LBox <- ETH.fit.trend.aug %>% features(.resid, ljung_box, lag = 10, dof = 2)


# Damped

ETH.fit.damped <- ETH.weekly %>% model("Damped - ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N")))
ETH.fit.damped.report <- tidy(ETH.fit.damped)
ETH.fit.damped.report <- reshape2::dcast(ETH.fit.damped.report, .model ~ term)
ETH.fit.damped.report <- cbind(ETH.fit.damped.report,ETH.fit.damped[[1]][[1]][["fit"]][["fit"]])[,-c(12:14)]

ETH.fit.damped.aug <- ETH.fit.damped %>% augment()

# Box-Pierce / Ljung-Box
ETH.fit.damped.BoxP <- ETH.fit.damped.aug %>% features(.resid, box_pierce, lag = 10, dof = 3)
ETH.fit.damped.LBox <- ETH.fit.damped.aug %>% features(.resid, ljung_box, lag = 10, dof = 3)

ETH.fit.ETS.report <- bind_rows(ETH.fit.damped.report, ETH.fit.trend.report, ETH.fit.ses.report)[c(3:1),c(1,2,5:3,6:11)] 

ETH.fit.ETS.BoxP <- bind_rows(ETH.fit.ses.BoxP, ETH.fit.trend.BoxP, ETH.fit.damped.BoxP)
ETH.fit.ETS.LBox <- bind_rows(ETH.fit.ses.LBox, ETH.fit.trend.LBox, ETH.fit.damped.LBox)
ETH.fit.ETS.Autoc <- left_join(ETH.fit.ETS.BoxP[,-2], ETH.fit.ETS.LBox[,-2], by = ".model")

ETH.fit.ETS.report <- left_join(ETH.fit.ETS.report, ETH.fit.ETS.Autoc, by = ".model") %>% rename(Ether = .model)


# ------------------------------------------------ Residuals ------------------------------------------------ #

ETH.fit.ETS.aug <- ETH.fit.ETS %>% augment()

ETH.fit.ETS.aug %>% autoplot(.resid) + xlab("Year") + ylab(NULL) + ggtitle("Residuals from ETS(A,Ad,N)")
ETH.fit.ETS.aug %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 50) + ggtitle("Histogram of residuals") + xlab(NULL) + ylab(NULL)
ETH.fit.ETS.aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals") + xlab("Lag") + ylab(NULL)

# Box-Pierce
ETH.fit.ETS.aug %>% features(.resid, box_pierce, lag = 10, dof = 3)
# Ljung-Box
ETH.fit.ETS.aug %>% features(.resid, ljung_box, lag = 10, dof = 3)


ETH.fit.ETS %>% forecast(h = 10) %>% autoplot(ETH.weekly) + xlab("Year") + ylab("Log Quote") +  ggtitle("Damped Exponential Smoothing - ETH Log Quote")

components(ETH.fit.ETS) %>% autoplot() + ggtitle("ETS(A,Ad,N) components") + xlab("Year")


### ARIMA ------------------------------------------------------------------------------------------------------


# model selection
ETH.weekly %>% ACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - ETH Log Return")
ETH.weekly %>% PACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - ETH Log Return")

ETH.fit.arima1 <- ETH.weekly %>% model("ARIMA(1,1,4)" = ARIMA(log_quote ~ PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(ETH.fit.arima1)
arima1 <- tidy(ETH.fit.arima1) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

ETH.fit.arima2 <- ETH.weekly %>% model("ARIMA(0,1,3)" = ARIMA(log_quote ~ pdq(c(0:5),0:2,c(0:3,5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(ETH.fit.arima2)
arima2 <- tidy(ETH.fit.arima2) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

ETH.fit.arima3 <- ETH.weekly %>% model("ARIMA(1,1,5)" = ARIMA(log_quote ~ pdq(c(0:5),0:2,c(0:2,5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(ETH.fit.arima3)
arima3 <- tidy(ETH.fit.arima3) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

arima1 <- reshape2::dcast(arima1, .model ~ term) %>% mutate(AICc = ETH.fit.arima1[[1]][[1]][["fit"]][["fit"]][["AICc"]])
arima2 <- reshape2::dcast(arima2, .model ~ term) %>% mutate(AICc = ETH.fit.arima2[[1]][[1]][["fit"]][["fit"]][["AICc"]])
arima3 <- reshape2::dcast(arima3, .model ~ term) %>% mutate(AICc = ETH.fit.arima3[[1]][[1]][["fit"]][["fit"]][["AICc"]])

ETH.arima <- bind_rows(arima3, arima1, arima2)


# ------------------------------------------------ Residuals ------------------------------------------------ #

ETH.fit.arima1.aug <- ETH.fit.arima1 %>% augment()

ETH.fit.arima1.aug %>% autoplot(.resid) + xlab("Year") + ylab(NULL) + ggtitle("Residuals from ARIMA (1,1,4)")
ETH.fit.arima1.aug %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 50) + ggtitle("Histogram of residuals") + xlab(NULL) + ylab(NULL)
ETH.fit.arima1.aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals") + xlab("Lag") + ylab(NULL)


# Box-Pierce / Ljung-Box
ETH.fit.arima1.BoxP <- ETH.fit.arima1 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 5)
ETH.fit.arima1.LBox <- ETH.fit.arima1 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 5)
ETH.fit.arima2.BoxP <- ETH.fit.arima2 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 3)
ETH.fit.arima2.LBox <- ETH.fit.arima2 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 3)
ETH.fit.arima3.BoxP <- ETH.fit.arima3 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 6)
ETH.fit.arima3.LBox <- ETH.fit.arima3 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 6)

ETH.fit.arima.BoxP <- bind_rows(ETH.fit.arima1.BoxP, ETH.fit.arima2.BoxP, ETH.fit.arima3.BoxP)
ETH.fit.arima.LBox <- bind_rows(ETH.fit.arima1.LBox, ETH.fit.arima2.LBox, ETH.fit.arima3.LBox)
ETH.fit.arima.Autoc <- left_join(ETH.fit.arima.BoxP[,-2], ETH.fit.arima.LBox[,-2], by = ".model")

ETH.fit.arima.report <- left_join(ETH.arima, ETH.fit.arima.Autoc, by = ".model") %>% rename(Ether = .model)


# ------------------------------------------------ Forecast ------------------------------------------------ #


ETH.arima.accuracy <- ETH.weekly %>%
  stretch_tsibble(.init = 10) %>%
  model("Arima1" = ARIMA(log_quote ~ 0 + pdq(1,1,4) + PDQ(0,0,0)),
        "Arima2" = ARIMA(log_quote ~ 0 + pdq(0,1,3) + PDQ(0,0,0)),
        "Arima3" = ARIMA(log_quote ~ 0 + pdq(1,1,5) + PDQ(0,0,0))) %>%
  forecast(h = 1) %>%
  accuracy(ETH.weekly) %>%
  rename(ETH = .model)


fit.arima %>% forecast(h = 10) %>% hilo()
fit.arima %>% forecast(h = 10) %>% autoplot(slice(ETH.weekly, (n()-80):n())) + xlab("Year") + ylab(NULL)
fit.arima %>% forecast(h = 10) %>% autoplot(ETH.weekly) + xlab("Year") + ylab(NULL)


# ----------------------------------------------- Random Walk ----------------------------------------------- #

ETH.weekly_tr <- ETH.weekly %>%
  slice(1:(n()-1)) %>%
  stretch_tsibble(.init = 3, .step = 1)
fc <- ETH.weekly_tr %>%
  model(RW(log_quote ~ drift())) %>%
  forecast(h = 1)

# Test Residual accuracy
fc %>% accuracy(ETH.weekly)
# Training Residual accuracy
ETH.weekly %>% model(RW(log_quote ~ drift())) %>% accuracy()


# LITECOIN -----------------------------------------------------------------------------------------------------

LTC <- crypto_history(coin = "LTC", start_date = "20130429", end_date = "20200829")[,c(-1:-3,-5:-8)]

## Daily Data --------------------------------------------------------------------------------------------------

LTC.daily <- LTC %>% 
  as_tsibble(index = date) %>% 
  rename(quote = close) %>%
  mutate(log_quote = log(quote),
         log_return = difference(log_quote))


plot(x = LTC.daily$date, y = LTC.daily$quote, main = "LTC Quote (USD)", xlab = "Year", ylab = NA, type = "l") # price plot
plot(x = LTC.daily$date, y = LTC.daily$quote, main = "LTC Log Quote", xlab = "Year", ylab = NA, type = "l") # price plot
LTC.daily %>% ACF(quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - LTC Quote") # price ACF plot


max(LTC.daily$quote)
min(LTC.daily$quote)

hist(x = LTC.daily$log_return, breaks = 100, main = "Histogram - LTC Log Return" , xlim = c(-0.3, 0.3), 
     xlab = NA, ylab = NA) # histogram log return


LTC.daily <- left_join(LTC.daily, crypto, by = "date") %>%
  rename(market = market.x,
         volume = volume.x,
         market.overall = market.y,
         volume.overall = volume.y) %>%
  mutate(market.dominance = ifelse(market/market.overall > 1, NA, market/market.overall * 100))


# market dominance
par(mar = c(5, 4, 4, 4) + 0.3)                                                                                           # Additional space for second y-axis
plot(x = LTC.daily$date, y = LTC.daily$market.dominance, main = "LTC Market Dominance vs Quote", xlab = NA, ylab = "LTC Market Dominance (%)", type ="l")        # Create first plot
par(new = TRUE)                                                                                                          # Add new plot
plot(x = LTC.daily$date, y = LTC.daily$market.overall/1000000000, axes = F, xlab = NA, ylab = NA, type = "l", col = 3)
axis(side = 4, at = pretty(range(LTC.daily$market.overall/1000000000)))                                                  # Add second axis
mtext("Overall Market Cap (G USD)", side = 4, line = 3)         


# log price plot
plot(x = LTC.daily$date, y = LTC.daily$log_quote, main = "LTC Log Quote", xlab = NA, ylab = NA, type = "l")


## Weekly Data -------------------------------------------------------------------------------------------------

LTC.weekly <- LTC %>%
  rename(quote = close) %>%
  mutate(week = as.numeric(date-date[1]) %/% 7,
         last = !duplicated(week,fromLast=TRUE)) 

LTC.weekly.last <- LTC.weekly %>% filter(last == TRUE) %>% distinct(date, week)

LTC.weekly <- LTC.weekly %>%
  select(quote, week) %>%
  group_by(week) %>%
  summarise(quote = mean(quote)) %>%
  ungroup()
LTC.weekly <- left_join(LTC.weekly, LTC.weekly.last, by = "week") %>% select(-week) %>% select(2,1)

LTC.weekly <- LTC.weekly %>% as_tsibble(index = date)

LTC.weekly <- LTC.weekly %>%
  mutate(log_quote = log(quote),
         log_return = difference(log_quote))


# price plot
plot(x = LTC.weekly$date, y = LTC.weekly$quote, main = "LTC Quote (USD)", xlab = "Year", ylab = NA, type = "l")
# log price plot
plot(x = LTC.weekly$date, y = LTC.weekly$log_quote, main = "LTC Log Quote", xlab = "Year", ylab = NA, type = "l")
# log return plot (first difference of log price)
plot(x = LTC.weekly$date, y = LTC.weekly$log_return, main = "LTC Log Return", xlab = "Year", ylab = NA, type = "l")



### Descriptive Statistics ------------------------------------------------------------------------------------- 


LTC.KPSS.LQ <- LTC.weekly %>% features(log_quote, unitroot_kpss) # KPSS unit root test 
LTC.KPSS.LR <- LTC.weekly %>% features(log_return, unitroot_kpss) # KPSS unit root test

LTC.URoots <- LTC.weekly %>% features(log_quote, unitroot_ndiffs) # testing for number of unit roots
LTC.SDiff <- LTC.weekly %>% features(log_quote, unitroot_nsdiffs) # testing for seasonal differences required


LTC.stationarity <- bind_cols(CRYPTO = "LTC",LTC.KPSS.LQ, LTC.URoots, LTC.SDiff)

# --------------------------------------------- Autocorrelation ---------------------------------------------- #

# Ljung-Box - test for autocorrelation in y
LTC.weekly %>% features(log_return, box_pierce, lag = 10)
LTC.weekly %>% features(log_return, ljung_box, lag = 10)


LTC.weekly %>% features(log_quote, feat_acf)
LTC.weekly[-1,] %>% features(log_return, mean)

LTC.weekly %>% ACF(log_quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - LTC Log Quote")
LTC.weekly %>% PACF(log_quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - LTC Log Quote")


LTC.weekly %>% ACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - LTC Log Return")
LTC.weekly %>% PACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - LTC Log Return")




### Exponential Smoothing --------------------------------------------------------------------------------------

LTC.ETS.accuracy <- LTC.weekly %>%
  stretch_tsibble(.init = 10) %>%
  model("SES - ETS(A,N,N)" = ETS(log_quote ~ error("A") + trend("N") + season("N")),
        "Holt - ETS(A,A,N)" = ETS(log_quote ~ error("A") + trend("A") + season("N")),
        "Damped - ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 1) %>%
  accuracy(LTC.weekly) %>%
  rename(LTC = .model) %>% select(c(1:2,4:5,8))


LTC.fit.ETS <- LTC.weekly %>% model(ETS(log_quote)) # select model with less AICc
report(LTC.fit.ETS) 

# SES - simple exp smooth

LTC.fit.ses <- LTC.weekly %>% model("SES - ETS(A,N,N)" = ETS(log_quote ~ error("A") + trend("N") + season("N"), opt_crit = "mse"))
report(LTC.fit.ses)
LTC.fit.ses.report <- tidy(LTC.fit.ses)
LTC.fit.ses.report <- reshape2::dcast(LTC.fit.ses.report, .model ~ term)
LTC.fit.ses.report <- cbind(LTC.fit.ses.report,LTC.fit.ses[[1]][[1]][["fit"]][["fit"]])[,-c(9:11)]

LTC.fit.ses.aug <- LTC.fit.ses %>% augment()
LTC.fit.ses.aug %>% ACF(.resid) %>% autoplot() + ggtitle("LTC - ETS(A,N,N) - ACF of residuals") + xlab("Lag") + ylab(NULL)


# Box-Pierce / Ljung-Box
LTC.fit.ses.BoxP <- LTC.fit.ses.aug %>% features(.resid, box_pierce, lag = 10, dof = 1)
LTC.fit.ses.LBox <- LTC.fit.ses.aug %>% features(.resid, ljung_box, lag = 10, dof = 1)


# TES - trend exp smooth - Holt

LTC.fit.trend <- LTC.weekly %>% model("Holt - ETS(A,A,N)" = ETS(log_quote ~ error("A") + trend("A") + season("N")))
LTC.fit.trend.report <- tidy(LTC.fit.trend)
LTC.fit.trend.report <- reshape2::dcast(LTC.fit.trend.report, .model ~ term)
LTC.fit.trend.report <- cbind(LTC.fit.trend.report,LTC.fit.trend[[1]][[1]][["fit"]][["fit"]])[,-c(11:13)]

LTC.fit.trend.aug <- LTC.fit.trend %>% augment()

# Box-Pierce / Ljung-Box
LTC.fit.trend.BoxP <- LTC.fit.trend.aug %>% features(.resid, box_pierce, lag = 10, dof = 2)
LTC.fit.trend.LBox <- LTC.fit.trend.aug %>% features(.resid, ljung_box, lag = 10, dof = 2)


# Damped

LTC.fit.damped <- LTC.weekly %>% model("Damped - ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N")))
LTC.fit.damped.report <- tidy(LTC.fit.damped)
LTC.fit.damped.report <- reshape2::dcast(LTC.fit.damped.report, .model ~ term)
LTC.fit.damped.report <- cbind(LTC.fit.damped.report,LTC.fit.damped[[1]][[1]][["fit"]][["fit"]])[,-c(12:14)]

LTC.fit.damped.aug <- LTC.fit.damped %>% augment()

# Box-Pierce / Ljung-Box
LTC.fit.damped.BoxP <- LTC.fit.damped.aug %>% features(.resid, box_pierce, lag = 10, dof = 3)
LTC.fit.damped.LBox <- LTC.fit.damped.aug %>% features(.resid, ljung_box, lag = 10, dof = 3)

LTC.fit.ETS.report <- bind_rows(LTC.fit.damped.report, LTC.fit.trend.report, LTC.fit.ses.report)[c(3:1),c(1,2,5:3,6:11)] 

LTC.fit.ETS.BoxP <- bind_rows(LTC.fit.ses.BoxP, LTC.fit.trend.BoxP, LTC.fit.damped.BoxP)
LTC.fit.ETS.LBox <- bind_rows(LTC.fit.ses.LBox, LTC.fit.trend.LBox, LTC.fit.damped.LBox)
LTC.fit.ETS.Autoc <- left_join(LTC.fit.ETS.BoxP[,-2], LTC.fit.ETS.LBox[,-2], by = ".model")

LTC.fit.ETS.report <- left_join(LTC.fit.ETS.report, LTC.fit.ETS.Autoc, by = ".model") %>% rename(Litecoin = .model)

# ------------------------------------------------ Residuals ------------------------------------------------ #

LTC.fit.ETS.aug <- LTC.fit.damped %>% augment()

LTC.fit.ETS.aug %>% autoplot(.resid) + xlab("Year") + ylab(NULL) + ggtitle("Residuals from ETS(A,Ad,N)")
LTC.fit.ETS.aug %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 50) + ggtitle("Histogram of residuals") + xlab(NULL) + ylab(NULL)
LTC.fit.ETS.aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals") + xlab("Lag") + ylab(NULL)

# Box-Pierce
LTC.fit.ETS.aug %>% features(.resid, box_pierce, lag = 10, dof = 0)
# Ljung-Box
LTC.fit.ETS.aug %>% features(.resid, ljung_box, lag = 10, dof = 0)


fit.damped %>% forecast(h = 10) %>% autoplot(LTC.weekly) + xlab("Year") + ylab("Log Quote") +  ggtitle("Damped Exponential Smoothing - LTC Log Quote")

components(LTC.fit.ETS) %>% autoplot() + ggtitle("ETS(A,Ad,N) components") + xlab("Year")


### ARIMA ------------------------------------------------------------------------------------------------------


# model selection
LTC.weekly %>% ACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - LTC Log Return")
LTC.weekly %>% PACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - LTC Log Return")

LTC.fit.arima1 <- LTC.weekly %>% model("ARIMA(1,1,0)" = ARIMA(log_quote ~ pdq(c(0:5),0:2,c(0:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(LTC.fit.arima1)
arima1 <- tidy(LTC.fit.arima1) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

LTC.fit.arima2 <- LTC.weekly %>% model("ARIMA(0,1,1)" = ARIMA(log_quote ~ pdq(c(0,2:5),0:2,c(0:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(LTC.fit.arima2)
arima2 <- tidy(LTC.fit.arima2) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

LTC.fit.arima3 <- LTC.weekly %>% model("ARIMA(2,1,0)" = ARIMA(log_quote ~ pdq(c(0,2:5),0:2,c(0,2:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(LTC.fit.arima3)
arima3 <- tidy(LTC.fit.arima3) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

arima1 <- reshape2::dcast(arima1, .model ~ term) %>% mutate(AICc = LTC.fit.arima1[[1]][[1]][["fit"]][["fit"]][["AICc"]])
arima2 <- reshape2::dcast(arima2, .model ~ term) %>% mutate(AICc = LTC.fit.arima2[[1]][[1]][["fit"]][["fit"]][["AICc"]])
arima3 <- reshape2::dcast(arima3, .model ~ term) %>% mutate(AICc = LTC.fit.arima3[[1]][[1]][["fit"]][["fit"]][["AICc"]])

LTC.arima <- bind_rows(arima3, arima1, arima2) %>% select(1:3,5,4)


# ------------------------------------------------ Residuals ------------------------------------------------ #

LTC.fit.arima1.aug <- LTC.fit.arima1 %>% augment()

LTC.fit.arima1.aug %>% autoplot(.resid) + xlab("Year") + ylab(NULL) + ggtitle("Residuals from ARIMA (1,1,0)")
LTC.fit.arima1.aug %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 50) + ggtitle("Histogram of residuals") + xlab(NULL) + ylab(NULL)
LTC.fit.arima1.aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals") + xlab("Lag") + ylab(NULL)


# Box-Pierce / Ljung-Box
LTC.fit.arima1.BoxP <- LTC.fit.arima1 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 1)
LTC.fit.arima1.LBox <- LTC.fit.arima1 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 1)
LTC.fit.arima2.BoxP <- LTC.fit.arima2 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 1)
LTC.fit.arima2.LBox <- LTC.fit.arima2 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 1)
LTC.fit.arima3.BoxP <- LTC.fit.arima3 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 2)
LTC.fit.arima3.LBox <- LTC.fit.arima3 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 2)

LTC.fit.arima.BoxP <- bind_rows(LTC.fit.arima1.BoxP, LTC.fit.arima2.BoxP, LTC.fit.arima3.BoxP)
LTC.fit.arima.LBox <- bind_rows(LTC.fit.arima1.LBox, LTC.fit.arima2.LBox, LTC.fit.arima3.LBox)
LTC.fit.arima.Autoc <- left_join(LTC.fit.arima.BoxP[,-2], LTC.fit.arima.LBox[,-2], by = ".model")

LTC.fit.arima.report <- left_join(LTC.arima, LTC.fit.arima.Autoc, by = ".model") %>% rename(Litecoin = .model)


# ------------------------------------------------ Forecast ------------------------------------------------ #


LTC.arima.accuracy <- LTC.weekly %>%
  stretch_tsibble(.init = 10) %>%
  model(ARIMA(log_quote ~ pdq(c(0:5),0:2,c(0:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE)) %>%
  forecast(h = 1) %>%
  accuracy(LTC.weekly) %>%
  rename(LTC = .model)

LTC.arima.accuracy_ <- LTC.weekly %>%
  stretch_tsibble(.init = 10) %>%
  model("ARIMA(1,1,0)" = ARIMA(log_quote ~ 0 + pdq(1,1,0) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE)) %>%
  forecast(h = 1) %>%
  accuracy(LTC.weekly) %>%
  rename(LTC = .model)

fit.arima %>% forecast(h = 10) %>% hilo()
fit.arima %>% forecast(h = 10) %>% autoplot(slice(LTC.weekly, (n()-80):n())) + xlab("Year") + ylab(NULL)
fit.arima %>% forecast(h = 10) %>% autoplot(LTC.weekly) + xlab("Year") + ylab(NULL)



# ----------------------------------------------- Random Walk ----------------------------------------------- #

LTC.weekly_tr <- LTC.weekly %>%
  slice(1:(n()-1)) %>%
  stretch_tsibble(.init = 3, .step = 1)
fc <- LTC.weekly_tr %>%
  model(RW(log_quote ~ drift())) %>%
  forecast(h = 1)

# Test Residual accuracy
fc %>% accuracy(LTC.weekly)
# Training Residual accuracy
LTC.weekly %>% model(RW(log_quote ~ drift())) %>% accuracy()


# XRP -----------------------------------------------------------------------------------------------------

XRP <- crypto_history(coin = "XRP", start_date = "20130805", end_date = "20200829")[,c(-1:-3,-5:-8)]

## Daily Data --------------------------------------------------------------------------------------------------

XRP.daily <- XRP %>% 
  as_tsibble(index = date) %>% 
  rename(quote = close) %>%
  mutate(log_quote = log(quote),
         log_return = difference(log_quote))


plot(x = XRP.daily$date, y = XRP.daily$quote, main = "XRP Quote (USD)", xlab = "Year", ylab = NA, type = "l") # price plot
plot(x = XRP.daily$date, y = XRP.daily$log_quote, main = "XRP Log Quote", xlab = "Year", ylab = NA, type = "l") # price plot
plot(x = XRP.daily$date, y = XRP.daily$log_return, main = "XRP Log Return", xlab = "Year", ylab = NA, type = "l") # price plot
XRP.daily %>% ACF(quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - XRP Quote") # price ACF plot


max(XRP.daily$quote)
min(XRP.daily$quote)

hist(x = XRP.daily$log_return, breaks = 10, main = "Histogram - XRP Log Return" , xlim = c(-0.3, 0.3), 
     xlab = NA, ylab = NA) # histogram log return


XRP.daily <- left_join(XRP.daily, crypto, by = "date") %>%
  rename(market = market.x,
         volume = volume.x,
         market.overall = market.y,
         volume.overall = volume.y) %>%
  mutate(market.dominance = ifelse(market/market.overall > 1, NA, market/market.overall * 100))


# market dominance
par(mar = c(5, 4, 4, 4) + 0.3)                                                                                           # Additional space for second y-axis
plot(x = XRP.daily$date, y = XRP.daily$market.dominance, main = "XRP Market Dominance vs Quote", xlab = NA, ylab = "XRP Market Dominance (%)", type ="l")        # Create first plot
par(new = TRUE)                                                                                                          # Add new plot
plot(x = XRP.daily$date, y = XRP.daily$market.overall/1000000000, axes = F, xlab = NA, ylab = NA, type = "l", col = 3)
axis(side = 4, at = pretty(range(XRP.daily$market.overall/1000000000)))                                                  # Add second axis
mtext("Overall Market Cap (G USD)", side = 4, line = 3)         



## Weekly Data -------------------------------------------------------------------------------------------------

XRP.weekly <- XRP %>%
  rename(quote = close) %>%
  mutate(week = as.numeric(date-date[1]) %/% 7,
         last = !duplicated(week,fromLast=TRUE)) 

XRP.weekly.last <- XRP.weekly %>% filter(last == TRUE) %>% distinct(date, week)

XRP.weekly <- XRP.weekly %>%
  select(quote, week) %>%
  group_by(week) %>%
  summarise(quote = mean(quote)) %>%
  ungroup()
XRP.weekly <- left_join(XRP.weekly, XRP.weekly.last, by = "week") %>% select(-week) %>% select(2,1)

XRP.weekly <- XRP.weekly %>% as_tsibble(index = date)

XRP.weekly <- XRP.weekly %>%
  mutate(log_quote = log(quote),
         log_return = difference(log_quote))


# price plot
plot(x = XRP.weekly$date, y = XRP.weekly$quote, main = "XRP Quote (USD)", xlab = "Year", ylab = NA, type = "l")
# log price plot
plot(x = XRP.weekly$date, y = XRP.weekly$log_quote, main = "XRP Log Quote", xlab = "Year", ylab = NA, type = "l")
# log return plot (first difference of log price)
plot(x = XRP.weekly$date, y = XRP.weekly$log_return, main = "XRP Log Return", xlab = "Year", ylab = NA, type = "l")


### Descriptive Statistics ------------------------------------------------------------------------------------- 


summary <- XRP.weekly %>% descr(log_return)
XRP.summary <- data.frame(XRP = summary[c(1:3,7,11,13),])

hist(XRP.weekly$log_return, breaks = 50, main = "Histogram - XRP Log Return", xlim = c(-0.6, 1.21), xlab = NA, ylab = NA)

# --------------------------------------------- Stationarity ---------------------------------------------- #

XRP.KPSS.LQ <- XRP.weekly %>% features(log_quote, unitroot_kpss) # KPSS unit root test 
XRP.KPSS.LR <- XRP.weekly %>% features(log_return, unitroot_kpss) # KPSS unit root test

XRP.URoots <- XRP.weekly %>% features(log_quote, unitroot_ndiffs) # testing for number of unit roots
XRP.SDiff <- XRP.weekly %>% features(log_quote, unitroot_nsdiffs) # testing for seasonal differences required


XRP.stationarity <- bind_cols(CRYPTO = "XRP",XRP.KPSS.LQ, XRP.URoots, XRP.SDiff)

# --------------------------------------------- Autocorrelation ---------------------------------------------- #

# Ljung-Box - test for autocorrelation in y
XRP.weekly %>% features(log_return, box_pierce, lag = 10)
XRP.weekly %>% features(log_return, ljung_box, lag = 10)


XRP.weekly %>% features(log_quote, feat_acf)
XRP.weekly[-1,] %>% features(log_return, mean)

XRP.weekly %>% ACF(log_quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - XRP Log Quote")
XRP.weekly %>% PACF(log_quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - XRP Log Quote")


XRP.weekly %>% ACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - XRP Log Return")
XRP.weekly %>% PACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - XRP Log Return")




### Exponential Smoothing --------------------------------------------------------------------------------------


XRP.ETS.accuracy <- XRP.weekly %>%
  stretch_tsibble(.init = 10) %>%
  model("SES - ETS(A,N,N)" = ETS(log_quote ~ error("A") + trend("N") + season("N")),
        "Holt - ETS(A,A,N)" = ETS(log_quote ~ error("A") + trend("A") + season("N")),
        "Damped - ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 1) %>%
  accuracy(XRP.weekly) %>%
  rename(XRP = .model) %>% select(c(1:2,4:5,8))


XRP.fit.ETS <- XRP.weekly %>% model(ETS(log_quote)) # select model with less AICc
report(XRP.fit.ETS) 

# SES - simple exp smooth

XRP.fit.ses <- XRP.weekly %>% model("SES - ETS(A,N,N)" = ETS(log_quote ~ error("A") + trend("N") + season("N"), opt_crit = "mse"))
report(XRP.fit.ses)
XRP.fit.ses.report <- tidy(XRP.fit.ses)
XRP.fit.ses.report <- reshape2::dcast(XRP.fit.ses.report, .model ~ term)
XRP.fit.ses.report <- cbind(XRP.fit.ses.report,XRP.fit.ses[[1]][[1]][["fit"]][["fit"]])[,-c(9:11)]

XRP.fit.ses.aug <- XRP.fit.ses %>% augment()
XRP.fit.ses.aug %>% ACF(.resid) %>% autoplot() + ggtitle("XRP - ETS(A,N,N) - ACF of residuals") + xlab("Lag") + ylab(NULL)


# Box-Pierce / Ljung-Box
XRP.fit.ses.BoxP <- XRP.fit.ses.aug %>% features(.resid, box_pierce, lag = 10, dof = 1)
XRP.fit.ses.LBox <- XRP.fit.ses.aug %>% features(.resid, ljung_box, lag = 10, dof = 1)


# TES - trend exp smooth - Holt

XRP.fit.trend <- XRP.weekly %>% model("Holt - ETS(A,A,N)" = ETS(log_quote ~ error("A") + trend("A") + season("N")))
XRP.fit.trend.report <- tidy(XRP.fit.trend)
XRP.fit.trend.report <- reshape2::dcast(XRP.fit.trend.report, .model ~ term)
XRP.fit.trend.report <- cbind(XRP.fit.trend.report,XRP.fit.trend[[1]][[1]][["fit"]][["fit"]])[,-c(11:13)]

XRP.fit.trend.aug <- XRP.fit.trend %>% augment()

# Box-Pierce / Ljung-Box
XRP.fit.trend.BoxP <- XRP.fit.trend.aug %>% features(.resid, box_pierce, lag = 10, dof = 2)
XRP.fit.trend.LBox <- XRP.fit.trend.aug %>% features(.resid, ljung_box, lag = 10, dof = 2)


# Damped

XRP.fit.damped <- XRP.weekly %>% model("Damped - ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N")))
XRP.fit.damped.report <- tidy(XRP.fit.damped)
XRP.fit.damped.report <- reshape2::dcast(XRP.fit.damped.report, .model ~ term)
XRP.fit.damped.report <- cbind(XRP.fit.damped.report,XRP.fit.damped[[1]][[1]][["fit"]][["fit"]])[,-c(12:14)]

XRP.fit.damped.aug <- XRP.fit.damped %>% augment()

# Box-Pierce / Ljung-Box
XRP.fit.damped.BoxP <- XRP.fit.damped.aug %>% features(.resid, box_pierce, lag = 10, dof = 3)
XRP.fit.damped.LBox <- XRP.fit.damped.aug %>% features(.resid, ljung_box, lag = 10, dof = 3)

XRP.fit.ETS.report <- bind_rows(XRP.fit.damped.report, XRP.fit.trend.report, XRP.fit.ses.report)[c(3:1),c(1,2,5:3,6:11)] 

XRP.fit.ETS.BoxP <- bind_rows(XRP.fit.ses.BoxP, XRP.fit.trend.BoxP, XRP.fit.damped.BoxP)
XRP.fit.ETS.LBox <- bind_rows(XRP.fit.ses.LBox, XRP.fit.trend.LBox, XRP.fit.damped.LBox)
XRP.fit.ETS.Autoc <- left_join(XRP.fit.ETS.BoxP[,-2], XRP.fit.ETS.LBox[,-2], by = ".model")

XRP.fit.ETS.report <- left_join(XRP.fit.ETS.report, XRP.fit.ETS.Autoc, by = ".model") %>% rename(XRP = .model)

# ------------------------------------------------ Residuals ------------------------------------------------ #

XRP.fit.ETS.aug <- XRP.fit.damped %>% augment()

XRP.fit.ETS.aug %>% autoplot(.resid) + xlab("Year") + ylab(NULL) + ggtitle("Residuals from ETS(A,Ad,N)")
XRP.fit.ETS.aug %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 50) + ggtitle("Histogram of residuals") + xlab(NULL) + ylab(NULL)
XRP.fit.ETS.aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals") + xlab("Lag") + ylab(NULL)

# Box-Pierce
XRP.fit.ETS.aug %>% features(.resid, box_pierce, lag = 10, dof = 0)
# Ljung-Box
XRP.fit.ETS.aug %>% features(.resid, ljung_box, lag = 10, dof = 0)


fit.damped %>% forecast(h = 10) %>% autoplot(XRP.weekly) + xlab("Year") + ylab("Log Quote") +  ggtitle("Damped Exponential Smoothing - XRP Log Quote")

components(XRP.fit.ETS) %>% autoplot() + ggtitle("ETS(A,Ad,N) components") + xlab("Year")

### ARIMA ------------------------------------------------------------------------------------------------------


# model selection
XRP.weekly %>% ACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - XRP Log Return")
XRP.weekly %>% PACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - XRP Log Return")

XRP.fit.arima1 <- XRP.weekly %>% model("ARIMA(0,1,1)" = ARIMA(log_quote ~ pdq(c(0:5),0:2,c(0:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(XRP.fit.arima1)
arima1 <- tidy(XRP.fit.arima1) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

XRP.fit.arima2 <- XRP.weekly %>% model("ARIMA(0,1,2)" = ARIMA(log_quote ~ pdq(c(0:5),0:2,c(0,2:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(XRP.fit.arima2)
arima2 <- tidy(XRP.fit.arima2) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

XRP.fit.arima3 <- XRP.weekly %>% model("ARIMA(2,1,0)" = ARIMA(log_quote ~ pdq(c(0:5),0:2,c(0,3:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(XRP.fit.arima3)
arima3 <- tidy(XRP.fit.arima3) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

arima1 <- reshape2::dcast(arima1, .model ~ term) %>% mutate(AICc = XRP.fit.arima1[[1]][[1]][["fit"]][["fit"]][["AICc"]])
arima2 <- reshape2::dcast(arima2, .model ~ term) %>% mutate(AICc = XRP.fit.arima2[[1]][[1]][["fit"]][["fit"]][["AICc"]])
arima3 <- reshape2::dcast(arima3, .model ~ term) %>% mutate(AICc = XRP.fit.arima3[[1]][[1]][["fit"]][["fit"]][["AICc"]])

XRP.arima <- bind_rows(arima3, arima2, arima1) %>% select(1:3,5:6,4)


# ------------------------------------------------ Residuals ------------------------------------------------ #

XRP.fit.arima1.aug <- XRP.fit.arima1 %>% augment()

XRP.fit.arima1.aug %>% autoplot(.resid) + xlab("Year") + ylab(NULL) + ggtitle("Residuals from ARIMA (1,1,0)")
XRP.fit.arima1.aug %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 50) + ggtitle("Histogram of residuals") + xlab(NULL) + ylab(NULL)
XRP.fit.arima1.aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals") + xlab("Lag") + ylab(NULL)


# Box-Pierce / Ljung-Box
XRP.fit.arima1.BoxP <- XRP.fit.arima1 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 1)
XRP.fit.arima1.LBox <- XRP.fit.arima1 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 1)
XRP.fit.arima2.BoxP <- XRP.fit.arima2 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 2)
XRP.fit.arima2.LBox <- XRP.fit.arima2 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 2)
XRP.fit.arima3.BoxP <- XRP.fit.arima3 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 2)
XRP.fit.arima3.LBox <- XRP.fit.arima3 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 2)

XRP.fit.arima.BoxP <- bind_rows(XRP.fit.arima1.BoxP, XRP.fit.arima2.BoxP, XRP.fit.arima3.BoxP)
XRP.fit.arima.LBox <- bind_rows(XRP.fit.arima1.LBox, XRP.fit.arima2.LBox, XRP.fit.arima3.LBox)
XRP.fit.arima.Autoc <- left_join(XRP.fit.arima.BoxP[,-2], XRP.fit.arima.LBox[,-2], by = ".model")

XRP.fit.arima.report <- left_join(XRP.arima, XRP.fit.arima.Autoc, by = ".model") %>% rename(XRP = .model)


# ------------------------------------------------ Forecast ------------------------------------------------ #


fit.arima %>% forecast(h = 10) %>% hilo()
fit.arima %>% forecast(h = 10) %>% autoplot(slice(XRP.weekly, (n()-80):n())) + xlab("Year") + ylab(NULL)
fit.arima %>% forecast(h = 10) %>% autoplot(XRP.weekly) + xlab("Year") + ylab(NULL)



# -------------------------------------------- Model Comparison -------------------------------------------- #

XRP.weekly %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    ETS(log_quote ~ error("A") + trend("Ad") + season("N")),
    ARIMA(log_quote ~ pdq(0,1,1))
  ) %>%
  forecast(h = 1) %>%
  accuracy(XRP.weekly)


# ----------------------------------------------- Random Walk ----------------------------------------------- #

XRP.weekly_tr <- XRP.weekly %>%
  slice(1:(n()-1)) %>%
  stretch_tsibble(.init = 3, .step = 1)
fc <- XRP.weekly_tr %>%
  model(RW(log_quote ~ drift())) %>%
  forecast(h = 1)

# Test Residual accuracy
fc %>% accuracy(XRP.weekly)
# Training Residual accuracy
XRP.weekly %>% model(RW(log_quote ~ drift())) %>% accuracy()


# BCH -----------------------------------------------------------------------------------------------------

BCH <- crypto_history(coin = "BCH", start_date = "20170724", end_date = "20200829")[,c(-1:-3,-5:-8)]

## Daily Data --------------------------------------------------------------------------------------------------

BCH.daily <- BCH %>% 
  as_tsibble(index = date) %>% 
  rename(quote = close) %>%
  mutate(log_quote = log(quote),
         log_return = difference(log_quote))


plot(x = BCH.daily$date, y = BCH.daily$quote, main = "BCH Quote (USD)", xlab = "Year", ylab = NA, type = "l") # price plot
plot(x = BCH.daily$date, y = BCH.daily$log_quote, main = "BCH Log Quote", xlab = "Year", ylab = NA, type = "l") # price plot
plot(x = BCH.daily$date, y = BCH.daily$log_return, main = "BCH Log Return", xlab = "Year", ylab = NA, type = "l") # price plot
BCH.daily %>% ACF(quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - BCH Quote") # price ACF plot


max(BCH.daily$quote)
min(BCH.daily$quote)

hist(x = BCH.daily$log_return, breaks = 10, main = "Histogram - BCH Log Return" , xlim = c(-0.3, 0.3), 
     xlab = NA, ylab = NA) # histogram log return


BCH.daily <- left_join(BCH.daily, crypto, by = "date") %>%
  rename(market = market.x,
         volume = volume.x,
         market.overall = market.y,
         volume.overall = volume.y) %>%
  mutate(market.dominance = ifelse(market/market.overall > 1, NA, market/market.overall * 100))


# market dominance
par(mar = c(5, 4, 4, 4) + 0.3)                                                                                           # Additional space for second y-axis
plot(x = BCH.daily$date, y = BCH.daily$market.dominance, main = "BCH Market Dominance vs Quote", xlab = NA, ylab = "BCH Market Dominance (%)", type ="l")        # Create first plot
par(new = TRUE)                                                                                                          # Add new plot
plot(x = BCH.daily$date, y = BCH.daily$market.overall/1000000000, axes = F, xlab = NA, ylab = NA, type = "l", col = 3)
axis(side = 4, at = pretty(range(BCH.daily$market.overall/1000000000)))                                                  # Add second axis
mtext("Overall Market Cap (G USD)", side = 4, line = 3)         



## Weekly Data -------------------------------------------------------------------------------------------------

BCH.weekly <- BCH %>%
  rename(quote = close) %>%
  mutate(week = as.numeric(date-date[1]) %/% 7,
         last = !duplicated(week,fromLast=TRUE)) 

BCH.weekly.last <- BCH.weekly %>% filter(last == TRUE) %>% distinct(date, week)

BCH.weekly <- BCH.weekly %>%
  select(quote, week) %>%
  group_by(week) %>%
  summarise(quote = mean(quote)) %>%
  ungroup()
BCH.weekly <- left_join(BCH.weekly, BCH.weekly.last, by = "week") %>% select(-week) %>% select(2,1)

BCH.weekly <- BCH.weekly %>% as_tsibble(index = date)

BCH.weekly <- BCH.weekly %>%
  mutate(log_quote = log(quote),
         log_return = difference(log_quote))


# price plot
plot(x = BCH.weekly$date, y = BCH.weekly$quote, main = "BCH Quote (USD)", xlab = "Year", ylab = NA, type = "l")
# log price plot
plot(x = BCH.weekly$date, y = BCH.weekly$log_quote, main = "BCH Log Quote", xlab = "Year", ylab = NA, type = "l")
# log return plot (first difference of log price)
plot(x = BCH.weekly$date, y = BCH.weekly$log_return, main = "BCH Log Return", xlab = "Year", ylab = NA, type = "l")




### Descriptive Statistics ------------------------------------------------------------------------------------- 

summary <- BCH.weekly %>% descr(log_return)
BCH.summary <- data.frame(BCH = summary[c(1:3,7,11,13),])

hist(BCH.weekly$log_return, breaks = 50, main = "Histogram - BCH Log Return", xlim = c(-0.7, 0.6), xlab = NA, ylab = NA)

# --------------------------------------------- Autocorrelation ---------------------------------------------- #

# Ljung-Box - test for autocorrelation in y
BCH.weekly %>% features(log_return, box_pierce, lag = 10)
BCH.weekly %>% features(log_return, ljung_box, lag = 10)


BCH.weekly %>% features(log_quote, feat_acf)
BCH.weekly[-1,] %>% features(log_return, mean)

BCH.weekly %>% ACF(log_quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - BCH Log Quote")
BCH.weekly %>% PACF(log_quote) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - BCH Log Quote")


BCH.weekly %>% ACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - BCH Log Return")
BCH.weekly %>% PACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - BCH Log Return")



# --------------------------------------------- Stationarity ---------------------------------------------- #


BCH.KPSS.LQ <- BCH.weekly %>% features(log_quote, unitroot_kpss) # KPSS unit root test 
BCH.KPSS.LR <- BCH.weekly %>% features(log_return, unitroot_kpss) # KPSS unit root test

BCH.URoots <- BCH.weekly %>% features(log_quote, unitroot_ndiffs) # testing for number of unit roots
BCH.SDiff <- BCH.weekly %>% features(log_quote, unitroot_nsdiffs) # testing for seasonal differences required


BCH.stationarity <- bind_cols(CRYPTO = "BCH",BCH.KPSS.LQ, BCH.URoots, BCH.SDiff)


JarqueBera.test(BCH.daily$log_return)

### Exponential Smoothing --------------------------------------------------------------------------------------


BCH.ETS.accuracy <- BCH.weekly %>%
  stretch_tsibble(.init = 10) %>%
  model("SES - ETS(A,N,N)" = ETS(log_quote ~ error("A") + trend("N") + season("N")),
        "Holt - ETS(A,A,N)" = ETS(log_quote ~ error("A") + trend("A") + season("N")),
        "Damped - ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N"))) %>%
  forecast(h = 1) %>%
  accuracy(BCH.weekly) %>%
  rename(BCH = .model) %>% select(c(1:2,4:5,8))


BCH.fit.ETS <- BCH.weekly %>% model(ETS(log_quote)) # select model with less AICc
report(BCH.fit.ETS) 

# SES - simple exp smooth

BCH.fit.ses <- BCH.weekly %>% model("SES - ETS(A,N,N)" = ETS(log_quote ~ error("A") + trend("N") + season("N")))
report(BCH.fit.ses)
BCH.fit.ses.report <- tidy(BCH.fit.ses)
BCH.fit.ses.report <- reshape2::dcast(BCH.fit.ses.report, .model ~ term)
BCH.fit.ses.report <- cbind(BCH.fit.ses.report,BCH.fit.ses[[1]][[1]][["fit"]][["fit"]])[,-c(9:11)]

BCH.fit.ses.aug <- BCH.fit.ses %>% augment()
BCH.fit.ses.aug %>% ACF(.resid) %>% autoplot() + ggtitle("BCH - ETS(A,N,N) - ACF of residuals") + xlab("Lag") + ylab(NULL)

# Box-Pierce / Ljung-Box
BCH.fit.ses.aug %>% features(.resid, box_pierce, lag = 10, dof = 1)
BCH.fit.ses.aug %>% features(.resid, ljung_box, lag = 10, dof = 1)


# TES - trend exp smooth - Holt

BCH.fit.trend <- BCH.weekly %>% model("Holt - ETS(A,A,N)" = ETS(log_quote ~ error("A") + trend("A") + season("N")))
BCH.fit.trend.report <- tidy(BCH.fit.trend)
BCH.fit.trend.report <- reshape2::dcast(BCH.fit.trend.report, .model ~ term)
BCH.fit.trend.report <- cbind(BCH.fit.trend.report,BCH.fit.trend[[1]][[1]][["fit"]][["fit"]])[,-c(11:13)]

BCH.fit.trend.aug <- BCH.fit.trend %>% augment()

# Box-Pierce / Ljung-Box
BCH.fit.trend.BoxP <- BCH.fit.trend.aug %>% features(.resid, box_pierce, lag = 10, dof = 2)
BCH.fit.trend.LBox <- BCH.fit.trend.aug %>% features(.resid, ljung_box, lag = 10, dof = 2)


# Damped

BCH.fit.damped <- BCH.weekly %>% model("Damped - ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N")))
BCH.fit.damped.report <- tidy(BCH.fit.damped)
BCH.fit.damped.report <- reshape2::dcast(BCH.fit.damped.report, .model ~ term)
BCH.fit.damped.report <- cbind(BCH.fit.damped.report,BCH.fit.damped[[1]][[1]][["fit"]][["fit"]])[,-c(12:14)]

BCH.fit.damped.aug <- BCH.fit.damped %>% augment()

# Box-Pierce / Ljung-Box
BCH.fit.damped.BoxP <- BCH.fit.damped.aug %>% features(.resid, box_pierce, lag = 10, dof = 3)
BCH.fit.damped.LBox <- BCH.fit.damped.aug %>% features(.resid, ljung_box, lag = 10, dof = 3)

BCH.fit.ETS.report <- bind_rows(BCH.fit.damped.report, BCH.fit.trend.report, BCH.fit.ses.report)[c(3:1),c(1,2,5:3,6:11)] 

BCH.fit.ETS.BoxP <- bind_rows(BCH.fit.ses.BoxP, BCH.fit.trend.BoxP, BCH.fit.damped.BoxP)
BCH.fit.ETS.LBox <- bind_rows(BCH.fit.ses.LBox, BCH.fit.trend.LBox, BCH.fit.damped.LBox)
BCH.fit.ETS.Autoc <- left_join(BCH.fit.ETS.BoxP[,-2], BCH.fit.ETS.LBox[,-2], by = ".model")

BCH.fit.ETS.report <- left_join(BCH.fit.ETS.report, BCH.fit.ETS.Autoc, by = ".model") %>% rename(BitcoinCash = .model)

# ------------------------------------------------ Residuals ------------------------------------------------ #

BCH.fit.ETS.aug <- BCH.fit.damped %>% augment()

BCH.fit.ETS.aug %>% autoplot(.resid) + xlab("Year") + ylab(NULL) + ggtitle("Residuals from ETS(A,Ad,N)")
BCH.fit.ETS.aug %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 50) + ggtitle("Histogram of residuals") + xlab(NULL) + ylab(NULL)
BCH.fit.ETS.aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals") + xlab("Lag") + ylab(NULL)

# Box-Pierce
BCH.fit.ETS.aug %>% features(.resid, box_pierce, lag = 10, dof = 0)
# Ljung-Box
BCH.fit.ETS.aug %>% features(.resid, ljung_box, lag = 10, dof = 0)


fit.damped %>% forecast(h = 10) %>% autoplot(BCH.weekly) + xlab("Year") + ylab("Log Quote") +  ggtitle("Damped Exponential Smoothing - BCH Log Quote")

components(BCH.fit.ETS) %>% autoplot() + ggtitle("ETS(A,Ad,N) components") + xlab("Year")



### ARIMA ------------------------------------------------------------------------------------------------------

# model selection
BCH.weekly %>% ACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("ACF - BCH Log Return")
BCH.weekly %>% PACF(log_return) %>% autoplot() + xlab("Lag") + ylab(NULL) + ggtitle("PACF - BCH Log Return")

BCH.fit.arima1 <- BCH.weekly %>% model("ARIMA(2,1,3)" = ARIMA(log_quote ~ pdq(c(0:5),0:2,c(0:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(BCH.fit.arima1)
arima1 <- tidy(BCH.fit.arima1) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

BCH.fit.arima2 <- BCH.weekly %>% model("ARIMA(0,1,1)" = ARIMA(log_quote ~ pdq(c(0:5),0:2,c(0:2,4:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(BCH.fit.arima2)
arima2 <- tidy(BCH.fit.arima2) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

BCH.fit.arima3 <- BCH.weekly %>% model("ARIMA(1,1,5)" = ARIMA(log_quote ~ pdq(c(0:5),0:2,c(0,2,4:5)) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE))
report(BCH.fit.arima3)
arima3 <- tidy(BCH.fit.arima3) %>% 
  mutate(estimate = paste0(round(estimate,5)," (", round(p.value,5),")")) %>%
  select(-c(std.error, statistic, p.value))

arima1 <- reshape2::dcast(arima1, .model ~ term) %>% mutate(AICc = BCH.fit.arima1[[1]][[1]][["fit"]][["fit"]][["AICc"]])
arima2 <- reshape2::dcast(arima2, .model ~ term) %>% mutate(AICc = BCH.fit.arima2[[1]][[1]][["fit"]][["fit"]][["AICc"]])
arima3 <- reshape2::dcast(arima3, .model ~ term) %>% mutate(AICc = BCH.fit.arima3[[1]][[1]][["fit"]][["fit"]][["AICc"]])

BCH.arima <- bind_rows(arima3, arima1, arima2) %>% select(1:2,9,3:8)


# ------------------------------------------------ Residuals ------------------------------------------------ #

BCH.fit.arima1.aug <- BCH.fit.arima1 %>% augment()

BCH.fit.arima1.aug %>% autoplot(.resid) + xlab("Year") + ylab(NULL) + ggtitle("Residuals from ARIMA (2,1,3)")
BCH.fit.arima1.aug %>% ggplot(aes(x = .resid)) + geom_histogram(bins = 50) + ggtitle("Histogram of residuals") + xlab(NULL) + ylab(NULL)
BCH.fit.arima1.aug %>% ACF(.resid) %>% autoplot() + ggtitle("ACF of residuals") + xlab("Lag") + ylab(NULL)


# Box-Pierce / Ljung-Box
BCH.fit.arima1.BoxP <- BCH.fit.arima1 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 5)
BCH.fit.arima1.LBox <- BCH.fit.arima1 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 5)
BCH.fit.arima2.BoxP <- BCH.fit.arima2 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 1)
BCH.fit.arima2.LBox <- BCH.fit.arima2 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 1)
BCH.fit.arima3.BoxP <- BCH.fit.arima3 %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 6)
BCH.fit.arima3.LBox <- BCH.fit.arima3 %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 6)

BCH.fit.arima.BoxP <- bind_rows(BCH.fit.arima1.BoxP, BCH.fit.arima2.BoxP, BCH.fit.arima3.BoxP)
BCH.fit.arima.LBox <- bind_rows(BCH.fit.arima1.LBox, BCH.fit.arima2.LBox, BCH.fit.arima3.LBox)
BCH.fit.arima.Autoc <- left_join(BCH.fit.arima.BoxP[,-2], BCH.fit.arima.LBox[,-2], by = ".model")

BCH.fit.arima.report <- left_join(BCH.arima, BCH.fit.arima.Autoc, by = ".model") %>% rename(BitcoinCash = .model)


# ------------------------------------------------ Forecast ------------------------------------------------ #


fit.arima %>% forecast(h = 10) %>% hilo()
fit.arima %>% forecast(h = 10) %>% autoplot(slice(BCH.weekly, (n()-80):n())) + xlab("Year") + ylab(NULL)
fit.arima %>% forecast(h = 10) %>% autoplot(BCH.weekly) + xlab("Year") + ylab(NULL)



# -------------------------------------------- Model Comparison -------------------------------------------- #

BCH.weekly %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    ETS(log_quote ~ error("A") + trend("Ad") + season("N")),
    ARIMA(log_quote ~ pdq(0,1,1))
  ) %>%
  forecast(h = 1) %>%
  accuracy(BCH.weekly)


# ----------------------------------------------- Random Walk ----------------------------------------------- #

BCH.weekly_tr <- BCH.weekly %>%
  slice(1:(n()-1)) %>%
  stretch_tsibble(.init = 3, .step = 1)
fc <- BCH.weekly_tr %>%
  model(RW(log_quote ~ drift())) %>%
  forecast(h = 1)

# Test Residual accuracy
fc %>% accuracy(BCH.weekly)
# Training Residual accuracy
BCH.weekly %>% model(RW(log_quote ~ drift())) %>% accuracy()


# ETS VS. ARIMA -------------------------------------------------------------------------------------------------------

# BITCOIN
BTC.accuracy <- BTC.weekly %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model("ARIMA(1,1,0) w/ drift" = ARIMA(log_quote ~ 1 + pdq(1,1,0) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE),
        "ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N")),
        "RW w/drift" = RW(log_quote ~ drift()),
        "RW" = RW(log_quote)) %>%
  forecast(h = 1) %>%
  accuracy(BTC.weekly) %>%
  rename(Bitcoin = .model) %>% select(1,2,4,5,8)

BTC.accuracy$Bitcoin[BTC.accuracy$RMSE == min(BTC.accuracy$RMSE)]
BTC.accuracy$Bitcoin[BTC.accuracy$MAE == min(BTC.accuracy$MAE)]
BTC.accuracy$Bitcoin[BTC.accuracy$MASE == min(BTC.accuracy$MASE)]

BTC.fit.arima1 %>% forecast(h = 5) %>% hilo()
BTC.fit.arima1 %>% forecast(h = 5) %>% autoplot(slice(BTC.weekly, (n()-80):n())) + xlab("Date") + ylab(NULL) + ggtitle("BTC Log Quote - 5-step forecast (zoomed)")
BTC.fit.arima1 %>% forecast(h = 5) %>% autoplot(BTC.weekly) + xlab("Year") + ylab(NULL) + ggtitle("BTC Log Quote - 5-step forecast")
BTC.forecast <- BTC.fit.arima1 %>% forecast(h = 5) %>% mutate(quote = exp(.mean))



# ETHER
ETH.accuracy <- ETH.weekly %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model("ARIMA(1,1,4)" = ARIMA(log_quote ~ 0 + pdq(1,1,4) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE),
        "ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N")),
        "RW w/drift" = RW(log_quote ~ drift()),
        "RW" = RW(log_quote)) %>%
  forecast(h = 1) %>%
  accuracy(ETH.weekly) %>%
  rename(Ether = .model) %>% select(1,2,4,5,8)

ETH.accuracy$Ether[ETH.accuracy$RMSE == min(ETH.accuracy$RMSE)]
ETH.accuracy$Ether[ETH.accuracy$MAE == min(ETH.accuracy$MAE)]
ETH.accuracy$Ether[ETH.accuracy$MASE == min(ETH.accuracy$MASE)]


ETH.fit.RW <- ETH.weekly %>% model("RW" = RW(log_quote))
ETH.fit.RW.BoxP <- ETH.fit.RW %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 0)
ETH.fit.RW.LBox <- ETH.fit.RW %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 0)

ETH.fit.arima1 %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("ARIMA(1,1,4) - ACF of Residuals") + xlab("Lag") + ylab(NULL)
ETH.fit.RW %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("RW - ACF of residuals") + xlab("Lag") + ylab(NULL)


ETH.fit.arima1 %>% forecast(h = 5) %>% hilo()
ETH.fit.arima1 %>% forecast(h = 5) %>% autoplot(slice(ETH.weekly, (n()-80):n())) + xlab("Date") + ylab(NULL) + ggtitle("ETH Log Quote - 5-step forecast (zoomed)")
ETH.fit.arima1 %>% forecast(h = 5) %>% autoplot(ETH.weekly) + xlab("Year") + ylab(NULL) + ggtitle("ETH Log Quote - 5-step forecast")
ETH.forecast <- ETH.fit.arima1 %>% forecast(h = 5) %>% mutate(quote = exp(.mean))

# LITECOIN
LTC.accuracy <- LTC.weekly %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model("ARIMA(1,1,0)" = ARIMA(log_quote ~ 0 + pdq(1,1,0) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE),
        "ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N")),
        "RW w/drift" = RW(log_quote ~ drift()),
        "RW" = RW(log_quote)) %>%
  forecast(h = 1) %>%
  accuracy(LTC.weekly) %>%
  rename(Litecoin = .model) %>% select(1,2,4,5,8)

LTC.accuracy$Litecoin[LTC.accuracy$RMSE == min(LTC.accuracy$RMSE)]
LTC.accuracy$Litecoin[LTC.accuracy$MAE == min(LTC.accuracy$MAE)]
LTC.accuracy$Litecoin[LTC.accuracy$MASE == min(LTC.accuracy$MASE)]

LTC.fit.arima1 %>% forecast(h = 5) %>% hilo()
LTC.fit.arima1 %>% forecast(h = 5) %>% autoplot(slice(LTC.weekly, (n()-80):n())) + xlab("Date") + ylab(NULL) + ggtitle("LTC Log Quote - 5-step forecast (zoomed)")
LTC.fit.arima1 %>% forecast(h = 5) %>% autoplot(LTC.weekly) + xlab("Year") + ylab(NULL) + ggtitle("LTC Log Quote - 5-step forecast")
LTC.forecast <- LTC.fit.arima1 %>% forecast(h = 5) %>% mutate(quote = exp(.mean))

# XRP
XRP.accuracy <- XRP.weekly %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model("ARIMA(0,1,1)" = ARIMA(log_quote ~ 0 + pdq(0,1,1) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE),
        "ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N")),
        "RW w/drift" = RW(log_quote ~ drift()),
        "RW" = RW(log_quote)) %>%
  forecast(h = 1) %>%
  accuracy(XRP.weekly) %>%
  rename(XRP = .model) %>% select(1,2,4,5,8)

XRP.accuracy$XRP[XRP.accuracy$RMSE == min(XRP.accuracy$RMSE)]
XRP.accuracy$XRP[XRP.accuracy$MAE == min(XRP.accuracy$MAE)]
XRP.accuracy$XRP[XRP.accuracy$MASE == min(XRP.accuracy$MASE)]

XRP.fit.arima1 %>% forecast(h = 5) %>% hilo()
XRP.fit.arima1 %>% forecast(h = 5) %>% autoplot(slice(XRP.weekly, (n()-80):n())) + xlab("Date") + ylab(NULL) + ggtitle("XRP Log Quote - 5-step forecast (zoomed)")
XRP.fit.arima1 %>% forecast(h = 5) %>% autoplot(XRP.weekly) + xlab("Year") + ylab(NULL)  + ggtitle("XRP Log Quote - 5-step forecast")
XRP.forecast <- XRP.fit.arima1 %>% forecast(h = 5) %>% mutate(quote = exp(.mean))

# BITCOIN CASH
BCH.accuracy <- BCH.weekly %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model("ARIMA(2,1,3)" = ARIMA(log_quote ~ 0 + pdq(2,1,3) + PDQ(0,0,0), approximation = FALSE, stepwise = FALSE),
        "ETS(A,N,N)" = ETS(log_quote ~ error("A") + trend("N") + season("N")),
        "ETS(A,Ad,N)" = ETS(log_quote ~ error("A") + trend("Ad") + season("N")),
        "RW w/drift" = RW(log_quote ~ drift()),
        "RW" = RW(log_quote)) %>%
  forecast(h = 1) %>%
  accuracy(BCH.weekly) %>%
  rename(BCH = .model) %>% select(1,2,4,5,8)
BCH.accuracy <- BCH.arima.accuracy %>% select(1,2,4,5,8)

BCH.accuracy$BCH[BCH.accuracy$RMSE == min(BCH.accuracy$RMSE)]
BCH.accuracy$BCH[BCH.accuracy$MAE == min(BCH.accuracy$MAE)]
BCH.accuracy$BCH[BCH.accuracy$MASE == min(BCH.accuracy$MASE)]

BCH.fit.RW <- BCH.weekly %>% model("RW" = RW(log_quote ~ drift()))

BCH.fit.RW.BoxP <- BCH.fit.RW %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 1)
BCH.fit.RW.LBox <- BCH.fit.RW %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 1)

BCH.fit.damped %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("ETS(A,Ad,N) - ACF of Residuals") + xlab("Lag") + ylab(NULL)
BCH.fit.damped %>% augment() %>% features(.resid, box_pierce, lag = 10, dof = 3)
BCH.fit.damped %>% augment() %>% features(.resid, ljung_box, lag = 10, dof = 3)

BCH.fit.arima1 %>% forecast(h = 5) %>% hilo()
BCH.fit.arima1 %>% forecast(h = 5) %>% autoplot(slice(BCH.weekly, (n()-80):n())) + xlab("Date") + ylab(NULL) + ggtitle("BCH Log Quote - 5-step forecast (zoomed)")
BCH.fit.arima1 %>% forecast(h = 5) %>% autoplot(BCH.weekly) + xlab("Year") + ylab(NULL) + ggtitle("BCH Log Quote - 5-step forecast")
BCH.forecast <- BCH.fit.arima1 %>% forecast(h = 5) %>% mutate(quote = exp(.mean))

BTC.fit.RW <- BTC.weekly %>% model("RW" = RW(log_quote))
BTC.fit.RW %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("BTC - ACF of RW residuals") + xlab("Lag") + ylab(NULL)
ETH.fit.RW <- ETH.weekly %>% model("RW" = RW(log_quote))
ETH.fit.RW %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("ETH - ACF of RW residuals") + xlab("Lag") + ylab(NULL)
LTC.fit.RW <- LTC.weekly %>% model("RW" = RW(log_quote))
LTC.fit.RW %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("LTC - ACF of RW residuals") + xlab("Lag") + ylab(NULL)
XRP.fit.RW <- XRP.weekly %>% model("RW" = RW(log_quote))
XRP.fit.RW %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("XRP - ACF of RW residuals") + xlab("Lag") + ylab(NULL)
BCH.fit.RW <- BCH.weekly %>% model("RW" = RW(log_quote))
BCH.fit.RW %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("BCH - ACF of RW residuals") + xlab("Lag") + ylab(NULL)

BTC.fit.RW <- BTC.weekly %>% model("RW" = RW(log_quote ~ drift()))
BTC.fit.RW %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("BTC - ACF of RW w/drift residuals") + xlab("Lag") + ylab(NULL)
ETH.fit.RW <- ETH.weekly %>% model("RW" = RW(log_quote ~ drift()))
ETH.fit.RW %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("ETH - ACF of RW w/drift residuals") + xlab("Lag") + ylab(NULL)
LTC.fit.RW <- LTC.weekly %>% model("RW" = RW(log_quote ~ drift()))
LTC.fit.RW %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("LTC - ACF of RW w/drift residuals") + xlab("Lag") + ylab(NULL)
XRP.fit.RW <- XRP.weekly %>% model("RW" = RW(log_quote ~ drift()))
XRP.fit.RW %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("XRP - ACF of RW w/drift residuals") + xlab("Lag") + ylab(NULL)
BCH.fit.RW <- BCH.weekly %>% model("RW" = RW(log_quote ~ drift()))
BCH.fit.RW %>% augment() %>% ACF(.resid) %>% autoplot() + ggtitle("BCH - ACF of RW w/drift residuals") + xlab("Lag") + ylab(NULL)

# Gather Tables ------------------------------------------------------------------------------------------------------

summaries <- bind_cols(BTC.summary, ETH.summary, LTC.summary, XRP.summary, BCH.summary)

stationarity <- bind_rows(BTC.stationarity, ETH.stationarity, LTC.stationarity, XRP.stationarity, BCH.stationarity)
stationairty2 <- bind_rows(BTC.KPSS.LR, ETH.KPSS.LR, LTC.KPSS.LR, XRP.KPSS.LR, BCH.KPSS.LR)




