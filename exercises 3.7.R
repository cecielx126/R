install.packages("fpp3")
library(fpp3)

###------------- 1. -------------
gdp <- global_economy %>%
  select(Country, GDP, Population) %>%
  group_by(Country) %>%
  mutate(GDPpc = GDP/Population)

summary(gdp)

autoplot(gdp, GDPpc) +
  labs(title = "Global GDP per capita",
       y = "GDP per capita")

###------------- 2. -------------
#1.
usgdp <-global_economy %>%
  filter(Country == "United States") %>%
  select(Country, GDP)

lambda_usgdp <- usgdp %>%
  features(GDP, features = guerrero) %>%
  pull(lambda_guerrero)
usgdp %>%
  autoplot(box_cox(Gas, lambda_usgdp)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "GDP with $\\lambda$ = ",
         round(lambda_usgdp,2))))

#2.
slv <-aus_livestock %>%
  filter(Animal == "Bulls, bullocks and steers")

lambda_slv <- slv %>%
  features(Count, features = guerrero) %>%
  pull(lambda_guerrero)
slv %>%
  autoplot(box_cox(Count, lambda_slv)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Slaughter of Victorian “Bulls, bullocks and steers” with $\\lambda$ = ",
         round(lambda_slv,2))))

#3.
lambda_demand <- vic_elec %>%
  features(Demand, features = guerrero) %>%
  pull(lambda_guerrero)
vic_elec %>%
  autoplot(box_cox(Demand, lambda_demand)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Victorian Electricity Demand with $\\lambda$ = ",
         round(lambda_demand,2))))

#4.
gasp <- aus_production %>%
  select(Gas)

lambda_gasp <- gasp %>%
  features(Gas, features = guerrero) %>%
  pull(lambda_guerrero)
gasp %>%
  autoplot(box_cox(Gas, lambda_gasp)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Slaughter of Victorian “Bulls, bullocks and steers” with $\\lambda$ = ",
         round(lambda_gasp,2))))

###------------- 3. -------------
lambda_canadian_gas <- canadian_gas %>%
  features(GDP, features = guerrero) %>%
  pull(lambda_guerrero)
canadian_gas %>%
  autoplot(box_cox(Gas, lambda_canadian_gas)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "GDP with $\\lambda$ = ",
         round(lambda_canadian_gas,2))))
#Box-Cox transformation doesn't yield simpler model

###------------- 4. -------------
retaildata <- xlsx::read.xlsx("retail.xlsx", sheetIndex = 1, startRow = 2)
myts <- ts(retaildata[,"A3349873A"], frequency=12, start=c(1982,4))
lambda_retail <- BoxCox.lambda(myts)
print(c("selected lambda:", lambda_retail))
fc_retail <- rwf(myts, 
                 drift = TRUE, 
                 lambda = lambda_retail,
                 h = 50,
                 level = 80)
fc_retail_biasadj <- rwf(myts, 
                         drift = TRUE, 
                         lambda = lambda_retail,
                         h = 50,
                         level = 80,
                         biasadj = TRUE)
autoplot(myts) +
  autolayer(fc_retail, series = "Drift method with Box-Cox Transformation") +
  autolayer(fc_retail_biasadj$mean, series = "Bias Adjusted") +
  guides(colour = guide_legend(title = "Forecast"))
# It would be better to choose bias adjusted Box-Cox Transformation with lambda = 0.128

###------------- 5. -------------
#1.
tbc <- aus_production %>%
  select(Tobacco)

lambda_tbc <- tbc %>%
  features(Tobacco, features = guerrero) %>%
  pull(lambda_guerrero)

print(c("Good value of lambda for Tobacco: ", 
        lambda_tbc))

tbc %>%
  autoplot(box_cox(Gas, lambda_tbc)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "GDP with $\\lambda$ = ",
         round(lambda_tbc,2))))

#2.
mse <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000)

lambda_mse <- mse %>%
  features(Passengers, features = guerrero) %>%
  pull(lambda_guerrero)

print(c("Good value of lambda for Passengers: ", 
        lambda_mse))

mse %>%
  autoplot(box_cox(Passengers, lambda_mse)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "GDP with $\\lambda$ = ",
         round(lambda_mse,2))))

#3.
pscs <- pedestrian %>%
  filter(Sensor == "Southern Cross Station") %>%
  select(Sensor, Count)

lambda_pscs <- pscs %>%
  features(Count, features = guerrero) %>%
  pull(lambda_guerrero)

print(c("Good value of lambda for Pedestrian: ", 
        lambda_pscs))

pscs %>%
  autoplot(box_cox(Count, lambda_pscs)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "GDP with $\\lambda$ = ",
         round(lambda_pscs,2))))

###------------- 6. -------------
#A 3X5 MA will lead to
#1/15(y + 2y + 3y + 3y + 3y + 2y + y) =1/15(Y1) + 2/15(Y2) +3/15(Y3) +3/15(Y4) +3/15(Y5) +2/15(Y6) +1/15(Y7) =0.067Y1 + 0.133Y2 + 0.2Y3 + 0.2Y4 + 0.2Y5 + 0.133Y6 + 0.067Y7

###------------- 7. -------------
gas <- tail(aus_production, 5*4) %>% select(Gas)
#a.
autoplot(gas, Gas) +
  labs(title = "the last five years of the Gas data",
       y = "Gas")

gg_season(gas) +
  labs(title = "the last five years of the Gas data",
       y = "Gas")

###------------- 8. -------------
library(xlsx)

retail <- read.xlsx("retail.xlsx", 
                    sheetIndex = 1,
                    startRow = 2)

ts_retail <- ts(retail[,"A3349873A"], 
                frequency=12, 
                start=c(1982,4))

autoplot(ts_retail)
x11_retail <- seas(ts_retail, x11 = "")
autoplot(x11_retail)

#There are some outliers that I didn't expect. 
#In particular, the largest outlier occurred in 2001. 
#I didn't expect the seasonal effect to decrease with increasing trend.

###------------- 9. -------------
#a.
#The number of employed persons as a whole showed an upward trend. 
#The seasonal component changes over time, 
#so that any two consecutive years have similar patterns.
#It can be seen that after excluding seasonal and trend changes, 
#there was a significant decrease in 1991 and 1992 for other reasons.

#b.
#This is very evident in remainder.

###------------- 10. -------------
#a.
autoplot(canadian_gas, Volume) +
  labs(title = "monthly Canadian gas production",
       y = "billions of cubic metres")

gg_season(canadian_gas) +
  labs(y = "billions of cubic metres",
       title = "Seasonal plot: monthly Canadian gas production")

gg_subseries(canadian_gas) +
  labs(y = "billions of cubic metres",
       title = "monthly Canadian gas production")
