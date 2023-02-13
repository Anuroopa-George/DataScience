#Libraries
library(ggplot2) 
library(datarium) 
library(qqplotr)
library(moments)
library(nortest)
library(tidyverse)
library(stats)
library(corrplot)
library(caret)
library(car)
library(RVAideMemoire)
library(TTR)
library(tseries)
library(forecast)

## Dataset
CO2_emissions <- read.csv("World_bank_world_development_indicators_CO2_emmisions_Task2.csv", header= TRUE)
CO2_emissions_source <- CO2_emissions[ , c(1,2,3,4,6,11,12,13,16,17)]

## EDA
names(CO2_emissions_source)
head(CO2_emissions_source)
tail(CO2_emissions_source)
str(CO2_emissions_source)
dim(CO2_emissions_source)

Income_type=as.factor(CO2_emissions_source$Income.Type)
Country=as.factor(CO2_emissions_source$Country.Name)

X<-split(CO2_emissions_source, CO2_emissions_source$Income.Type)

CO2_emissions_hi <- X[[1]]
CO2_emissions_li <- X[[2]]
CO2_emissions_lmi <- X[[3]]
CO2_emissions_umi <- X[[4]]

## EDA
names(CO2_emissions_hi)
head(CO2_emissions_hi)
tail(CO2_emissions_hi)
str(CO2_emissions_hi)
dim(CO2_emissions_hi)

names(CO2_emissions_li)
head(CO2_emissions_li)
tail(CO2_emissions_li)
str(CO2_emissions_li)
dim(CO2_emissions_li)

names(CO2_emissions_lmi)
head(CO2_emissions_lmi)
tail(CO2_emissions_lmi)
str(CO2_emissions_lmi)
dim(CO2_emissions_lmi)

names(CO2_emissions_umi)
head(CO2_emissions_umi)
tail(CO2_emissions_umi)
str(CO2_emissions_umi)
dim(CO2_emissions_umi)

boxplot(CO2.emissions.from.electricity.and.heat.production~Country.Name,CO2_emissions_source)
boxplot(CO2.emissions.from.electricity.and.heat.production~Income.Type,CO2_emissions_source)
boxplot(CO2.emissions.from.manufacturing.industries.and.construction~Income.Type,CO2_emissions_source)
boxplot(CO2.emissions.from.other.sectors~Income.Type,CO2_emissions_source)
boxplot(CO2.emissions.from.residential.buildings.and.commercial.and.public.services~Income.Type,CO2_emissions_source)
boxplot(CO2.emissions.from.transport~Income.Type,CO2_emissions_source)

##4.1 descriptive statistical analysis
summary(CO2_emissions_lmi)
sd(CO2_emissions_lmi$CO2.emissions.from.electricity.and.heat.production)
skewness(CO2_emissions_lmi$CO2.emissions.from.electricity.and.heat.production)
kurtosis(CO2_emissions_lmi$CO2.emissions.from.electricity.and.heat.production)

## Correlation Analysis

# scatter plot to find if covariance is linear or not
ggplot(CO2_emissions_lmi, aes(x = CO2.emissions.from.electricity.and.heat.production, y = CO2.intensity)) +
  geom_point()

## Shapiro test to test normality 
shapiro.test(CO2_emissions_lmi$CO2.intensity)
shapiro.test(CO2_emissions_lmi$CO2.emissions.from.electricity.and.heat.production)

# Normality plot
ggplot(mapping = aes(sample=CO2_emissions_lmi$CO2.intensity)) + 
  stat_qq_point(size = 2,color = "green") + 
  stat_qq_line(color="yellow")+ 
  xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=CO2_emissions_lmi$CO2.emissions.from.electricity.and.heat.production)) + 
  stat_qq_point(size = 2,color = "green") + 
  stat_qq_line(color="yellow")+ 
  xlab("Theoretical") + ylab("Sample")

# Correlation dataset - numeric
CO2_emissions_lmi_1 <- CO2_emissions_lmi %>% select(-Time, -Country.Name,-Income.Type)
names(CO2_emissions_lmi)
str(CO2_emissions_lmi)
head(CO2_emissions_lmi, 5)
cor(CO2_emissions_lmi_1$CO2.intensity, CO2_emissions_lmi_1$CO2.emissions.from.electricity.and.heat.production)

#Pearson correlation test
cor.test(CO2_emissions_lmi_1$CO2.intensity, CO2_emissions_lmi_1$CO2.emissions.from.electricity.and.heat.production,method="pearson")
method = "pearson")

#Correlation plot
round(cor(CO2_emissions_lmi_1), digits = 2)
corrplot(cor(CO2_emissions_lmi_1[,1:4]), method = "number", type = "upper",tl.cex=0.2)

#### Regression Analysis

###Simple Linear Regression (SLR)
model_1 <-lm(CO2.intensity ~ CO2.emissions.from.electricity.and.heat.production,CO2_emissions_lmi)
summary.lm(model_1)

plot(CO2.intensity~ CO2.emissions.from.electricity.and.heat.production,CO2_emissions_lmi,    ###Linearity
     col = "blue", 
     main = "Regression: CO2.emissions.from.electricity.and.heat.production", 
     xlab = "Total CO2 emissions", ylab = "CO2.emissions.from.electricity.and.heat.production")
abline(model_1, col="red") 

plot(model_1, 1) ###Residuals’ Independence
plot(model_1, 2)  ###Normality of residuals
plot(model_1, 3)  ###Equal variances of the residuals (Homoscedasticity)

####Multiple Linear Regression (MLR)

model_2 <-lm(CO2.intensity ~ CO2.emissions.from.electricity.and.heat.production+ CO2.emissions.from.manufacturing.industries.and.construction+CO2.emissions.from.other.sectors+CO2.emissions.from.residential.buildings.and.commercial.and.public.services+CO2.emissions.from.transport, CO2_emissions_lmi)
summary.lm(model_2)

pairs(CO2_emissions_lmi[,c(4,5,6,7)], lower.panel = NULL, pch = 19,cex = 0.2)
plot(model_2, 1)
plot(model_2, 2)
plot(model_2, 3)
vif(model_2)


#Time series Analysis

#Time series plot
elec_heat_hi <- c(CO2_emissions_hi$CO2.emissions.from.electricity.and.heat.production)
elec_heat_li <- c(CO2_emissions_li$CO2.emissions.from.electricity.and.heat.production)
elec_heat_umi <- c(CO2_emissions_umi$CO2.emissions.from.electricity.and.heat.production)
elec_heat_lmi <- c(CO2_emissions_lmi$CO2.emissions.from.electricity.and.heat.production)

CO2_emissions_ts <- ts(cbind(elec_heat_hi, elec_heat_li,elec_heat_umi,elec_heat_lmi),
                       start = min(CO2_emissions_source$Time),
                       end = max(CO2_emissions_source$Time),
                       frequency = 1)
plot(CO2_emissions_ts)

CO2_emissions_ts_hi <- ts(elec_heat_hi,
                          start = min(CO2_emissions_hi$Time),
                          end = max(CO2_emissions_hi$Time),
                          frequency = 1)
plot(CO2_emissions_ts_hi)


CO2_emissions_ts_li <- ts(elec_heat_li,
                          start = min(CO2_emissions_li$Time),
                          end = max(CO2_emissions_li$Time),
                          frequency = 1)
plot(CO2_emissions_ts_li)


CO2_emissions_ts_umi <- ts(elec_heat_umi,
                           start = min(CO2_emissions_umi$Time),
                           end = max(CO2_emissions_umi$Time),
                           frequency = 1)
plot(CO2_emissions_ts_umi)

CO2_emissions_ts_lmi <- ts(elec_heat_lmi,
                          start = min(CO2_emissions_lmi$Time),
                          end = max(CO2_emissions_lmi$Time),
                          frequency = 1)
plot(CO2_emissions_ts_lmi)

#adf test
adf.test(CO2_emissions_ts_hi)

adf.test(CO2_emissions_ts_li)

adf.test(CO2_emissions_ts_lmi)

adf.test(CO2_emissions_ts_umi)

# acf plot

acf(CO2_emissions_ts_lmi)

#pcf plot
pacf(CO2_emissions_ts_lmi)

#using Arima Model
CO2_emissions_lmi_ts_m=auto.arima(CO2_emissions_ts_lmi,ic="aic",trace = TRUE)

CO2_emissions_lmi_ts_fc=forecast(CO2_emissions_lmi_ts_m,level = c(95),h=10)
CO2_emissions_lmi_ts_fc
plot(CO2_emissions_lmi_ts_fc)

#acf plot for residual
acf(CO2_emissions_lmi_ts_fc$residuals, lag.max=20)

#Ljung-box test
Box.test(CO2_emissions_lmi_ts_fc$residuals,lag=8, type="Ljung-Box")

#forecast errors
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

# time plot forecast error

plot.ts(CO2_emissions_lmi_ts_fc$residuals) 

# make a histogram
plotForecastErrors(CO2_emissions_lmi_ts_fc$residuals) 

#Hypothesis analysis

### T-Test
t.test(CO2_emissions_lmi$CO2.emissions..kt.)



