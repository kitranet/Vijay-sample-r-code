setwd("C:/Users/admin/Desktop/R")

setwd("C:/Users/admin/Desktop/R/Hackathon/Train")

stocks <- read.csv("FRA- Group.csv")

summary(stocks)
str(stocks)

stocks$Date <- as.Date(stocks$Date, format="%d-%b-%y")

View(stocks)

#Example and visualization of VaR and CVaR with dummy data

#Random Number generated
#set.seed(1004)
#n.sim <- 1000
#mu <- 60  ## the mean
#sigma <- 20  ## management's view of how certain they think their estimates are
#sigma.sq <- sigma^2
#beta <- mu/sigma.sq
#alpha <- beta * mu
#severity <- rgamma(n.sim, alpha, beta)
#summary(severity)
#
#require(ggplot2)
#
##Plot Loss Distribution - This is based on Gamma distrbution
#gamma.sev <- data.frame(Severity = severity, 
#                        Distribution = rep("Gamma", each = n.sim))
#ggplot(gamma.sev, aes(x = Severity, fill = Distribution)) + 
#  geom_density(alpha = 0.3)
#
#alpha.tolerance <- 0.05
#(VaR.sev <- quantile(severity, 1 - alpha.tolerance))
#(ES.sev <- mean(gamma.sev$Severity[gamma.sev$Severity > 
#                                     VaR.sev]))
#
##Plot Loss Distribution with VaR and CVaR
#ggplot(gamma.sev, aes(x = Severity, fill = Distribution)) + 
#  geom_density(alpha = 0.3) + geom_vline(xintercept = VaR.sev, 
#                                         color = "red") + geom_vline(xintercept = ES.sev,color = "blue")
#
##Same as above
#
#alpha.tolerance <- 0.95
#(VaR.sev <- quantile(severity, alpha.tolerance))
#
#(ES.sev <- mean(gamma.sev$Severity[gamma.sev$Severity > 
#                                     VaR.sev]))
#ggplot(gamma.sev, aes(x = Severity, fill = Distribution)) + 
#  geom_density(alpha = 0.3) + geom_vline(xintercept = VaR.sev, 
#                                         color = "red") + geom_vline(xintercept = ES.sev, color = "blue")
#
##########################################################################################################

install.packages("quantmod")
install.packages("PerformanceAnalytics")

library(quantmod)
library(PerformanceAnalytics)


maxDate <- "2019-01-01"
MSFT.prices <- Ad(getSymbols("MSFT",auto.assign = F, from=maxDate))
str(MSFT.prices)
class(MSFT.prices)
barChart(MSFT.prices)

?getSymbols

#Pull Data from stocks DF

maxDate <- "2019-08-30"

Bajaj.prices <- data.frame(stocks$Date,stocks$Bajaj.Auto.Close)
Bajaj.prices <- xts(Bajaj.prices[,-1], order.by=Bajaj.prices[,1])
barChart(Bajaj.prices)

Hindustan.prices <- data.frame(stocks$Date,stocks$Hindustan.Lever)
Hindustan.prices <- xts(Hindustan.prices[,-1], order.by=Hindustan.prices[,1])
barChart(Hindustan.prices)

Infosys.prices <- data.frame(stocks$Date,stocks$Infosys)
Infosys.prices <- xts(Infosys.prices[,-1], order.by=Infosys.prices[,1])
barChart(Infosys.prices)

Maruti.prices <- data.frame(stocks$Date,stocks$Maruti)
Maruti.prices <- xts(Maruti.prices[,-1], order.by=Maruti.prices[,1])
barChart(Maruti.prices)

AsianPaints.prices <- data.frame(stocks$Date,stocks$Asian.Paint)
AsianPaints.prices <- xts(AsianPaints.prices[,-1], order.by=AsianPaints.prices[,1])
barChart(AsianPaints.prices)



#Calculate Daily Returns
Bajaj.rets <- dailyReturn(Bajaj.prices)
Hindustan.rets <- dailyReturn(Hindustan.prices)
Infosys.rets <- dailyReturn(Infosys.prices)
Maruti.rets <- dailyReturn(Maruti.prices)
AsianPaints.rets <- dailyReturn(AsianPaints.prices)

str(Bajaj.prices)

barChart(Bajaj.rets)
barChart(Hindustan.rets)
barChart(Infosys.rets)
barChart(Maruti.rets)
barChart(AsianPaints.rets)

#Calculate VaR and CVaR
VaR(Bajaj.rets, p=0.95,method = "historical")
CVaR(Bajaj.rets, p=0.95,method = "historical")

VaR(Hindustan.rets, p=0.95,method = "historical")
CVaR(Hindustan.rets, p=0.95,method = "historical")

VaR(Infosys.rets, p=0.95,method = "historical")
CVaR(Infosys.rets, p=0.95,method = "historical")

VaR(Maruti.rets, p=0.95,method = "historical")
CVaR(Maruti.rets, p=0.95,method = "historical")

VaR(AsianPaints.rets, p=0.95,method = "historical")
CVaR(AsianPaints.rets, p=0.95,method = "historical")

#Bajaj: VaR  -0.02092848  --  20%
#HUL :VaR    -0.0171488   --  30%
#Infy:VaR    -0.02153898  --  10%
#Maruti:VaR  -0.02273213  --  10%
#AP:VaR      -0.01768905  --  30%


#Create your Portfolio
tickers <- c("Bajaj","HUL","Infy","Maruti","AsianPaints")
weights <- c(0.20,0.25,0.25,0.10,0.20)
getSymbols(tickers, from=maxDate)

Port.prices <- na.omit(merge(Bajaj.prices,Hindustan.prices,Infosys.prices,Maruti.prices,AsianPaints.prices))
Port.returns <- ROC(Port.prices, type = "discrete")[-1,]
Port.returns

#Give column names to your portfolio
colnames(Port.returns) <- tickers

#Calculate Portfolio VaR and CVaR of your Portfolio
VaR(Port.returns,p=0.95,weights=weights,portfolio_method = "component", method = "historical")
CVaR(Port.returns,p=0.95,weights=weights,portfolio_method = "component", method = "historical")

VaR(Port.returns,p=0.95,weights=weights,portfolio_method = "component", method = "gaussian")
CVaR(Port.returns,p=0.95,weights=weights,portfolio_method = "component", method = "gaussian")

VaR(Port.returns,p=0.95,weights=weights,portfolio_method = "component", method = "modified")
CVaR(Port.returns,p=0.95,weights=weights,portfolio_method = "component", method = "modified")

#Calculate Individual stocks VaR by different methods
VaR.Hist <- VaR(Port.returns, p=0.95, weights = NULL, portfolio_method = "single",method = "historical")
VaR.Gaus <- VaR(Port.returns, p=0.95, weights = NULL, portfolio_method = "single",method = "gaussian")
VaR.Mod <- VaR(Port.returns, p=0.95, weights = NULL, portfolio_method = "single",method = "modified")

All.VaR <- abs(data.frame(rbind(VaR.Hist,VaR.Gaus,VaR.Mod)))
rownames(All.VaR) <- c("Hist","Gaus","Mod")

#Calculate Portfolio VaR by different methods
PortVaR.Hist <- as.data.frame(VaR(Port.returns,p=0.95,weights=weights, portfolio_method = "component", method = "historical"))[1,1]
PortVaR.Gaus <- VaR(Port.returns,p=0.95,weights=weights, portfolio_method = "component", method = "gaussian")$VaR
PortVaR.Mod <- VaR(Port.returns,p=0.95,weights=weights, portfolio_method = "component", method = "modified")$MVaR


#Combine Individual Stocks and Portfolio VaR
All.VaR$Portfolio <- 0
All.VaR$Portfolio <- c(PortVaR.Hist,PortVaR.Gaus,PortVaR.Mod)
All.VaR$Type <- c("Hist","Gaus","Mod")

#Visualization of VaR calculated for individual stocks and Portfolio
install.packages("readshape2")
library(reshape2)
library(ggplot2)

plotVaR <- melt(All.VaR, variable.name = "Ticker", value.name = "VaR")
ggplot(plotVaR,aes(x=Type, y=VaR, fill=Ticker)) + geom_bar(stat = "identity", position = "dodge")

