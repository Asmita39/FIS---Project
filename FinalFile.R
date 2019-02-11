# Market Index, #Portfolio, #Bonds, #Options




######## Market Index ###########

#Calculate Return


########Portfolio Index#####################

library(quantmod)
library(PerformanceAnalytics)

data.VISA <- getSymbols("V", from = "2014-12-31", to = "2016-12-31", auto.assign = FALSE)
data.VISA <- to.monthly(data.VISA)
ret.VISA <- Return.calculate(data.VISA$data.VISA.Adjusted)


data.UNH <- getSymbols("UNH", from = "2014-12-31", to = "2016-12-31", auto.assign = FALSE)
data.UNH <- to.monthly(data.UNH)
ret.UNH <- Return.calculate(data.UNH$data.UNH.Adjusted)



portfolio <- cbind(ret.VISA, ret.UNH)
portfolio <- portfolio[-1,]
portfolio.ret <- Return.portfolio(portfolio, weights = c(.5,0.5), rebalance_on = "months" )

port.csv <- cbind(index(portfolio.ret), data.frame(portfolio.ret))
names(port.csv) <- c("date", "port.ret")
row.names(port.csv) <- seq(1, nrow(port.csv), by = 1)
write.csv(port.csv, "C:/users/tchan/Desktop/FIS/Project/Hypothetical_Portfolio2(Monthly).csv", row.names = F)




########Bonds##########################
bondprc<-function(coupon,maturity,yield,par,coupon.freq){
  periods=maturity*coupon.freq
  coupon.period=coupon/coupon.freq
  yield.period=yield/coupon.freq
  bond.coupon<-rep(coupon.period,times=periods,length.out=NA,each=1)
  bond.df<-as.data.frame(bond.coupon)
  for (i in 1:periods) {
    bond.df$cf[i]=par*coupon.period
    bond.df$period[i]=i
    bond.df$yield[i]=yield.period
  }
  bond.df$cf[periods]=bond.df$cf[periods]+par
  bond.df$PV=bond.df$cf/((1+bond.df$yield)^bond.df$period)
  value=sum(bond.df$PV)
  value
}

#Calculating the value of a bond
coupon=0.0425
maturity=4
yield=0.017
par=1000
coupon.freq=1

price <- bondprc(coupon,maturity,yield,par,coupon.freq)
price


