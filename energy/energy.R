# US residential energy consumption
# http://www.eia.gov/totalenergy/data/monthly/index.cfm#consumption
data1<-read.csv("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T02.01")
# subset to TERCBUS Total Energy Consumed by the Residential Sector
data2<-subset(data1,MSN=="TERCBUS")
# subset to "your lifetime"
data3<-subset(data2,data2$YYYYMM>199100)
# remove yearly total (coded "month 13", every 13th obs)
data4<-subset(data3,data3$YYYYMM%%100 != 13)
energy<-data4$Value

#EDA
plot(Value~YYYYMM,data = data4, type ="b", 
     ylab = "US Residential Energy (trillion Btu)",
     xlab = "Month")
#Notice that in addition to an increasing trend, there is also a seasonal trend(12 month cycle)

#ANALYSIS

#Additive (see no evidenc eof curvature)

#constant mean change (see no evidence of chnaging trend)

#Model: Seasonal ARIMA(1,1,1)(1,1,1)_12
out.energy = sarima(energy, 1,1,1,1,1,1,12)
#Forcast next 2 years
future.energy = sarima.for(energy, n.ahead=24, 1,1,1,1,1,1,12)


