library(XML)
library(RCurl)
avalanche.url <- "https://utahavalanchecenter.org/observations?page="
all.pages<-0:202
avalanche<-NULL

for(page in all.pages){
  this.url<-paste(avalanche.url,page,sep="")
  this.webpage<-htmlParse(getURL(this.url))
  thispage.avalanche <- readHTMLTable(this.webpage,which=1,header=TRUE,
                                      stringsAsFactors=FALSE)
  avalanche<-rbind(avalanche,thispage.avalanche)
}



avalanche1 = subset(avalanche, Region == "Salt Lake")



avalanche1$Date <- as.character(avalanche1$Date)
avalanche1$my.month = as.numeric(unlist(strsplit(avalanche1$Date,"/"))[seq(1,3*length(avalanche1$Date), by = 3)])
avalanche1$my.year = as.numeric(unlist(strsplit(avalanche1$Date,"/"))[seq(3,3*length(avalanche1$Date), by = 3)])

avalanche2 = NULL
for (i in 2000:2019) {
  for (j in c(1:3,12)) {
    tally = dim(subset(avalanche1, my.year == i &
                         my.month == j))[1]
    avalanche2 = rbind(avalanche2, c(i,j,tally))
  }
}

avalanche2 = data.frame(avalanche2)
names(avalanche2) = c("my.year", "my.month", "Avalanches")

avalanche3 <- read.csv(file="1678425.csv", header=TRUE, sep=",")

avalanche3$DATE <- as.character(avalanche3$DATE)
avalanche3$my.month = as.numeric(unlist(strsplit(avalanche3$DATE,"-"))[seq(2,2*length(avalanche3$DATE), by = 2)])
avalanche3$my.year = as.numeric(unlist(strsplit(avalanche3$DATE,"-"))[seq(1,2*length(avalanche3$DATE), by = 2)])


merged_data = merge(avalanche3,avalanche2, by = c("my.month", "my.year"))






#EDA
#monthly snowfall
plot(Avalanches~SNOW, data = merged_data)
#number of days with below 32 degrees
plot(Avalanches~DT32, data = merged_data)


#Analysis
#response variable # of avalanches
#explanatory variables:
# SNOW = inches of snow
# DT32 = days temp below 32
# TMIN = low temp for month (degress)

#Model
# Avalanches ~ Poisson( exp(beta0 + beta2 SNOW + beta3 DT32 + beta4 TWIN))

avalanche.out = glm(Avalanches ~ SNOW + DT32 + TMIN, data = merged_data, family="poisson")
summary(avalanche.out)
exp(coef(avalanche.out)[-1])

# 95% CI
exp(confint(avalanche.out)[-1,])


#For each additional inch of snow we estimate the mean # of avalanches 
#increases by 1%(95% CI: .43%, 1.50%) holding all else constant



#H0: beta1 = 0
# Ha: beta1 != 0

#Test_statistics : 3.572
# pvalue = 0.0004
# reject the null hypothesis



#effective of temperature
#temperature has two variables
#h0: beta 2 =beta3 = 0
# Ha: at least one of them is not zero
avalanche.red = glm(Avalanches ~ SNOW, data = merged_data, family="poisson")
anova(avalanche.red,avalanche.out, test = "Chisq")


# test statistics 55.577
# pvalue < 0.0001



#Typical December
summary(subset(merged_data, my.month==12))
exp(predict(avalanche.out, newdat = data.frame(DT32 = 28.00, SNOW = 12.90, TMIN = 22.90, Avalanches= 17.00)))

logmu.hat = predict(avalanche.out, newdat = data.frame(DT32 = 28.00, SNOW = 12.90, TMIN = 22.90, Avalanches= 17.00), se = TRUE)

logmu.L = logmu.hat$fit - 1.96*logmu.hat$se.fit
logmu.U = logmu.hat$fit + 1.96*logmu.hat$se.fit
exp(logmu.L)
exp(logmu.U)


#Typical Janurary
summary(subset(merged_data, my.month==1))
exp(predict(avalanche.out, newdat = data.frame(DT32 = 28.50, SNOW = 9.75, TMIN = 22.45, Avalanches= 13.00)))

logmu.hat1 = predict(avalanche.out, newdat = data.frame(DT32 = 28.50, SNOW = 9.75, TMIN = 22.45, Avalanches= 13.00), se = TRUE)

logmu.L1 = logmu.hat1$fit - 1.96*logmu.hat1$se.fit
logmu.U1 = logmu.hat1$fit + 1.96*logmu.hat1$se.fit
exp(logmu.L1)
exp(logmu.U1)

#Typical Feburary
summary(subset(merged_data, my.month==2))
exp(predict(avalanche.out, newdat = data.frame(DT32 = 22.00, SNOW = 10.00, TMIN = 27.45, Avalanches= 19.5)))

logmu.hat2 = predict(avalanche.out, newdat = data.frame(DT32 = 22.00, SNOW = 10.00, TMIN = 27.45, Avalanches= 19.5), se = TRUE)

logmu.L2 = logmu.hat2$fit - 1.96*logmu.hat2$se.fit
logmu.U2 = logmu.hat2$fit + 1.96*logmu.hat2$se.fit
exp(logmu.L2)
exp(logmu.U2)


#Typical March
summary(subset(merged_data, my.month==3))
exp(predict(avalanche.out, newdat = data.frame(DT32 = 11.00, SNOW = 5.30, TMIN = 34.80, Avalanches= 1.00)))

logmu.hat3 = predict(avalanche.out, newdat = data.frame(DT32 = 11.00, SNOW = 5.30, TMIN = 34.80, Avalanches= 1.00), se = TRUE)

logmu.L3 = logmu.hat3$fit - 1.96*logmu.hat3$se.fit
logmu.U3 = logmu.hat3$fit + 1.96*logmu.hat3$se.fit
exp(logmu.L3)
exp(logmu.U3)



speech = read.csv(header=TRUE, text ="
                  wcnt year    budget inflate thanksP  man   woman  thanksM  thanksW  time
                  212 1942   1344000   16.06  3       101     452        1        2   108
                  119 1946   2100000   13.85  1        56     218        2        1   101
                  176 1947   2000000   11.73  5        96     220        1        1   172
                  50 1949         0   10.51  4        29      31        3        1   118
                  34 1950   1400000   10.73  4       208      46        3        1   110
                  31 1951   2723903    9.93  3        73      43        1        1   138
                  156 1952   4000000    9.51  3       159     100        0        4   113
                  97 1953   1650000    9.48  3         4      33        2        1    93
                  46 1954    910000    9.37  1        64      33        1        2   118
                  70 1955    343000    9.44  1        61      71        4        1   108
                  35 1956   6000000    9.41  2        22     132        1        3    90
                  91 1957   3000000    9.14  1        79      41        2        3   188
                  20 1958   3319355    8.82  1        36      39        2        4   161
                  81 1959  15900000    8.69  1       131      78        3        4   115
                  70 1960   3000000    8.61  1        76      30        3        2   125
                  125 1961   6000000    8.46  2       104      71        1        0   130
                  90 1962  15000000    8.40  2        74      28        5        1   150
                  64 1963   1000000    8.29  1        52      55        1        3   128
                  159 1964  17000000    8.16  6        81      97        2        6   170
                  69 1965   8200000    8.08  4        46      24        4        2   174
                  4 1966   2000000    7.93  1        62      36        1        2   151
                  99 1967   2000000    7.66  3       120      44       11        2   110
                  62 1968  10000000    7.39  2        44      50        2        1   153
                  37 1969   3600000    7.08  3       127      74        3        2   145
                  51 1970  12000000    6.67  5        44      41        0        2   172
                  66 1971   1800000    6.34  2       143      41        5        4   104
                  217 1972   6000000    6.13  2       141      58        1        4   158
                  127 1973   5500000    5.92  4       240     119        3        5   203
                  73 1974  13000000    5.41  7        59      57        3        4   200
                  236 1975   4400000    4.84  3       106     131        3        3   192
                  125 1976    960000    4.53  5       193      82        7        4   218
                  216 1977   4000000    4.31  3        77      60        1        3   210
                  68 1978  15000000    4.03  5       317     367        8       11   215
                  208 1979   8000000    3.69  1       362     287        4        3   192
                  162 1980   6000000    3.24  5       240     137        3        2   193
                  188 1981   5500000    2.90  4       590       0        6        0   204
                  427 1982  22000000    2.67  1       123     231        1        6   195
                  192 1983   8000000    2.58  2       265     359        3        3   222
                  248 1984  18000000    2.47  4       127     144        1        2   190
                  48 1985  31000000    2.39  3        55     119        2        5   182
                  279 1986   6000000    2.30  5        97     104        1        5   199
                  118 1987  23000000   2.27   4       316     184        8        5   213
                  207 1988  25000000    2.18  5       326     140       11        3   199
                  213 1989   7500000    2.08  9       111     100        1        2   217
                  258 1990  22000000    1.98  3       126     189        8        9   215
                  236 1991  19000000    1.87  7       159     278        3        9   213
                  123 1992  14400000    1.83  5       472     185       11        3   210
                  282 1993  22000000    1.77  8       414     264        0        5   198
                  423 1994  55000000    1.72  9       228     201        3        3   215
                  145 1995  72000000    1.68  9       184     317        4       12   218
                  243 1996  27000000    1.63  6       226     200        5        1   214
                  594 1997 200000000    1.58  5       193     271        3        6   227
                  386 1998  25000000    1.56  8       198     363        7       11   242
                  321 1999  15000000    1.53  9       260     385        7        9   249
                  314 2000 103000000    1.49  10      253     396        4        5   203
                  378 2001  58000000    1.44  11      302     528        4       32   263
                  232 2002  45000000    1.42  2       462     234       10        2   210
                  436 2003  94000000    1.39  4       139     287        3       15   224
                  265 2004  30000000    1.36  6       490     354       15       11   194
                  193 2005   6500000    1.32  12      208     436        8       11   213
                  257 2006  90000000    1.27  8       297     192        8        6   231
                  181 2007  25000000    1.25  6       199      72        6        6   201
                  241 2008  15000000    1.19  5       300     328        4        4   210
                  271 2009  15000000    1.19  8       302     468       12       11   217
                  273 2010  15000000    1.16  9       319     361        2        6   195
                  263 2011  15000000    1.14  8       122     270        7       11   194
                  634 2012  44500000    1.11  22      254     118        2        7   215
                  380 2013  20000000    1.09  14      549     513       12       11   214
                  431 2014  18000000    1.08  10      195     324        5        8   223
                  148 2015  20000000    1.08  4       402     178       10       10   217
                  283 2016   1500000    1.06  9       218     294        4        9   229
                  213 2017  19400000    1.04  4       293     264        8        3   233
                  ",sep = "")




award = NULL
for (val in c(1:72))
{
  q = speech[val,]
  p = data.frame(year = q$year, words = q$wcnt, thanks = q$thanksP,type = "BestPicture")
  m = data.frame(year = q$year, words = q$man, thanks = q$thanksM,type = "Actor")
  w = data.frame(year = q$year, words = q$woman, thanks = q$thanksW,type = "Actress")
  award = rbind(data,p,m,w)
}
#Model
# thanks ~ Poisson( exp(beta0 + beta1 year + beta2 words + beta3 Actor + beta4 Actress))

award.out = glm(thanks ~ year + words + type, data = award, family="poisson")
summary(award.out)

(coef(award.out)[-1])

# 95% CI
exp(confint(award.out)[-1,])


award.red = glm(thanks ~ year + words, data = award, family="poisson")
anova(award.out,award.red, test ="Chisq")
