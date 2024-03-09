





library(readxl)
library(lubridate)
library(dplyr)
library(circular)
library(ggplot2)
library(systemfit)
library(car)



ST00_ps <- read_excel(path= "Screen Time.xlsx",
                      col_types = c("date" , "text" , 
                                    "numeric", "text", "numeric", "numeric", "date"))



# Variables used in the regression
ST00_ps$lag_TST <- lag(ST00_ps$Total.ST.min,n=1)
ST00_ps$lag_SST <- lag(ST00_ps$Social.ST.min,n=1)
ST00_ps$Date <- as.Date(ST00_ps$Date, format = "%m/%d/%Y" )
ST00_ps$weekday <- weekdays(ST00_ps$Date , abbreviate = T)
ST00_ps <- ST00_ps %>% mutate ( if_weekend = weekday %in% c("Sun", "Sat"))
ST00_ps$Xt <- ifelse(ST00_ps$if_weekend==T,1,0)
ST00_ps$Zt <- ifelse( ST00_ps$Date < ST00_ps$Date[4],0,1)

eqTST <- Total.ST.min ~ lag_TST + Xt + Zt
eqSST <- Social.ST.min ~ lag_SST + Xt + Zt
system <- list( TST = eqTST, SST = eqSST )



fitsur <- systemfit( system, "SUR", data = ST00_ps, maxit = 100 )
print( fitsur )


summary(fitsur)


linearHypothesis(fitsur,c("TST_Zt=0","SST_Zt=0"))