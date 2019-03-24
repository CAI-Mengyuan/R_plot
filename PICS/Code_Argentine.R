rm(list = ls()) # clear environment
library(readxl)
library(reshape2)
library(ggplot2)
library(zoo)
library(scales)
library(plyr)
library(lattice)
library(Rmisc)

############### import data ################
AREMOS01 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/ARGENTINA/AREMOS01.xls")
AREMOS02 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/ARGENTINA/AREMOS02.xls", 
                       col_types = c("numeric", "numeric"))
AREMOS03 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/ARGENTINA/AREMOS03.xls")
AREMOS04 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/ARGENTINA/AREMOS04.xls")
AREMOS05 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/ARGENTINA/AREMOS05.xls")
AREMOS07 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/ARGENTINA/AREMOS07.xls")


############### change format to Date ################
AREMOS01$DATE <- as.Date(as.yearqtr(AREMOS01$DATE, format= "%Y Q%q"))
AREMOS01 <- AREMOS01[9:36,]
AREMOS02$DATE <- as.character(AREMOS02$DATE) 
AREMOS02$DATE <- as.Date(as.yearmon(AREMOS02$DATE, format= "%Y%m")) 
AREMOS02 <- AREMOS02[25:108,]
AREMOS03$DATE <- as.character(AREMOS03$DATE) 
AREMOS03$DATE <- as.Date(as.yearmon(AREMOS03$DATE, format= "%Y%m"))
AREMOS03 <- AREMOS03[25:108,]
AREMOS04$DATE <- as.character(AREMOS04$DATE)
AREMOS04$DATE <- as.Date(as.yearmon(AREMOS04$DATE, format= "%Y%m")) 
AREMOS04 <- AREMOS04[25:108,]
AREMOS05$DATE <- as.Date(as.yearqtr(AREMOS05$DATE, format= "%Y Q%q")) 
AREMOS05 <- AREMOS05[9:36,]
AREMOS07$DATE <- as.character(AREMOS07$DATE) 
AREMOS07$DATE <- as.Date(as.yearmon(AREMOS07$DATE, format= "%Y%m")) 
AREMOS07 <- AREMOS07[25:108,]

############### plot ################
p1 <- ggplot(AREMOS01,aes(x=DATE, y=RealGDPGrowth)) + geom_line() + geom_point(size=0.6)+ theme_bw() + # basic draw 
  scale_x_date(breaks="1 year", labels=date_format("%Y"), limits =c(as.Date("1998-01-01"), as.Date("2004-11-01"))) + # set xlabel
  xlab(" ") + ylab(" ") + #set the range of year and remove labels and 
  ggtitle("Real GDP Growth (%)") + theme(plot.title = element_text(hjust = 0.5, size = 12)) # set title


p2 <- ggplot(AREMOS02,aes(x=DATE, y=GovernmentBondYield)) + geom_line() + geom_point(size=0.6)+ theme_bw() + # basic draw and connecting across missing value
  scale_x_date(breaks="1 year", labels=date_format("%Y"), limits =c(as.Date("1998-01-01"), as.Date("2004-12-01"))) + # set xlabel
  xlab(" ") + ylab(" ") + ggtitle("Market Rate (%)") + theme(plot.title = element_text(hjust = 0.5, size = 12)) # remove labels and set title


p3 <- ggplot(AREMOS03,aes(x=DATE, y=ForeignReserves)) + geom_line() + geom_point(size=0.6)+ theme_bw() + # basic draw and connecting across missing value
  scale_x_date(breaks="1 year", labels=date_format("%Y")) + # set xlabel
  xlab(" ") + ylab(" ") + ggtitle("Foreign Reserves (Millions of U.S. dollar)") + theme(plot.title = element_text(hjust = 0.5, size = 12)) # remove labels and set title


p4 <- ggplot(AREMOS04,aes(x=DATE, y=ExchangRate)) + geom_line() + geom_point(size=0.6)+ theme_bw() + # basic draw and connecting across missing value
  scale_x_date(breaks="1 year", labels=date_format("%Y")) + # set xlabel
  xlab(" ") + ylab(" ") + ggtitle("Exchang Rate (Argentina Pesos per U.S. dollar)") + theme(plot.title = element_text(hjust = 0.5, size = 12)) # remove labels and set title


p5 <- ggplot(AREMOS05,aes(x=DATE, y=CurrentAccount)) + geom_line() + geom_point(size=0.6)+ theme_bw() + # basic draw and connecting across missing value
  scale_x_date(breaks="1 year", labels=date_format("%Y")) + # set xlabel
  xlab(" ") + ylab(" ") + ggtitle("Current Account (% of GDP)") + theme(plot.title = element_text(hjust = 0.5, size = 12)) # remove labels and set title


copy <- AREMOS07 # make a copy
copyl <- melt(copy, id.vars = c("DATE")) #turn wide-format into long-format
p7 <- ggplot(copyl,aes(x=DATE, y= value))	+ geom_line(aes(linetype=variable)) + # basic draw
  scale_x_date(breaks="1 year", labels=date_format("%Y")) +
  theme_bw() + theme(legend.position=c(0,1), legend.justification=c(0,1)) + theme(legend.background = element_rect(fill = 'white', colour = 'black')) + # change the legend
  scale_linetype_discrete(labels = c('Demand Deposits','Time, Saving, & Foreign Currency Deposits','Sum')) + # change the labels
  theme(legend.title = element_text(size = 0)) +   # remove legend title
  theme(legend.text = element_text(size = 6)) +   # set the size of legend
  xlab(" ") + ylab(" ") + ggtitle("Deposits of Banks (Billions of Argentina Pesos)") + theme(plot.title = element_text(hjust = 0.5, size = 11))  # set the title and center it



p <- multiplot(p1,p2,p3,p4,p5,p7, cols = 3)
p
