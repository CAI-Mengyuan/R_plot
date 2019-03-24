rm(list = ls()) # clear environment

library(reshape2)
library(ggplot2)
library(zoo)
library(scales)
library(plyr)
library(lattice)
library(Rmisc)

############### import data ################
X1 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/G/1.xlsx")
X2 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/G/2.xlsx")
X3 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/G/3.xlsx")
X4 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/G/4.xlsx")

############### change format to Date ################
X1$DATE <- as.character(X1$DATE)
X1$DATE <- as.Date(as.yearmon(X1$DATE, format= "%Y")) 
X1 <- melt(X1,id.vars = "DATE")
X2$DATE <- as.character(X2$DATE) 
X2$DATE <- as.Date(as.yearmon(X2$DATE, format= "%Y%m")) 
X2 <- X2[25:108,]
X3$DATE <- as.character(X3$DATE) 
X3$DATE <- as.Date(as.yearmon(X3$DATE, format= "%Y%m")) 
X3 <- X3[25:108,]
X4$DATE <- as.character(X4$DATE) 
X4$DATE <- as.Date(as.yearmon(X4$DATE, format= "%Y")) 
X4 <- X4[3:9,]
X4 <- melt(X4,id.vars = "DATE") # turn the wide to the long


############### plot ################
p1 <- ggplot(X1,aes(x=DATE, y=value, linetype=variable)) + geom_line() + geom_point(size=0.9)+ theme_bw() + # basic draw 
  scale_x_date(breaks="1 year", labels=date_format("%Y")) + # set xlabel
  xlab(" ") + ylab(" ") + #set the range of year and remove labels and 
  ggtitle("Real GDP Growth (%)") + theme(plot.title = element_text(hjust = 0.5, size = 13)) + # set title
  theme(legend.position=c(1,0), legend.justification=c(1,0)) + theme(legend.background = element_rect(fill = 'white', colour = 'black')) + # change the legend
  theme(legend.title = element_text(size = 0)) +   # remove legend title
  theme(legend.text = element_text(size = 8))    # set the size of legend

p2 <- ggplot(X2,aes(x=DATE, y=ForeignReserves)) + geom_line() + geom_point(size=0.5)+ theme_bw() + # basic draw and connecting across missing value
  scale_x_date(breaks="1 year", labels=date_format("%Y")) + # set xlabel
  xlab(" ") + ylab(" ") + ggtitle("Foreign Reserves (Millions of U.S. Dollar)") + theme(plot.title = element_text(hjust = 0.5, size = 13)) # remove labels and set title


p3 <- ggplot(X3,aes(x=DATE, y=ExchangeRate)) + geom_line() + geom_point(size=0.5)+ theme_bw() + # basic draw and connecting across missing value
  scale_x_date(breaks="1 year", labels=date_format("%Y")) + # set xlabel
  xlab(" ") + ylab(" ") + ggtitle("Exchang Rate (U.S. Cents per Gold Reichsmark)") + theme(plot.title = element_text(hjust = 0.5, size = 13)) # remove labels and set title



p4 <- ggplot(X4,aes(x=DATE, y=value, linetype=variable)) + geom_line() + geom_point(size=0.9)+ theme_bw() + # basic draw 
  scale_x_date(breaks="1 year", labels=date_format("%Y")) + # set xlabel
  xlab(" ") + ylab(" ") + #set the range of year and remove labels and 
  ggtitle("Current Account (% of GDP)") + theme(plot.title = element_text(hjust = 0.5, size = 13)) + # set title
  theme(legend.position=c(1,0), legend.justification=c(1,0)) + theme(legend.background = element_rect(fill = 'white', colour = 'black')) + # change the legend
  scale_linetype_discrete(labels = c('Balance of current account','Balance of current account(Exclude reparations)')) + # change the labels
  theme(legend.title = element_text(size = 0)) +   # remove legend title
  theme(legend.text = element_text(size = 8))    # set the size of legend


p <- multiplot(p1,p2,p4,p3, cols = 2)
