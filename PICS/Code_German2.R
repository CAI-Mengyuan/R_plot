rm(list = ls()) # clear environment

library(reshape2)
library(ggplot2)
library(zoo)
library(scales)
library(plyr)
library(lattice)
library(Rmisc)
library(readxl)

############### import data ################
X5 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/G/5.xlsx")
X6 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/G/6.xlsx")
X7 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/G/7.xlsx")
X8 <- read_excel("~/Documents/WP.GERMAN.1931.CRISIS/DATA/G/8.xlsx")

############### change format to Date ################
X5$DATE <- as.character(X5$DATE)
X5$DATE <- as.Date(as.yearmon(X5$DATE, format= "%Y"))
X5 <- X5[3:9,]
X6$DATE <- as.Date(as.yearqtr(X6$DATE, format= "%Y Q%q")) 
X6 <- X6[9:36,]
X7$DATE <- as.character(X7$DATE)
X7$DATE <- as.Date(as.yearmon(X7$DATE, format= "%Y")) 
X7 <- X7[3:9,]
X8$DATE <- as.character(X8$DATE) 
X8$DATE <- as.Date(as.yearmon(X8$DATE, format= "%Y%m")) 
X8 <- X8[25:108,]
X7 <- melt(X7,id.vars = "DATE") # turn the wide to the long
X8 <- melt(X8,id.vars = "DATE") # turn the wide to the long
X7$variable <- sort(X7$variable, decreasing = T)

############### plot ################
p5 <- ggplot(X5,aes(x=DATE, y=DebtRatio)) + geom_line() + geom_point(size=0.5)+ theme_bw() + # basic draw and connecting across missing value
  scale_x_date(breaks="1 year", labels=date_format("%Y")) + # set xlabel
  xlab(" ") + ylab(" ") + ggtitle("Public Debt to GDP Ratio (%)") + theme(plot.title = element_text(hjust = 0.5, size = 13)) # remove labels and set title


p6 <- ggplot(X6,aes(x=DATE, y=BudgetDeficits)) + geom_line() + geom_point(size=0.5)+ theme_bw() + # basic draw and connecting across missing value
  scale_x_date(breaks="1 year", labels=date_format("%Y")) + # set xlabel
  xlab(" ") + ylab(" ") + ggtitle("Budget Deficits (Millions of Reichsmark)") + theme(plot.title = element_text(hjust = 0.5, size = 13)) # remove labels and set title

p7 <- ggplot(X7,aes(x=DATE, y=value))	+ geom_line(aes(linetype=variable)) + # basic draw
  scale_x_date(breaks="1 year", labels=date_format("%Y")) +
  theme_bw() + theme(legend.position=c(0,0.5), legend.justification=c(0,1)) + theme(legend.background = element_rect(fill = 'white', colour = 'black')) + # change the legend
  scale_linetype_discrete(labels = c('Sum','Sparkassen','Alle Banken')) + # change the labels
  theme(legend.title = element_text(size = 0)) +   # remove legend title
  theme(legend.text = element_text(size = 9)) +   # set the size of legend
  xlab(" ") + ylab(" ") + ggtitle("Deposits by Commercial Banks (Billions of Reichsmark)") + theme(plot.title = element_text(hjust = 0.5, size = 13))  # set the title and center it


p8 <- ggplot(X8,aes(x=DATE, y=value))	+ geom_line(aes(linetype=variable)) + # basic draw
  scale_x_date(breaks="1 year", labels=date_format("%Y")) +
  theme_bw() + theme(legend.position=c(0,1), legend.justification=c(0,1)) + theme(legend.background = element_rect(fill = 'white', colour = 'black')) + # change the legend
  scale_linetype_discrete(labels = c('German','US','UK')) + # change the labels
  theme(legend.title = element_text(size = 0)) +   # remove legend title
  theme(legend.text = element_text(size = 9)) +   # set the size of legend
  xlab(" ") + ylab(" ") + ggtitle("Central Bank Discount Rate (Percent per Annual)") + theme(plot.title = element_text(hjust = 0.5, size = 13))  # set the title and center it

p <- multiplot(p5,p6,p7,p8, cols = 2)