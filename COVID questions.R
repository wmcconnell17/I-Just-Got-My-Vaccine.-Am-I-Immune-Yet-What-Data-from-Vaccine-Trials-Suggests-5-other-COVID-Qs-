library(dplyr)
library(ggplot2)


########### When am I immune? ##########

### data: dataframes
jnjeff <- data.frame(day = (0:18)*7, numreal = c(0,27,76,96,126,151,168,178,184,188,189,191,191,192,193,193,193,193,193),
                     numplac = c(0,22,81,168,237,299,351,387,407,416,423,425,430,432,432,432,432,432,432))

pfizer <- data.frame(day = (0:48), numreal = c(0,0,2,4,4,5,8,12,12,13,15,16.1,16.2,16.3,16.4,16.6,17.1,17,18.1,18.2,18.3,18,(18+(1:27)*4/27)),
                                          numplac = c(0,0,2,4,6,9,11,14,15,16,18,21,25,25.5,26,26.5,30,31,33,34,35,37,
                                                      38,39,40,42,44,47,48,51,54,55,56,57,58,59,61,63,65,67.5,68,69,70,71.5,73,74.5,76,78,80.5))

moderna <- data.frame(day = 0:51, numplac = c(0,0,0,0,0,0,1,1.1,1.15,1.15,1.2,1.2,1.2,1.6,1.8,1.8,1.9,2,2,2,2.2,2.2,2.3,2.3,2.4,
                                             2.4,2.5,2.8,3,3,3,3,3.1,3.1,3.3,3.3,3.3,3.4,3.6,3.7,3.9,4.2,4.4,4.7,4.9,5.15,5.34,5.5,5.75,6.0,6.1,6.2),
                     numreal = c(0,0,0,0,0,(0:25)*.4/25,.4+(0:20)*.1/20))

## chart for a given vaccine. Change the first four entries here
vaccine <- pfizer
lastr < 22
lastp <- 80.5
vaccine$final <- 95

vaccine$offreal <- c(vaccine$numreal,lastr)[-1]
vaccine$offplac <- c(vaccine$numplac,lastp)[-1]
vaccine$realslope <- vaccine$offreal-vaccine$numreal
vaccine$placslope <- vaccine$offplac-vaccine$numplac
vaccine$eff <- 100*(vaccine$placslope - vaccine$realslope)/vaccine$placslope
vaccine$eff[1] <- 0
vaccine$eff[2] <- 0
vaccine$eff[3] <- 0

vaccine <- vaccine %>% filter(eff >= 0)

ggplot(vaccine)+ 
  geom_smooth(aes(x=day,y=eff,color=2),color = "#7BBFEE",position = position_nudge(x =0)) +
  geom_point(aes(x=day,y=eff,color=2),color = "#0D49A4",position = position_nudge(x = 0)) +
  xlab("days from injection") + ylab("efficacy(%)") +
  geom_line(aes(x=day,y=final,col=day)) +
  scale_color_continuous(low="#ffffff00",high=2) +
  scale_x_continuous(breaks=((0:18)*7)) + scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90)) +
  labs(title = "Vaccine Efficacy Vs Days From First Dose",caption = "(Red line is final reported efficacy)")


########### When am I at risk? ##########
## https://docs.google.com/spreadsheets/d/1CmpTcluPpigJ0slQruk9-0m0zLz31BBd8E_wFt7cWuE/edit?usp=sharing


########### When am I infectious ##########
# import meta-analysis data and add 3 zeros
viralload <- data.frame(x = 0:25, y = c(0,0,0,4,4.5,4.5,4.5,4,3.9,3.8,3.7,
                                        3.6,3.5,3.2,2.9,2.6,2.3,2.2,2.1,2,1.9,1.8,1.7,1.6,1.5,1.5))


ggplot(viralload,aes(x=x,y=y)) + geom_point() + geom_line() + xlab("days from exposure") + ylab("viral load in log scale") +
  labs(title="COVID Viral Load From Time of Contraction",caption = "estimates based on reconciling mean time to symptom onset with Benefield et al meta-analysis, and first 48 hours not being contagious")

########### Is herd immunity possible? ##########

### Israel covid  counts

bilo <- c()
for (i in 1:11){
  bilo <- c(bilo, (46000*.8^i))
}

israelcases <- data.frame(x = c("Feb 1","Feb 8", "Feb 15", "Feb 22", "Mar 1", "Mar 8", "Mar 15", "Mar 22", "Mar 29","Apr 5","Apr 12"),
                          y = c(46000,36000,26000,25000,25000,18000,9000,4500,2400,1700,700),z = 1:11,pred = bilo)


library(forcats)
israelcases$x <- fct_inorder(israelcases$x) %>% fct_rev() %>% fct_rev()
ggplot(israelcases, aes(y=y,x=x)) + geom_point(alpha = .01) + geom_smooth(aes(x=z,y=y),position = position_nudge(x = 0)) + 
  geom_point(aes(x=x,y=pred),color=2,position = position_nudge(x = 0),alpha=.01) +
  geom_line(aes(x=z,y=pred),color=2,position = position_nudge(x = 0)) +
  xlab("") + ylab("weekly new cases")

########### Are vaccine blood clots a risk? ##########
# https://docs.google.com/spreadsheets/d/1CmpTcluPpigJ0slQruk9-0m0zLz31BBd8E_wFt7cWuE/edit?usp=sharing

########### I already had COVID. Am I immune? ##########
#  https://docs.google.com/spreadsheets/d/1CmpTcluPpigJ0slQruk9-0m0zLz31BBd8E_wFt7cWuE/edit?usp=sharing

## all other graphics done in google: https://docs.google.com/spreadsheets/d/1CmpTcluPpigJ0slQruk9-0m0zLz31BBd8E_wFt7cWuE/edit?usp=sharing

