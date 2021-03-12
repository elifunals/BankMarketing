library(dplyr)
library(tidyverse)
library(Hmisc)
library(arules)
library(arulesViz)
library(cluster)
library(fpc)

bankData<-read.csv('bank-additional-full.csv',sep=';',stringsAsFactors = T)
testData<-read.csv('bank-additional-full.csv',sep=';',stringsAsFactors = T)

str(bankData)
describe(bankData)
summary(bankData)

trans <- read.transactions('bank-additional-full.csv', format="basket",  sep=";", rm.duplicates=TRUE, header = TRUE)

itemFrequencyPlot(trans, topN=15, type="absolute", col="wheat2",xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

ggplot(bankData, aes(x = job, y = education)) +geom_count(color = "navy")+ xlab("Job") + ylab("Education")
ggplot(bankData, aes(x = age, y = education)) +geom_count(color = "red")+ xlab("Age") + ylab("Education")
ggplot(bankData, aes(x = y, y = age)) +   geom_boxplot() + 
  xlab("Target") + ylab("Age")


trans <- read.transactions('bank-additional-full.csv', format="single", cols=c("education","loan"), sep=";", rm.duplicates=TRUE, header = TRUE)

itemFrequencyPlot(trans, support = 0.01, topN=10, col="lightcyan2", cex.names = 0.7,main="Eduacation-Loan Columns Frequency Plot")

trans <- read.transactions('bank-additional-full.csv', format="single", cols=c("y","pdays"), sep=";", rm.duplicates=TRUE, header = TRUE)

itemFrequencyPlot(trans, topN=15, type="absolute", col="lightcyan2", xlab="Item name", 
                  ylab="Frequency (absolute)", main="Contact-Target Frequency Plot")

bankData$duration<-NULL

hist(bankData$pdays,xlab="Prior Contact")
prior<-unique(sort(bankData$pdays,decreasing = TRUE))

bankData$pdays <- cut(bankData$pdays, breaks = c(0,prior[2],999),labels = c("Contacted","No Contact"))
table(bankData$pdays)

numData<- select_if(bankData, is.numeric)
dbscan::kNNdistplot(numData, k =  1)

dbscan.outliers <- function(data,a, ...) 
{ 
  cl <- fpc::dbscan(data,scale=a, ...)
  posOuts <- which(cl$cluster == 0) 
  list(positions = posOuts,outliers = data[posOuts,],dbscanResults = cl) 
}
outs<-dbscan.outliers(numData,TRUE, 0.5)

bankData <- bankData[-c(outs$positions), ]
testData<-bankData

hist(bankData$age,xlab="Age Dist.")
bankData$age <- cut(bankData$age, breaks = c(15,30,45,60), labels = c("young" ,"adult", "old" ))
table(bankData$age)

hist(bankData$campaign,xlab="Number of Cont.")
contMean<-mean(bankData$campaign)
contMin<-min(bankData$campaign)
contMax<-max(bankData$campaign)
mean<-round(contMean)

bankData$campaign <- cut(bankData$campaign, breaks=c(0,contMin,mean,contMax))
table(bankData$campaign)

hist(bankData$previous,xlab="Prior Contact (times)")
bankData$previous <- factor(bankData$previous)
table(bankData$previous)

empMean<-mean(bankData$emp.var.rate)
empMin<-min(bankData$emp.var.rate)
empMax<-max(bankData$emp.var.rate)

hist(bankData$emp.var.rate,xlab="Employment Var. Rate")
bankData$emp.var.rate <- cut(bankData$emp.var.rate, breaks = c(empMin,empMean,1.2,empMax),labels=c("Bw -3.4 and 0.08","Bw 0.08 and 1.2","Bw 1.2 and 1.4"))
table(bankData$emp.var.rate)

hist(bankData$cons.price.idx,xlab="Consumer Price Ind.")
bankData$cons.price.idx <- cut(bankData$cons.price.idx, breaks = c(91,93,93.99,95))
table(bankData$cons.price.idx)

consMean<-mean(bankData$cons.conf.idx)
consMin<-min(bankData$cons.conf.idx)
consMax<-max(bankData$cons.conf.idx)
hist(bankData$cons.conf.idx,xlab="Consumer Conf. Ind.")
bankData$cons.conf.idx <- cut(bankData$cons.conf.idx, breaks =c(consMin,-42.5,consMean+4,consMax))
table(bankData$cons.conf.idx)

hist(bankData$euribor3m,xlab="Euribor Rate")
bankData$euribor3m <- cut(bankData$euribor3m, breaks = c(0,2.8,4.9,5.1))
table(bankData$euribor3m)

numMean<-mean(bankData$nr.employed)
numMin<-min(bankData$nr.employed)
numMax<-max(bankData$nr.employed)
hist(bankData$nr.employed,xlab="Number of Employees")
bankData$nr.employed <- cut(bankData$nr.employed, breaks = c(numMin,numMean,5200,numMax),labels = c("4960-5170","5170-5200","5200-5230"))
table(bankData$nr.employed)

bankData <- as(bankData, "transactions")
summary(bankData)

itemFrequencyPlot(bankData, support = 0.01,topN=15,sep=";", col="lightcyan2", cex.names = 0.7)

trans <- read.transactions('bank-additional-full.csv', format="single", cols=c("education","loan"), sep=";", rm.duplicates=TRUE, header = TRUE)

itemFrequencyPlot(trans, topN=10, type="absolute", col="lightcyan2", xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

trans <- read.transactions('bank-additional-full.csv', format="single", cols=c("y","pdays"), sep=";", rm.duplicates=TRUE, header = TRUE)

itemFrequencyPlot(trans, topN=, type="absolute", col="red", xlab="Item name", 
                  ylab="Frequency (absolute)", main="Absolute Item Frequency Plot")

supportLevels <- c(0.1, 0.05, 0.01, 0.005,0.001,0.00075)
confidenceLevels <- c(0.95, 0.85, 0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.15)

rules_sup10 <- integer(length=9)
rules_sup5 <- integer(length=9)
rules_sup1 <- integer(length=9)
rules_sup0.5 <- integer(length=9)
rules_sup0.1 <- integer(length=9)
rules_sup0.05<-integer(length=9)

for (i in 1:length(confidenceLevels)) {
  
  rules_sup10[i] <- length(apriori(bankData, parameter=list(sup=supportLevels[1],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=yes'),
                                   control=list(verbose=FALSE)))
  rules_sup5[i] <- length(apriori(bankData, parameter=list(sup=supportLevels[2],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=yes'),
                                  control=list(verbose=FALSE)))
  rules_sup1[i] <-length(apriori(bankData, parameter=list(sup=supportLevels[3],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=yes'),
                                 control=list(verbose=FALSE)))
  rules_sup0.5[i] <- length(apriori(bankData, parameter=list(sup=supportLevels[4],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=yes'),
                                    control=list(verbose=FALSE)))
  rules_sup0.1[i] <- length(apriori(bankData, parameter=list(sup=supportLevels[5],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=yes'),
                                    control=list(verbose=FALSE)))
  rules_sup0.05[i] <- length(apriori(bankData, parameter=list(sup=supportLevels[6],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=yes'),
                                     control=list(verbose=FALSE)))
  
}

num_rules <- data.frame(rules_sup10, rules_sup5, rules_sup1, rules_sup0.5,rules_sup0.1, rules_sup0.05,confidenceLevels)

ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
  geom_line(aes(y=rules_sup10, colour="Support level of 10%")) + 
  geom_point(aes(y=rules_sup10, colour="Support level of 10%")) +
  
  geom_line(aes(y=rules_sup5, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5, colour="Support level of 5%")) +
  
  geom_line(aes(y=rules_sup1, colour="Support level of 1%")) + 
  geom_point(aes(y=rules_sup1, colour="Support level of 1%")) +
  
  geom_line(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  geom_point(aes(y=rules_sup0.5, colour="Support level of 0.5%")) +
  
  geom_line(aes(y=rules_sup0.1, colour="Support level of 0.1%")) +
  geom_point(aes(y=rules_sup0.1, colour="Support level of 0.1%")) +
  
  geom_line(aes(y=rules_sup0.05, colour="Support level of 0.05%")) +
  geom_point(aes(y=rules_sup0.05, colour="Support level of 0.05%")) +
  
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())

rules <- apriori(bankData,parameter = list(sup=0.00075,conf=0.8),
                 appearance =list(default='lhs', rhs='y=yes'),
                 control=list(verbose=FALSE))

rules <- sort(rules, by='confidence', decreasing = TRUE)
inspect(rules[1:8])

subrules <- head(rules,8)
plot(subrules, method='graph', interactive=FALSE)

rules <- sort(rules, by='support', decreasing=TRUE)
inspect(rules[1:8])

subrules <- head(rules,8)
plot(subrules, method='graph', interactive=FALSE)

rules <- sort(rules, by='lift', decreasing=TRUE)
inspect(rules[1:8])

subrules <- head(rules,8)
plot(subrules, method='graph', interactive=FALSE)

plot(subrules, method="graph")
plot(subrules, method="graph", control=list(layout=igraph::in_circle()))

###For y="no"


rules_sup10N <- integer(length=9)
rules_sup5N <- integer(length=9)
rules_sup1N <- integer(length=9)
rules_sup0.5N <- integer(length=9)
rules_sup0.1N <- integer(length=9)
rules_sup0.05N<-integer(length=9)

for (i in 1:length(confidenceLevels)) {
  
  rules_sup10N[i] <- length(apriori(bankData, parameter=list(sup=supportLevels[1],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=no'),
                                    control=list(verbose=FALSE)))
  rules_sup5N[i] <- length(apriori(bankData, parameter=list(sup=supportLevels[2],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=no'),
                                   control=list(verbose=FALSE)))
  rules_sup1N[i] <-length(apriori(bankData, parameter=list(sup=supportLevels[3],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=no'),
                                  control=list(verbose=FALSE)))
  rules_sup0.5N[i] <- length(apriori(bankData, parameter=list(sup=supportLevels[4],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=no'),
                                     control=list(verbose=FALSE)))
  rules_sup0.1N[i] <- length(apriori(bankData, parameter=list(sup=supportLevels[5],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=no'),
                                     control=list(verbose=FALSE)))
  rules_sup0.05N[i] <- length(apriori(bankData, parameter=list(sup=supportLevels[6],conf=confidenceLevels[i]),appearance =list(default='lhs', rhs='y=no'),
                                      control=list(verbose=FALSE)))
  
}

num_rulesN <- data.frame(rules_sup10N, rules_sup5N, rules_sup1N, rules_sup0.5N,rules_sup0.1N, rules_sup0.05N,confidenceLevels)

ggplot(data=num_rulesN, aes(x=confidenceLevels)) +
  
  geom_line(aes(y=rules_sup10N, colour="Support level of 10%")) + 
  geom_point(aes(y=rules_sup10N, colour="Support level of 10%")) +
  
  geom_line(aes(y=rules_sup5N, colour="Support level of 5%")) +
  geom_point(aes(y=rules_sup5N, colour="Support level of 5%")) +
  
  geom_line(aes(y=rules_sup1N, colour="Support level of 1%")) + 
  geom_point(aes(y=rules_sup1N, colour="Support level of 1%")) +
  
  geom_line(aes(y=rules_sup0.5N, colour="Support level of 0.5%")) +
  geom_point(aes(y=rules_sup0.5N, colour="Support level of 0.5%")) +
  
  geom_line(aes(y=rules_sup0.1N, colour="Support level of 0.1%")) +
  geom_point(aes(y=rules_sup0.1N, colour="Support level of 0.1%")) +
  
  geom_line(aes(y=rules_sup0.05N, colour="Support level of 0.05%")) +
  geom_point(aes(y=rules_sup0.05N, colour="Support level of 0.05%")) +
  
  
  labs(x="Confidence levels", y="Number of rules found", 
       title="Apriori algorithm with different support levels") +
  theme_bw() +
  theme(legend.title=element_blank())


rulesN <- apriori(bankData,parameter = list(sup=0.01,conf=0.8),
                  appearance =list(default='lhs', rhs='y=no'),
                  control=list(verbose=FALSE))


rulesN <- sort(rulesN, by='confidence', decreasing = TRUE)
inspect(rulesN[1:8])

subrulesN <- head(rulesN,8)
plot(subrulesN, method='graph', interactive=FALSE)

rules <- sort(rulesN, by='support', decreasing=TRUE)
inspect(rulesN[1:8])

subrulesN <- head(rulesN,8)
plot(subrulesN, method='graph', interactive=FALSE)

rules <- sort(rulesN, by='lift', decreasing=TRUE)
inspect(rulesN[1:8])

subrulesN <- head(rulesN,8)
plot(subrulesN, method='graph', interactive=FALSE)

plot(subrulesN, method="graph")
plot(subrulesN, method="graph", control=list(layout=igraph::in_circle())) 

