library(ggplot2)
#read_data
loan <- read.csv('loan_data.csv')
head(loan)
str(loan)
summary(loan)

#convert columns to factor 
loan$inq.last.6mths <- as.factor(loan$inq.last.6mths)
loan$delinq.2yrs <- as.factor(loan$delinq.2yrs)
loan$pub.rec <- as.factor(loan$pub.rec)
loan$not.fully.paid <- as.factor(loan$not.fully.paid)
loan$credit.policy <- as.factor(loan$credit.policy)
str(loan)

#EDA
#hist for not.full.paid
ggplot(loan, aes(fico, fill = not.fully.paid)) + geom_histogram(color = 'black')

#barplot for purpose columns 
ggplot(loan, aes(purpose, fill = not.fully.paid)) + geom_bar(position='dodge') + theme(axis.text.x = element_text(angle = 90))

#scatteerplot of fico score versus int.rate 
ggplot(loan,aes(int.rate, fico)) + geom_point()
ggplot(loan,aes(int.rate, fico, color = not.fully.paid)) + geom_point()

