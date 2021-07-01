library(ggplot2)
library(caTools)
library(e1071)

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
ggplot(loan, aes(fico)) + geom_histogram(aes(fill=not.fully.paid), color='black',bins=40,alpha=0.5) + scale_fill_manual(values = c('green','red')) + theme_bw()


#barplot for purpose columns 
ggplot(loan, aes(factor(purpose))) + geom_bar(aes(fill = not.fully.paid), position='dodge') + theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#scatteerplot of fico score versus int.rate 
ggplot(loan,aes(int.rate, fico)) + geom_point() + theme_bw()
ggplot(loan,aes(int.rate,fico)) + geom_point(aes(color=not.fully.paid),alpha=0.3) + theme_bw()

#split data into traind and test 
set.seed(101)
split = sample.split(loan$not.fully.paid, SplitRatio = 0.70)
train = subset(loan, split == TRUE)
test = subset(loan, split == FALSE)

#apply svm() function on train model
model <- svm(not.fully.paid ~., data = train)
summary(model)

#predict new value from the test set
predicted.values <- predict(model,test[1:13])
table(predicted.values,test$not.fully.paid)

#Using the tune() function to test out different cost and gamma values
tune.results <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',
                     ranges=list(cost=c(1,10), gamma=c(0.1,1)))

#predict Values after tuning and found cost and gamma
model <- svm(not.fully.paid ~ .,data=train,cost=10,gamma = 0.1)
predicted.values <- predict(model,test[1:13])
table(predicted.values, test$not.fully.paid)
