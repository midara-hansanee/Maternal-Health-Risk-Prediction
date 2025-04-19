#Install Required packages and libraries
install.packages('caTools')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('caret')
install.packages('corrplot')

library('caTools')
library('dplyr')
library('ggplot2')
library('caret')
library('corrplot')
#Import the dataset into R
MH<-read.csv("#Your File path",header=TRUE,sep=" ,")
                 ,col.names=c("Age","SystolicBP","DiastolicBP"),"BP" ,
                  "BodyTemp","HeartRate","RiskLevel"))
#Explore the dataset
names(MH)
head(MH)
tail(MH)
Summay(MH)
str(MH)

#Remove Outliers
standard.features<- scale(MH[1:6])

#Rename the dataset
MH1<-cbind(standard.features,MH[7])
MH1
head(MH1)
summary(MH1)
#Check for null values
anyNa(MH1)

#create corrplot
#load corrplot function
corrplot(cor(MH1[,-7]))

#Split the dataset 
set.seed(101)
#train sample data
#load the catool function
sample<-sample.split(MH1$RiskLevel,SplitRatio=0.70)
train<-subset(MH1,sample=+TRUE)
dim(train)
#Testing the Dataset
test<-subset(MH1,sample==FALSE)
dim(test)

#Improving Model Performance
predicted.type<-knn(train[1:6],test[1:6],train$RiskLevel,k=1)
predict.type

#Error in prediction
error<-mean(predicted.type!=test$RiskLevel)
error

#confusion matrix
confusionMatrix(predicted.type,as.factor(test$RiskLevel),mode="everything")
#Testing the alternative k-values

predicted.type<- NULL
error.rate<-NULL
for(i in 1:10){
  predicted.type<-knn(train[1:6],test[1:6],train$RiskLevel,k=i)
  error.rate[i]<-mean(predicted.type!=test$RiskLevel)
}
knn.error<-as.data.frame(cbind(k=1:10,error.type=error.rate))
knn.error

#creating the ggplot for the k values
ggplot(knn.error,aes(k,error.type))+
  geom_point()+
  Geom_line()+
  scale_x_continoues(breaks=1:10)+
  theme_bw()+
  xlab("Value of K")+
  ylab("Error")
#Load the class function
#predict our target variable risklevel of the dataset with k=1 using knn model
predicted.type<-knn(train[1:6],test[1:6],Train$RiskLevel,k=1)
#error in predictions
error<-mean(predicted.type!=test$RiskLevel)
error
#confusion matrix
confusionMatrix(predicted.type,as.factor(test$RiskLevel))





