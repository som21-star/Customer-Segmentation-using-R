data<-read.csv("C:/Users/Somenath Banerjee/Desktop/projects/MLR/data.csv")
dim(data)
View(data)
#Customer.Lifetime.Value is the predicted variable
library(dplyr)
library(caret)
library(corrplot)
glimpse(data)
#Discarding the Customer column as it has no significance
data<-data[,-1]
str(data)
#check for NA values
na_val<-sum(is.na(data))
#Variable Transformation

new_data<-data[,c(2,4,5,7)]
glimpse(new_data)
names(new_data)<-c("CLV","Coverage","Education","EmployeeStatus")
View(new_data)
require(graphics)
sum(is.na(new_data))
lm.mod<-lm(CLV~Coverage+Education+EmployeeStatus,data=new_data)
lm.mod<-train(CLV~.,new_data, method = "lm", metric="RMSE",trControl=control)
names(data)<-
glimpse(data)

#(list) object cannot be coerced to type 'double
#data<-as.numeric(data[,-c(2,9,12,13,14,15,16,21)])
levels(data)
#As there is no missing values and no need of validation,
#all categorical variables are already converted in factor
#We will do some descriptive stats including some unimodal and multimodal visualization
glimpse(data)
summary(data)

#Identifying and correcting collinearity
new_data<-subset(data,select=-c(Customer.Lifetime.Value))
new_data<-new_data[sapply(new_data,is.numeric)]
corr<-cor(new_data)
print(corr)
#Visualize correlation matrix
corrplot(corr, order="FPC", method="circle", type="lower", tl.cex=0.7, tl.col=rgb(0,0,0))
n.data<-data[,c(9,12,13,14,15,16,21)]
varImp(colnames(n.data))
highlyCorrelated<-findCorrelation(corr,cutoff = 0.7)
names<-colnames(highlyCorrelated)
names


#We would proceed further with model building part without data partitioning
#Performing multiple linear regression
set.seed(99)
control<-trainControl(method = "cv", number = 10)
mod.lm<-train(Customer.Lifetime.Value~Coverage+Education+EmploymentStatus, data, method="lm", trControl=control)
mod.lm<-lm(data$Customer.Lifetime.Value~data$Coverage)
#summary
#feature scaling : center/scale/boxcox
#again fit the model and get a summary
varImp(model, scale=FALSE)
#fit the model again and obtain the prediction accuracy and confusion matrix
#finally
summary(model$coefficients)
confint(model)
#Residual Squared Error
sigma(model)/mean(data$predictor)




