#install Packages
install.packages("e1071")
install.packages("caTools")
install.packages("caret")
install.packages("class")
install.packages("lattice")
install.packages("fpc")
install.packages("factoextra")

#decision Tree
install.packages("party")

#setwd('E:/third year material/firstSemester/Statistical Inference/R_Project')

#load packages
library(e1071)
library(caTools)
library(caret)
library(cluster)
library(class)
#for decision Tree
library(party)
library(dplyr)
library(magrittr)
library(factoextra)
library(ggplot2)
#for DBSCAN
library(fpc)


#read data set
data <- read.csv("CustomerSegmentation.csv")

data$CustomerID<-NULL
head(data)
#get the unique values in Gender Column
gender_unique<-unique(data$Gender)

#transform data type in Gender Column => "Male"=>1 "Female"=>2
data$Gender<-as.numeric(factor(data$Gender,levels = gender_unique))

#handling missing data in columns
#save data with age more than or equal 18
data=data[data$Age>=18,]
data=data[data$Annual.Income..k..>=0,]
data=data[data$Spending.Score..1.100.>=1,]
data=data[data$Spending.Score..1.100.<100,]

#create column to spend score range (low, med,high)
data[, 'spend_score_range'] = NA

data$spend_score_range[data$Spending.Score..1.100.<35]<-"low"
data$spend_score_range[data$Spending.Score..1.100.>=35&data$Spending.Score..1.100.<70]<-"med"
data$spend_score_range[data$Spending.Score..1.100.>=70]<-"high"
spend_unique<-unique(data$spend_score_range)
data$spend_score_range<-as.numeric(factor(data$spend_score_range,levels = spend_unique))
#check if any gender cell is missing 
mean(data$Gender)
#check if any Age cell is missing 
mean(data$Age)
#check if any annual income cell is missing 
mean(data$Annual.Income..k..)
#check if any score cell is missing 
mean(data$Spending.Score..1.100.)
#calculate the correlation between columns
corr_data=cor(data[,0:4])


summary(data)
boxplot(data$Annual.Income..k..)
plot(density(data$Age))


#Kmeans(clustering)

km<-kmeans(x = data[,0:3],centers = 3)


table(data[,5],km$cluster)

plot(data$Age,data$Annual.Income..k..,col=km$cluster)
points(km$centers,col=1:4,pch=8,cex=2)


data$Spending.Score..1.100.<-NULL
#naive Bayes

# Splitting data into train
# and test data
split <- sample.split(data, SplitRatio = 0.7)
train_cl <- subset(data, split == "TRUE")
test_cl <- subset(data, split == "FALSE")



# Feature Scaling
train_scale <- scale(train_cl)
test_scale <- scale(test_cl)



# Fitting Naive Bayes Model
# to training dataset
set.seed(120)  # Setting Seed

classifier_cl <- naiveBayes(spend_score_range ~ ., data = train_cl)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$spend_score_range, y_pred)
cm

# Model Evaluation
confusionMatrix(cm)




#KNN


# Splitting data into train
# and test data
split_k <- sample.split(data, SplitRatio = 0.7)
train_cl_k <- subset(data, split == "TRUE")
test_cl_k <- subset(data, split == "FALSE")



# Feature Scaling
train_scale_k <- scale(train_cl_k[, 1:3])
test_scale_k <- scale(test_cl_k[, 1:3])


# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale_k,
                      test = test_scale_k,
                      cl = train_cl_k$spend_score_range,
                      k = 1)
classifier_knn



# Confusiin Matrix
cm <- table(test_cl_k$spend_score_range, classifier_knn)
cm



misClassError <- mean(classifier_knn != test_cl_k$spend_score_range)
print(paste('Accuracy =', 1-misClassError))



#Decision Tree

#split into train and test data
sample_data = sample.split(data, SplitRatio = 0.7)
train_data <- subset(data, sample_data == TRUE)
test_data <- subset(data, sample_data == FALSE)


model<- ctree(spend_score_range ~ ., train_data)
plot(model)

#predict
predict_model<-predict(model, test_data)
m_at <- table(test_data$spend_score_range, predict_model)
m_at

#calculate accuracy

ac_Test <- sum(diag(m_at)) / sum(m_at)
print(paste('Accuracy for test is found to be', ac_Test))





#----clustering

#DBSCAN

# Fitting DBScan clustering Model 
# to training dataset
set.seed(220)  # Setting seed
Dbscan_cl <- dbscan(data[,2:4], eps = 0.45, MinPts = 2,)

# Checking cluster
Dbscan_cl$cluster


table(Dbscan_cl$cluster, data$spend_score_range)

# Plotting Cluster
plot(Dbscan_cl, data[,2:4], main = "DBScan")




#SVM

tempDataSet=data

#head(tempDataSet[,1:3])
#head(tempDataSet)
tempDataSet=tempDataSet[,1:4]
set.seed(123)


tempDataSet$Gender[tempDataSet$Gender==1]<-as.numeric(0)
tempDataSet$Gender[tempDataSet$Gender==2]<-as.numeric(1)

split = sample.split(tempDataSet$Age, SplitRatio = 0.75)

training_set = subset(tempDataSet, split == TRUE)
test_set = subset(tempDataSet, split == FALSE)


classifier=svm(formula= Gender~.,data=tempDataSet,type='C-classification',kernel='linear')



# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[,2:4])


cm = table(test_set[, 1], y_pred)




data <- read.csv("CustomerSegmentation.csv")

data$CustomerID<-NULL

data[, 'spend_score_range'] = NA

data$spend_score_range[data$Spending.Score..1.100.<35]<-"low"
data$spend_score_range[data$Spending.Score..1.100.>=35&data$Spending.Score..1.100.<70]<-"med"
data$spend_score_range[data$Spending.Score..1.100.>=70]<-"high"

#Kmeans 2 (clustering) between age and annual income
rdata<-data[,2:3]
tempdata<-scale(rdata)

distData<-dist(tempdata)

#to get the best number of clusters using elbow method (here the best number of clusters is 5 because after number 5 the number of squares decrease too slow)
fviz_nbclust(tempdata,kmeans,method = "wss")+labs(subtitle = "Elbow method")

km.out<-kmeans(tempdata,centers = 5,nstart = 200)

#visualize the clustering algorithm results

km.cluster<-km.out$cluster
#name each point to be unique
rownames(tempdata)<- paste(data$spend_score_range,1:dim(data)[1],sep = "_")

# to display the clusters 
fviz_cluster(list(data=tempdata,cluster=km.cluster))


table(data$spend_score_range,km.cluster)
table(data$Gender,km.cluster)




#K means 3 (clustering) spending score and annual income 
rdata<-data[,3:4]
#tempdata<-rdata
tempdata<-scale(rdata)

distData<-dist(tempdata)

fviz_nbclust(tempdata,kmeans,method = "wss")+labs(subtitle = "Elbow method")

km.out<-kmeans(tempdata,centers = 6,nstart = 100)

#visualize the clustering algorithm results

km.cluster<-km.out$cluster
rownames(tempdata)<- paste(data$Gender,1:dim(data)[1],sep = "_")

fviz_cluster(list(data=tempdata,cluster=km.cluster))


table(data$spend_score_range,km.cluster)
table(data$Gender,km.cluster)






#visualize gender and and annual income and age
#(Blue for male) and (red for female)
#relationship between income and spend score
plot(x=data$Annual.Income..k..,y=data$Spending.Score..1.100.,xlab = "Annnual income",ylab = "Spending Score",col=ifelse(data$Gender=="Male",'blue','red'),pch=19,main = "income and spending")

#relationship between age and spend score  (conclusion)->the less ages are higher spending score
plot(x=data$Age,y=data$Spending.Score..1.100.,xlab = "Age",ylab = "Spending Score",col=ifelse(data$Gender=="Male",'blue','red'),pch=19,main = "age and spending")


#relationship between age and spend score  (conclusion)->the ages between 30 and 50 have the highest income
plot(x=data$Age,y=data$Annual.Income..k..,xlab = "Age",ylab = "annual income",col=ifelse(data$Gender=="Male",'blue','red'),pch=19,main = "age and annual income",)
