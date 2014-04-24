#read training set - 7352 lines
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", quote="\"")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", quote="\"")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", quote="\"")
#merge data
train <- cbind(subject_train,y_train,x_train)
#delete "cached" data
rm(subject_train)
rm(y_train)
rm(x_train)

#read test set - 2947 lines
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", quote="\"")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", quote="\"")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", quote="\"")
#merge data
test <- cbind(subject_test,y_test,x_test)
#delete "cached" data
rm(subject_test)
rm(y_test)
rm(x_test)

#merge train and test data to 
data <- rbind(train,test)
rm(train)
rm(test)

#column names 
features <- read.table("UCI HAR Dataset/features.txt", quote="\"")
names<-names(data)
names[1]<-c("subject")
names[2]<-c("activity")
names[3:563] <- as.character(features[,2])
names(data)<-names

#feature "activity" changes to factor and set names
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt", quote="\"")
activity.names <- as.character(activity_labels[,2])
data$activity<-factor(data$activity,labels=activity.names)
summary(data$activity)

#select only mean() and std() functions 
data.mean.std<-data.frame(data[1])
data.mean.std<-cbind(data.mean.std,data[2])
for(n in 3:ncol(data)){
  name <- names(data[n])
  if((grepl("mean\\(\\)",name)==1)|(grepl("std\\(\\)",name)==1)) {
    data.mean.std<-cbind(data.mean.std,data[n])
  }
}

#group data - create group(subject, activity) and count mean value for each group
subject<-rep(1:30,6)
activity<-rep(1:6,each=30)
data.group<-data.frame(subject)
data.group<-cbind(data.group,activity)
data$group <- data$subject+as.integer(data$activity)*100
summary(data$group)
count<-(ncol(data)-1)
for(n in 3:count){
  pom<-tapply(data[,n],data$group,mean)
  data.group<-cbind(data.group,pom)
}

#set same names as has data frame "data"
names(data.group)<-names
row.names(data.group)<-NULL
data.group$activity<-factor(data.group$activity,labels=activity.names)

#store data in file
write.table(data.group,file="data_group.txt")
