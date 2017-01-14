## Merges the training and the test sets to create one data set

subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
y_train<-read.table("./UCI HAR Dataset/train/y_train.txt")
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
train<-cbind(subject_train,y_train,X_train)

subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
y_test<-read.table("./UCI HAR Dataset/test/y_test.txt")
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
test<-cbind(subject_test,y_test,X_test)

features<-read.table("./UCI HAR Dataset/features.txt")[,2]
features<-as.character(features)
dataSet<-rbind(train,test)
##colnames(dataSet)<-c("subjectId","activityId",features)

##Extracts only the measurements on the mean and standard deviation for each measurement.
good<-c(TRUE,TRUE,grepl("mean",features)|grepl("std",features))
dataSetMeanAndStd<-dataSet[,c(good)]

##Uses descriptive activity names to name the activities in the data set
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")
activityRange<-activity_labels[,1]
activity<-as.character(activity_labels[,2])
dataSetDescriptive<-dataSetMeanAndStd
for(x in activityRange){
  dataSetDescriptive[,2]<-replace(dataSetDescriptive[,2],dataSetDescriptive[,2]==x,activity[x])
}

##Appropriately labels the data set with descriptive variable names
colnames(dataSetDescriptive)<-c("subjectId","activityId",features)

##From the data set in step 4, creates a second, independent tidy data set 
##with the average of each variable for each activity and each subject
library(plyr);
tidy<-aggregate(. ~subjectId + activityId, dataSetDescriptive, mean)
tidy<-tidy[order(tidy$subjectId,tidy$activityId),]
write.table(tidy,"./UCI HAR Dataset/tidy.txt",row.name=FALSE)
