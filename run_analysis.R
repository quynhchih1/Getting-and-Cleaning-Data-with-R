## This script is to read the descriptive data 
## Download the data 
fileUrl="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
library(data.table)
library(dplyr)
download.file(fileUrl, destfile="dataFiles.zip")
unzip(zipfile="dataFiles.zip")
## Load activity labels and features
feature_names <- read.table("UCI HAR Dataset/features.txt")
activity_labels <-read.table("UCI HAR Dataset/activity_labels.txt")
## Read training data
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
feature_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
## Read test data
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
feature_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
##Merges the training and the test sets to create one data set.
subject <- rbind(subject_train, subject_test)
activity <- rbind(activity_train, activity_test)
feature <- rbind(feature_train, feature_test)
colnames(feature)=t(feature_names[2]) ##set the names for columns in feature data
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(feature, activity, subject)
##Extracts only the measurements on the mean and standard deviation for each measurement. 
extract_index <- grep("mean|std", names(completeData), ignore.case=TRUE) #get index of columns that satisfy
extract_data<-completeData[,c(extract_index, 562, 563)] ##column 562 and 563 is activity and subject
##Uses descriptive activity names to name the activities in the data set
extract_data$Activity <- as.character(extract_data$Activity)
for (i in 1:6){ ##matching the value of Activity to labels 
  extract_data$Activity[extract_data$Activity==i] <- as.character(activity_labels[i,2])
}
##Appropriately labels the data set with descriptive variable names. 
names(extract_data)%<>%
  gsub("Acc", "Accelerometer",.)%>%
  gsub("Gyro", "Gyroscrope",.)%>%
  gsub("-mean()","Mean", ignore.case=TRUE,.)%>%
  gsub("-std()", "STD", ignore.case=TRUE,.)%>%
  gsub("Mag", "Magnitude",.)%>%
  gsub("Freq","Frequency",.)%>%
  gsub("gravity", "Gravity",.)%>%
  gsub("^t", "Time",.)%>%
  gsub("^f","Frequency",.)%>%
  gsub("BodyBody","Body",.)%>%
  gsub("tBody", "TimeBody",.)
names(extract_data)
##Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
extract_data$Activity<-as.factor(extract_data$Activity)
extract_data$Subject<-as.factor(extract_data$Subject)
tidy_data<-aggregate(.~Subject + Activity, extract_data, mean) ##aggregate all columns except Activity and Suject
tidy_data<-tidy_data[order(tidy_data$Subject, tidy_data$Activity),]
write.table(tidy_data, file="TidyData.txt",row.names=FALSE)
