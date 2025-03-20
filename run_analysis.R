setwd("C:/Users/rmendozam/Desktop/Getting-and-cleaning-DC-Project/UCI HAR Dataset")

install.packages("data.table")
install.packages("dplyr")
library(data.table)
library(dplyr)


#### Reading data ####

featureNames <- read.table("features.txt")
activityLabels <- read.table("activity_labels.txt", header = FALSE)


##### Format training and test data sets ####

### Read training data ####

subjectTrain <- read.table("train/subject_train.txt", header = FALSE)
activityTrain <- read.table("train/y_train.txt", header = FALSE)
featuresTrain <- read.table("train/X_train.txt", header = FALSE)


### Read test data ####

subjectTest <- read.table("test/subject_test.txt", header = FALSE)
activityTest <- read.table("test/y_test.txt", header = FALSE)
featuresTest <- read.table("test/X_test.txt", header = FALSE)


##### Component 1: Merges the training and the tests set to create one data set ----

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

#### 1.1 Naming columns

colnames(features) <- t(featureNames[2])


### 1.2 Merging the data
### completeData shows the merging between activity & subject

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)
summary(completeData)


##### Component 2: Extracts only the measurements on the mean and sdv for each variable ----

#### 2.1 extract the column indices that have either mean or std in them

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)


### 2.2 adding activity and subject columns to the list and look dim of completeData

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)


### 2.3 creating a extractData variable with the selected columns in requiredColumns. And again, we look at the dimension of requiredColumns

extractedData <- completeData[,requiredColumns]
dim(extractedData)



##### Component 3: Uses descriptive activity names to name the activities in the data set ----

### 3.1 The activity field in extractedData is originally of numeric type
### We need to change its type to character so that it can accept activity names
### The activity names are taken from metadata activityLabels


extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

### 3.2 we need to factor the activity variable, once the activity names are updated

extractedData$Activity <- as.factor(extractedData$Activity)



##### Component 4: Appropriately labels the data set with descriptive variable names ----

### 4.1 naming the variables from extractData


names(extractedData)


### 4.2 By examining extractedData, we can say that the following acronyms can be replaced:
### Acc can be replaced with Accelerometer
### Gyro can be replaced with Gyroscope
### BodyBody can be replaced with Body
### Mag can be replaced with Magnitude
### Character f can be replaced with Frequency
### Character t can be replaced with Time

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

### 4.3 Here the new names

names(extractedData)



##### Component 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject ----


### 5.1 Setting subject as a factor variable

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

### 5.2 creating tydydata as a data set with average for each activity and subject
### then, we order the enties in tidyData and write it into data file Tidy.txt that contains the processed data

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)



