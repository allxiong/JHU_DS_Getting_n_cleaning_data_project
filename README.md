#Filename: READEME.md
#Contents: This documents describs how the script run_analysis.R works step by step
#Author: Allie

##################### Requirements #################################################################################################
#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
#The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
#A full description is available at the site where the data was obtained:
#  http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#
#Here are the data for the project:
#  
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
#You should create one R script called run_analysis.R that does the following.
#
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement.
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names.
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity 
#  and each subject.
####################################################################################################################################
#manually downloaded and unziped files into directory "C:\Users\allie\OneDrive\Documents\data\assignment_Dataset\UCI_HAR_Dataset"
#setwd to this directory
setwd("C:/MyDocuments/ProfessionalDevelopment/TechnicalSkillDevelopment/DataScience/JohndHopkinsUniversity_DS_program/C3_Getting_n_cleaning_data/Lab_assignment")
getwd()
list.files("./")

fileName <- "UCIdata.zip"
url <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
dir <- "UCI HAR Dataset"

# File download to working directory.
if(!file.exists(fileName)){
  download.file(url,fileName, mode = "wb") 
}

# File unzip 
if(!file.exists(dir)){
  unzip("UCIdata.zip", files = NULL, exdir=".")
}



#Preparation: read the labels and features to have an overview of the data
#- 'features_info.txt': Shows information about the variables used on the feature vector.
#- 'features.txt': List of all features.
#- 'activity_labels.txt': Links the class labels with their activity name.
#- 'train/X_train.txt': Training set.
#- 'train/y_train.txt': Training labels.

features <- read.table("./UCI HAR Dataset/features.txt")
class(features)
dim(features)
head(features)
#V1                V2
#1  1 tBodyAcc-mean()-X
#2  2 tBodyAcc-mean()-Y
#3  3 tBodyAcc-mean()-Z
#4  4  tBodyAcc-std()-X
#5  5  tBodyAcc-std()-Y
#6  6  tBodyAcc-std()-Z

tail(features)
#V1                                   V2
#556 556 angle(tBodyAccJerkMean),gravityMean)
#557 557     angle(tBodyGyroMean,gravityMean)
#558 558 angle(tBodyGyroJerkMean,gravityMean)
#559 559                 angle(X,gravityMean)
#560 560                 angle(Y,gravityMean)
#561 561                 angle(Z,gravityMean)
class(features[,2])
#[1] "factor"

#should I use factor or convert them to characters? Here's a discussion: 
#https://datascience.stackexchange.com/questions/12018/when-to-choose-character-instead-of-factor-in-r
#convert factor to character
#activity_labels[,2] <- as.character(activity_labels[,2])
#features[,2] <- as.character(features[,2])

#step1: read the dataset
#read the training set
trainX <- read.table("./UCI HAR Dataset/train/X_train.txt")
class(trainX)
dim(trainX)
head(trainX)

trainy <- read.table("./UCI HAR Dataset/train/Y_train.txt")
class(trainy)
dim(trainy)
head(trainy)

train_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
class(train_subjects)
dim(train_subjects)
head(train_subjects)

#all the trainingdata has 7352 rows, use cbind
#train_data <- cbind(train_subjects, trainy, trainX)
#class(train_data) #data.frame
#dim(train_data)
#[1] 7352  563
#head(train_data)

#Read Test datasets .Look at their dimension. 
testX<-read.table("./UCI HAR Dataset/test/X_test.txt")
head(testX)
class(testX)  #testX is a data.frame
dim(testX)
#dimension: [1] 2947  561

testy<-read.table("./UCI HAR Dataset/test/y_test.txt")
head(testy)
class(testy)  #testy is a data.frame
dim(testy)  #[1] 2947    1

test_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")
head(test_subjects)
class(test_subjects)  #testSubjects is a data.frame
dim(test_subjects)

#all the data have 2947 rows. cbind test data
#teat_data<-cbind(test_subjects, testy, testX)
#head(teat_data)
#class(teat_data)  #testSubjects is a data.frame
#dim(teat_data)
#[1] 2947  563

#Merges the training and the test sets to create one data set.
data.set<-rbind(trainX, testX)
dim(data.set)
head(data.set, 2)

#step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
#according to features.txt file, The set of variables that were estimated from these signals are: 
#mean(): Mean value
#std(): Standard deviation
#create a subset of the mean and std only features

feature_mean_std <- grep("mean()|std()", features[,2])

data.set<-data.set[,feature_mean_std]
dim(data.set)
head(data.set,1)

feature_mean_std_names <- features[feature_mean_std]
#feature_mean_std_names = gsub('-mean', 'Mean', feature_mean_std_names)
#feature_mean_std_names = gsub('-std', 'Std', feature_mean_std_names)
feature_mean_std_names <- gsub('[-()]', '', feature_mean_std_names)

#Prepare for step 3, Uses descriptive activity names to name the activities in the data set
#'activity_labels.txt': Links the class labels with their activity name.
# 'train/y_train.txt': Training labels.
# 'test/y_test.txt': Test labels.
# read activity_labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
class(activity_labels)
dim(activity_labels)
head(activity_labels)
#V1                 V2
#1  1            WALKING
#2  2   WALKING_UPSTAIRS
#3  3 WALKING_DOWNSTAIRS
#4  4            SITTING
#5  5           STANDING
#6  6             LAYING

#Merges the training and the test labels to create one label set.
subject.set <- rbind(subject_train, subject_test)
label.set<-rbind(trainy, testy)
data.set<-cbind(subject.set, label.set, data.set)
dim(data.set)
head(data.set,1)


#step 4, Appropriately labels the data set with descriptive variable names.
featureNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(data.set)<-c("Subject", "Activity", featureNames[feature_mean_std])

#step 3, Uses descriptive activity names to name the activities in the data set
data.set$Activity <- factor(data.set$Activity, levels = activity_labels[,1], labels = activity_labels[,2])
head(data.set)

#step 5, From the data set in step 4, creates a second, independent tidy data set with the average of each 
#variable for each activity and each subject.
install.packages(reshape2)
library(reshape2)
data.set.melted <- melt(data.set, id = c("Subject", "Activity"))
data.set2<- dcast(data.set.melted, Subject+Activity ~ variable, mean)
View(data.set2)
write.table(data.set2, "tidyData.txt", row.names = FALSE)

#Use dataMaid package to generate codebook. More information about generating code book, refer to 
#https://www.r-bloggers.com/generating-codebooks-in-r/
install.packages("dataMaid")
library(dataMaid)
makeCodebook(data.set2)
#file is saved as codebook.pdf
