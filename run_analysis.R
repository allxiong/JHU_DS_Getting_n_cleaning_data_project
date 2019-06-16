#Filename: run_analysis.R
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

features <- read.table("./UCI HAR Dataset/features.txt")

#step1: read the dataset
#read the training set
trainX <- read.table("./UCI HAR Dataset/train/X_train.txt")
dim(trainX)

trainy <- read.table("./UCI HAR Dataset/train/Y_train.txt")
dim(trainy)

train_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")
dim(train_subjects)

#Read test datasets .Look at their dimension. 
testX<-read.table("./UCI HAR Dataset/test/X_test.txt")
testy<-read.table("./UCI HAR Dataset/test/y_test.txt")
test_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#Merges the training and the test sets to create one data set.
data.set<-rbind(trainX, testX)


#step 2: Extracts only the measurements on the mean and standard deviation for each measurement.
feature_mean_std <- grep("mean()|std()", features[,2])
data.set<-data.set[,feature_mean_std]
feature_mean_std_names <- features[feature_mean_std]
#feature_mean_std_names <- gsub('[-()]', '', feature_mean_std_names)

#Prepare for step 3, Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")

#Merges the training and the test labels to create one label set.
subject.set <- rbind(train_subjects, test_subjects)
label.set<-rbind(trainy, testy)
data.set<-cbind(subject.set, label.set, data.set)

#step 4, Appropriately labels the data set with descriptive variable names.
featureNames <- sapply(features[, 2], function(x) {gsub("[()]", "",x)})
names(data.set)<-c("Subject", "Activity", featureNames[feature_mean_std])

#step 3, Uses descriptive activity names to name the activities in the data set
data.set$Activity <- factor(data.set$Activity, levels = activity_labels[,1], labels = activity_labels[,2])
head(data.set)

#step 5, From the data set in step 4, creates a second, independent tidy data set with the average of each 
#variable for each activity and each subject.
install.packages("reshape2")
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