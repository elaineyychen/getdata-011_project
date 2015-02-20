## CourseRA--Getting and Cleaning Data--Project
## run_analysis.R
## 2015/02/20

library(reshape2)
rm(list=ls())

run_analysis <- function(){
  ### 1. Merges the training and the test sets to create one data set.
  
  ## loads data into R objects from local data files
  # subject data
  subject_train <- read.table("./data/train/subject_train.txt",col.names=c("subject"))
  subject_test <- read.table("./data/test/subject_test.txt",col.names=c("subject"))
  # activity data
  y_train <- read.table("./data/train/y_train.txt",col.names=c("activity"))
  y_test <- read.table("./data/test/y_test.txt",col.names=c("activity"))
  # feature data 
  # (labels the data set with descriptive variable names according to 'features.txt')
  features <- read.table("./data/features.txt",stringsAsFactors=FALSE)
  X_train <- read.table("./data/train/X_train.txt",col.names=features[,2])
  X_test <- read.table("./data/test/X_test.txt",col.names=features[,2])
  
  ## merges datasets
  all_train <- cbind(subject_train,y_train,X_train)
  all_test <- cbind(subject_test,y_test,X_test)
  total_dataset <- rbind(all_train,all_test)
  
  ### 2. Extracts only the measurements on the mean and 
  ### standard deviation for each measurement. 
  
  # searches the column names of dataset which contains 'mean'
  name_mean <- grepl("mean",names(total_dataset))
  # searches the column names of dataset which contains 'meanFreq'
  # variables with 'meanFreq' are estimated by meanFreq() rather than mean(),
  # so they should not be extracted.
  name_meanFreq <- grepl("meanFreq",names(total_dataset))
  col_mean <- colnames(total_dataset)[name_mean&!name_meanFreq]
  
  # searches the column names of dataset which contains 'std'
  name_std <- grepl("std",names(total_dataset))
  col_std <- colnames(total_dataset)[name_std]
  
  # extracts the measurements on the mean and standard deviation
  measurements <- total_dataset[c("subject","activity",col_std,col_mean)]
  
  
  ### 3. Uses descriptive activity names to name the activities in the data set
  measurements$activity <- factor(measurements$activity,levels=c(1,2,3,4,5,6),
                                  labels=c("WALKING","WALKING_UPSTAIRS",
                                           "WALKING_DOWNSTAIRS","SITTING",
                                           "STANDING","LAYING"))
  ### 4. Appropriately labels the data set with descriptive variable names. 
  ### Note: Step 4 has been completed in Step 1.
  
  ### 5. From the data set in step 4, creates a second, independent tidy data set 
  ### with the average of each variable for each activity and each subject.
  # melts the data frame
  md <- melt(measurements,id=c("subject","activity"))
  # casts the data frame
  # final result (wide-tidy form)
  result <- dcast(md,subject+activity~variable,mean)
  # final result (long-tidy form)
  # result <- melt(result,id=c("subject","activity"))

  result
}

