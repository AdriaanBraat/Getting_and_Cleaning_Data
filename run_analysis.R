# run_Analysis.R

# This syntax contains 5 steps:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Set working directory to the path of the Final Assignment
setwd("~/R/Exploring Data/Final assignment")

# The data from the training ande testset: 
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "Dataset.zip")
unzip("Dataset.zip")

library(dplyr)
# The testdata is in /test and the training data in /train. 
# The subject_train and subject_test files tell which row corresponds to which subject.
# The y_train and y_test files give information on the activities (numbers 1 to 6)
# The x_train and x_test files give the outcomes of the test (on 561 variables, 7352 observations).


###################################################################################################
# Step 1, 2 and 3 are handled at once. 
# The set of activities is recoded to descriptive outcomes first
# The variables are named
# The variables with mean and std (and not meanFreq) are extracted
# The extracted trainingset and testset are made
# The trainingseet and testset are combined
###################################################################################################


# First read activity labels and features to make the dataset tidy (i.e. descriptive names in the activity column and variable names for the features)
  activity_labels <- read.table("~/R/Exploring Data/Final assignment/UCI HAR Dataset/activity_labels.txt", col.names = c("activity_ID", "activity"))
  features_labels <- read.table("~/R/Exploring Data/Final assignment/UCI HAR Dataset/features.txt", col.names = c("feature_ID", "feature"))
  x_columns <- c("subject", "activity", as.vector(features_labels[,"feature"]))

# Create trainingset
  # get data
  x_train <- read.table("~/R/Exploring Data/Final assignment/UCI HAR Dataset/train/X_train.txt")
  # get subject vector
  subject_train <- read.table("~/R/Exploring Data/Final assignment/UCI HAR Dataset/train/subject_train.txt", col.names = c("subject"))
  
  # get activityvector and change it into descriptive names (part 3 of assignment)
  y_train <- read.table("~/R/Exploring Data/Final assignment/UCI HAR Dataset/train/y_train.txt", col.names = c("activity_ID"))
  y_train$original <- 1:nrow(y_train)
  y_train <- merge(y_train, activity_labels, by = "activity_ID")
  y_train[order(y_train$original),]

  # make trainingset and select only variables with mean or std and drop all variables with meanFreq (part 2), and give names to variables
  trainingset <- cbind(subject_train, y_train$activity, x_train)
  names(trainingset) <- x_columns
  select_columns <- grep("subject|activity|mean|std", x_columns)
  notselect_columns <- grep("meanFreq", x_columns)
  trainingset2 <- trainingset[,select_columns]
  notselect_columns <- grep("meanFreq", colnames(trainingset2))
  trainingset2 <- trainingset2[,-notselect_columns]
  

# Create testset
  # Get data
  x_test <- read.table("~/R/Exploring Data/Final assignment/UCI HAR Dataset/test/X_test.txt")

  # Get subject vector
  subject_test <- read.table("~/R/Exploring Data/Final assignment/UCI HAR Dataset/test/subject_test.txt", col.names = c("subject"))

  # Get activity vector and change into descriptive names (part 3 of assignment)
  y_test <- read.table("~/R/Exploring Data/Final assignment/UCI HAR Dataset/test/y_test.txt", col.names = c("activity_ID"))
  y_test$original <- 1:nrow(y_test)
  y_test <- merge(y_test, activity_labels, by = "activity_ID")
  y_test[order(y_test$original),]

  testset <- cbind(subject_test, y_test$activity, x_test)
  names(testset) <- x_columns
  testset2 <- testset[,select_columns]
  testset2 <- testset2[,-notselect_columns]
  
# Combine the two sets into the variable dataset
  dataset <- rbind(trainingset2, testset2)

##########################################################################
# Step 4. Rename variable names into descriptive variable names
##########################################################################

# Make descriptive variable names
  selected_columns <- names(dataset)
  selected_columns <- tolower(selected_columns)
  selected_columns <- gsub("^t", "time", selected_columns)
  selected_columns <- gsub("^f", "freq", selected_columns)
  selected_columns <- gsub("bodybody", "body", selected_columns)
  selected_columns <- gsub("body", "_body", selected_columns)
  selected_columns <- gsub("(jerk|gyro)", "_\\1", selected_columns)
  selected_columns <- gsub("mag", "_magnitude", selected_columns)
  selected_columns <- gsub("acc", "_acceleration", selected_columns)
  selected_columns <- gsub("-", "_", selected_columns)

# Give descriptive variable names to the dataset
  names(dataset) <- selected_columns




#####################################################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set with the average
#    of each variable for each activity and each subject.
#####################################################################################################

library(tidyr)
grouped_dataset <- group_by(dataset, subject, activity)
result <- summarise_each(new_dataset, funs(mean))
tidy_result <- gather(result, features, mean, -subject, -activity)

write.table(tidy_result, file="tidy_result.txt", row.name=FALSE)
