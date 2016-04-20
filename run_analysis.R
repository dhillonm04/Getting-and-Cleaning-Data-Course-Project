#run_analysis.R that does the following:

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#Dataset being used can be found here: "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"


run_analysis <- function() {
  
  library(plyr)
  
  #1. Merges the training and the test sets to create one data set. 

  #1.1 Read Dataset
  x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
  
  x_test <- read.table("UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
  
  # create 'x' data set
  x_data_set <- rbind(x_train, x_test)
  
  # create 'y' data set
  y_data_set <- rbind(y_train, y_test)
  
  # create 'subject' data set
  subject_data_set <- rbind(subject_train, subject_test)
  
  
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement.
  features <- read.table("UCI HAR Dataset/features.txt")
  mean_std_measurement <- grep("-(mean|std)\\(\\)", features[, 2])
  x_data_subset <- x_data_set[ ,mean_std_measurement]
  names(x_data_subset) <- features[mean_std_measurement,2]
  
  
  # 3. Uses descriptive activity names to name the activities in the data set
  activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
  y_data_set[, 1] <- activityLabels[y_data_set[, 1], 2]
  names(y_data_set) <- "activity"

  # 4. Appropriately labels the data set with descriptive variable names.
  names(subject_data_set) <- "subject"
  names(x_data_subset) = gsub('-mean', 'Mean', names(x_data_subset))
  names(x_data_subset) = gsub('-std', 'Std', names(x_data_subset))
  names(x_data_subset) <- gsub('[-()]', '', names(x_data_subset))
  tidy_data <- cbind(subject_data_set, y_data_set,x_data_subset)
  
  
  # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
   averages_tidy_data <- ddply(tidy_data, .(subject, activity), function(x) colMeans(x[, 3:68]))
  
   write.table(averages_tidy_data, "tidy_data_average.txt", row.name=FALSE)
  
}
