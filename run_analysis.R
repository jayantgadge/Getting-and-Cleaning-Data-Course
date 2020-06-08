library(data.table)
library(dplyr)


#Read training data
train.subjects <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
train.activities <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
train.data <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

# Read test data
test.subjects <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
test.activities <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
test.data <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# Read Supporting Metadata
featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# 1. Merges the training and the test sets to create one data set.

# Merge the training and the test sets to create one data set
  merge.data <- rbind(train.data, test.data)
  merge.activities <- rbind(train.activities, test.activities)
  merge.subjects <- rbind(train.subjects, test.subjects)

# combine all to form one table
full_table <- cbind(merge.subjects, merge.activities, merge.data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement

requiredFeatures <- featureNames[grep('-(mean|std)\\(\\)', featureNames[, 2 ]), 2]
full_table <- full_table[, c(1, 2, requiredFeatures)]


# 3. Uses descriptive activity names to name kthe activities in the data set
    # Update the activity name
  full_table[, 2] <- activityLabels[full_table[,2], 2]

# 4. Appropriately labels the data set with descriptive variable names.
   colnames(full_table) <- c('subject','activity', gsub('\\-|\\(|\\)', '', as.character(requiredFeatures)))
  
  # Coerce the data into strings
  full_table[, 2] <- as.character(full_table[, 2])

# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
	
   FinalData <- full_table %>% group_by(subject, activity) %>% summarise_all(list(mean))
   write.table(FinalData, "tidyData.txt", row.name=FALSE)
