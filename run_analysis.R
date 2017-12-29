setwd("E:/7 Analizy statystyczne/RCran/Coursera R/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset")

# Loading the libraries

library(data.table)
library(dplyr)

# Loading the data

features <- read.table("E:/7 Analizy statystyczne/RCran/Coursera R/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt")
a_labels <- read.table("E:/7 Analizy statystyczne/RCran/Coursera R/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt")
s_train <- read.table("E:/7 Analizy statystyczne/RCran/Coursera R/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
a_train <- read.table("E:/7 Analizy statystyczne/RCran/Coursera R/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
f_train <- read.table("E:/7 Analizy statystyczne/RCran/Coursera R/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
s_test <- read.table("E:/7 Analizy statystyczne/RCran/Coursera R/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
a_test <- read.table("E:/7 Analizy statystyczne/RCran/Coursera R/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
f_test <- read.table("E:/7 Analizy statystyczne/RCran/Coursera R/Getting and Cleaning Data/Week 4/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")

# 1. Merges the training and the test sets to create one data set.

s_merge <- rbind(s_train, s_test)
a_merge <- rbind(a_train, a_test)
f_merge <- rbind(f_train, f_test)

colnames(f_merge) <- t(features[2])
colnames(a_merge) <- "Activity"
colnames(s_merge) <- "Subject"
full_data <- cbind(f_merge,a_merge,s_merge)
str(full_data)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

Mean_STD <- grep(".*Mean.*|.*Std.*", names(full_data), ignore.case=TRUE)
add_activity_subject <- c(Mean_STD, 562, 563)
dim(full_data)
full_data <- subset(full_data,select=add_activity_subject)
dim(full_data)
str(full_data)

# 3. Uses descriptive activity names to name the activities in the data set

full_data$Activity <- as.character(full_data$Activity)
for (i in 1:6){
  full_data$Activity[full_data$Activity == i] <- as.character(a_labels[i,2])
}
str(full_data$Activity)

# 4. Appropriately labels the data set with descriptive variable names:
# Acc -> Accelerometer
# Gyro -> Gyroscope
# BodyBody -> Body
# Mag -> Magnitude
# f -> Frequency
# t -> Time
# Freq -> Frequency
# mean -> Mean
# std -> STD

names(full_data)<-gsub("Acc", "Accelerometer", names(full_data))
names(full_data)<-gsub("Gyro", "Gyroscope", names(full_data))
names(full_data)<-gsub("BodyBody", "Body", names(full_data))
names(full_data)<-gsub("Mag", "Magnitude", names(full_data))
names(full_data)<-gsub("^t", "Time", names(full_data))
names(full_data)<-gsub("^f", "Frequency", names(full_data))
names(full_data)<-gsub("tBody", "TimeBody", names(full_data))
names(full_data)<-gsub("gravity", "Gravity", names(full_data))
names(full_data)<-gsub("Freq", "Frequency", names(full_data))
names(full_data)<-gsub("mean", "Mean", names(full_data))
names(full_data)<-gsub("std", "STD", names(full_data))
str(full_data)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

full_data_table <- data.table(full_data)
tidy_data <- aggregate(. ~Subject + Activity, full_data_table, mean)
tidy_data <- tidy_data[order(tidy_data$Subject,tidy_data$Activity),]
write.table(tidy_data, file = "Tidy_data.txt", row.names = FALSE)
