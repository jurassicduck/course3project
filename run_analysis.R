# This script will perform the following steps: 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set.
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 



#----------------------------------Step 1------------------------------------------------------------
## Merge the training and the test sets to create one data set.

# Set working directory 
setwd('C:/Users/Jurassicduck/Documents/Coursera/Data_Science/Course3')

# Read in training data
features <- read.table('./features.txt', header=FALSE)
activity_labels <- read.table('./activity_labels.txt', header=FALSE) 
subject_train <- read.table('./train/subject_train.txt', header=FALSE) 
x_train <- read.table('./train/x_train.txt', header=FALSE) 
y_train <- read.table('./train/y_train.txt', header=FALSE) 

# Rename columns
colnames(activity_labels)  <- c('activity_id','activity_type')
colnames(subject_train)  <- "subject_id"
colnames(x_train) <- features[,2]
colnames(y_train) <- "activity_id"

# Create the training data set
train <- cbind(x_train, subject_train, y_train)

# Read in the test data
subject_test <- read.table('./test/subject_test.txt', header=FALSE) 
x_test       <- read.table('./test/x_test.txt', header=FALSE) 
y_test       <- read.table('./test/y_test.txt', header=FALSE)

# Rename columns
colnames(subject_test) <- "subject_id"
colnames(x_test) <- features[,2]
colnames(y_test) <- "activity_id"

# Create the test data set
test <- cbind(x_test, subject_test, y_test)

# Combine training and test data to create a final data set
final_data <- rbind(train, test)



#--------------------------------Step 2----------------------------------------
## Extract only the measurements on the mean and standard deviation for each measurement.

# Get all column names and store them in a list
col_names <- colnames(final_data)

# Keep only the columns we want 
logical_vec <-  grepl("_id$", col_names) | 
                grepl("-mean..",col_names) & 
                !grepl("-meanFreq..",col_names) & 
                !grepl("mean..-",col_names) | 
                grepl("-std..",col_names) & 
                !grepl("-std()..-",col_names)

data1 <- final_data[logical_vec==TRUE]




#--------------------------------Step 3----------------------------------------
## Use descriptive activity names to name the activities in the data set.

# Add activity_labels info to the final data set
final_data <- merge(final_data, activity_labels, by='activity_id',all.x=TRUE);




#--------------------------------Step 4----------------------------------------
## Appropriate label the data set with descriptive variable name.

col_names <- colnames(final_data)

for (i in 1:length(col_names)) 
{
        col_names[i] <- gsub("\\()", "", col_names[i])
        col_names[i] <- gsub("^(t)", "time", col_names[i])
        col_names[i] <- gsub("^(f)", "freq", col_names[i])
        col_names[i] <- gsub("([Gg]ravity)", "Gravity", col_names[i])
        col_names[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)", "Body", col_names[i])
        col_names[i] <- gsub("[Gg]yro", "Gyro", col_names[i])
        col_names[i] <- gsub("AccMag", "AccMagnitude", col_names[i])
        col_names[i] <- gsub("([Bb]odyaccjerkmag)", "BodyAccJerkMagnitude", col_names[i])
        col_names[i] <- gsub("JerkMag", "JerkMagnitude", col_names[i])
        col_names[i] <- gsub("GyroMag", "GyroMagnitude", col_names[i])
}

# Update column names for the final data set
colnames(final_data) <- col_names



#--------------------------------Step 5----------------------------------------
## Create a second, independent tidy data set with the average of each variable of
## for each activity and each subject.

# Get data set without activity type
data2 <- final_data[, !colnames(final_data)=="activity_type"]

# Get new data set with average of each variable for each activity and each subject
data3 <- aggregate(data2[, !colnames(data2) %in% c("activity_id", "subject_id")], 
                   by=list(subject_id=data2$subject_id, activity_id=data2$activity_id),
                   mean)

# Get activity type back into the data
data3 <- merge(data3, activity_labels, by="activity_id", all.x=TRUE)

# Export data3
write.table(data3, file='tidy_data.txt', row.names=FALSE)



