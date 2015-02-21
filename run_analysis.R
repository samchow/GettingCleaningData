# Setup. Not we assume you are in the UCI HAR Dataset directory

###### Common Routines
renameCols <- function(cols) {
  x <- NULL
  x <- gsub("[[:punct:]]", "", cols)
  x <- gsub("^t", "", x)
  x <- gsub("^f", "Freq", x)
  x <- gsub("([[:alpha:]]+)mean([[:alpha:]])", "avg\\1.\\2", x)
  x <- gsub("([[:alpha:]]+)mean", "avg\\1", x)
  x <- gsub("([[:alpha:]]+)std([[:alpha:]])", "sdev\\1.\\2", x)
  x <- gsub("([[:alpha:]]+)std", "sdev\\1", x)
  x <- gsub("Mag", "Magnitude", x)
  x <- gsub("Acc", "Accel", x)
  x
}

############### Getting Meta Info #####

## Get the meta information
# Read in the complete 561 features
features <- read.table("features.txt", 
                       col.names=c("index", "label"))
# Only interested in the mean() and std()
sel_features <- features[grep("(mean|std)\\(", features$label), ]
sel_labels <- renameCols(sel_features$label)

# Read in activity labels
activities  <- read.table("activity_labels.txt", 
                          col.names=c("activity.code", "activity"))

################ Read in test data
test_data <- read.table("test/X_test.txt")
# filter out unneeded cols
sel_test_data <- test_data[, sel_features$index]
colnames(sel_test_data) <- sel_labels

# get test subject
temp <- read.table("test/subject_test.txt", col.names="subject")
sel_test_data <- cbind(sel_test_data, temp)

# get activity
temp <- read.table("test/y_test.txt", col.names="activity.code")
sel_test_data <- cbind(sel_test_data, temp)


##############Read in train data
train_data <- read.table("train/X_train.txt")
# filter oun uneeded col
sel_train_data <- train_data[, sel_features$index]
colnames(sel_train_data) <- sel_labels

# get subject data
temp <- read.table("train/subject_train.txt", col.names="subject")
sel_train_data <- cbind(sel_train_data, temp)

# get activity data
temp <- read.table("train/y_train.txt", col.names="activity.code")
sel_train_data <- cbind(sel_train_data, temp)

## Combine the 2 data fame
sel_data1 <- rbind(sel_train_data, sel_test_data)

sel_data2 <- merge(sel_data1, activities, by="activity.code")

################## Now create final tiny_data
library(dplyr)

sel_sum <- group_by(sel_data2,  subject, activity)

tiny_data <- summarize(sel_sum, 
                       avgBodyAccel.X = mean(avgBodyAccel.X),
                       avgBodyAccel.Y = mean(avgBodyAccel.X),
                       avgBodyAccel.Z = mean(avgBodyAccel.Z),
                       sdevBodyAccel.X = mean(sdevBodyAccel.X),
                       sdevBodyAccel.Y = mean(sdevBodyAccel.Y),
                       sdevBodyAccel.Z = mean(sdevBodyAccel.Z),
                       avgGravityAccel.X = mean(avgGravityAccel.X),
                       avgGravityAccel.Y = mean(avgGravityAccel.Y),
                       avgGravityAccel.Z = mean(avgGravityAccel.Z),
                       sdevGravityAccel.X = mean(sdevGravityAccel.X),
                       sdevGravityAccel.Y = mean(sdevGravityAccel.Y),
                       sdevGravityAccel.Z = mean(sdevGravityAccel.Z),
                       avgBodyAccelJerk.X = mean(avgBodyAccelJerk.X),
                       avgBodyAccelJerk.Y = mean(avgBodyAccelJerk.Y),
                       avgBodyAccelJerk.Z = mean(avgBodyAccelJerk.Z),
                       sdevBodyAccelJerk.X = mean(sdevBodyAccelJerk.X),
                       sdevBodyAccelJerk.Y = mean(sdevBodyAccelJerk.Y),
                       sdevBodyAccelJerk.Z = mean(sdevBodyAccelJerk.Z),
                       avgBodyGyro.X = mean(avgBodyGyro.X),
                       avgBodyGyro.Y = mean(avgBodyGyro.Y),
                       avgBodyGyro.Z = mean(avgBodyGyro.Z),
                       sdevBodyGyro.X = mean(sdevBodyGyro.X),
                       sdevBodyGyro.Y = mean(sdevBodyGyro.Y),
                       sdevBodyGyro.Z = mean(sdevBodyGyro.Z),
                       avgBodyGyroJerk.X = mean(avgBodyGyroJerk.X),
                       avgBodyGyroJerk.Y = mean(avgBodyGyroJerk.Y),
                       avgBodyGyroJerk.Z = mean(avgBodyGyroJerk.Z),
                       sdevBodyGyroJerk.X = mean(sdevBodyGyroJerk.X),
                       sdevBodyGyroJerk.Y = mean(sdevBodyGyroJerk.Y),
                       sdevBodyGyroJerk.Z = mean(sdevBodyGyroJerk.Z),
                       avgBodyAccelMagnitude = mean(avgBodyAccelMagnitude),
                       sdevBodyAccelMagnitude = mean(sdevBodyAccelMagnitude),
                       avgGravityAccelMagnitude = mean(avgGravityAccelMagnitude),
                       sdevGravityAccelMagnitude = mean(sdevGravityAccelMagnitude),
                       avgBodyAccelJerkMagnitude = mean(avgBodyAccelJerkMagnitude),
                       sdevBodyAccelJerkMagnitude = mean(sdevBodyAccelJerkMagnitude),
                       avgBodyGyroMagnitude = mean(avgBodyGyroMagnitude),
                       sdevBodyGyroMagnitude = mean(sdevBodyGyroMagnitude),
                       avgBodyGyroJerkMagnitude = mean(avgBodyGyroJerkMagnitude),
                       sdevBodyGyroJerkMagnitude = mean(sdevBodyGyroJerkMagnitude),
                       avgFreqBodyAccel.X = mean(avgFreqBodyAccel.X),
                       avgFreqBodyAccel.Y = mean(avgFreqBodyAccel.Y),
                       avgFreqBodyAccel.Z = mean(avgFreqBodyAccel.Z),
                       sdevFreqBodyAccel.X = mean(sdevFreqBodyAccel.X),
                       sdevFreqBodyAccel.Y = mean(sdevFreqBodyAccel.Y),
                       sdevFreqBodyAccel.Z = mean(sdevFreqBodyAccel.Z),
                       avgFreqBodyAccelJerk.X = mean(avgFreqBodyAccelJerk.X),
                       avgFreqBodyAccelJerk.Y = mean(avgFreqBodyAccelJerk.Y),
                       avgFreqBodyAccelJerk.Z = mean(avgFreqBodyAccelJerk.Z),
                       sdevFreqBodyAccelJerk.X = mean(sdevFreqBodyAccelJerk.X),
                       sdevFreqBodyAccelJerk.Y = mean(sdevFreqBodyAccelJerk.Y),
                       sdevFreqBodyAccelJerk.Z = mean(sdevFreqBodyAccelJerk.Z),
                       avgFreqBodyGyro.X = mean(avgFreqBodyGyro.X),
                       avgFreqBodyGyro.Y = mean(avgFreqBodyGyro.Y),
                       avgFreqBodyGyro.Z = mean(avgFreqBodyGyro.Z),
                       sdevFreqBodyGyro.X = mean(sdevFreqBodyGyro.X),
                       sdevFreqBodyGyro.Y = mean(sdevFreqBodyGyro.Y),
                       sdevFreqBodyGyro.Z = mean(sdevFreqBodyGyro.Z),
                       avgFreqBodyAccelMagnitude = mean(avgFreqBodyAccelMagnitude),
                       sdevFreqBodyAccelMagnitude = mean(sdevFreqBodyAccelMagnitude),
                       avgFreqBodyBodyAccelJerkMagnitude = mean(avgFreqBodyBodyAccelJerkMagnitude),
                       sdevFreqBodyBodyAccelJerkMagnitude = mean(sdevFreqBodyBodyAccelJerkMagnitude),
                       avgFreqBodyBodyGyroMagnitude = mean(avgFreqBodyBodyGyroMagnitude),
                       sdevFreqBodyBodyGyroMagnitude = mean(sdevFreqBodyBodyGyroMagnitude),
                       avgFreqBodyBodyGyroJerkMagnitude = mean(avgFreqBodyBodyGyroJerkMagnitude),
                       sdevFreqBodyBodyGyroJerkMagnitude = mean(sdevFreqBodyBodyGyroJerkMagnitude)
)

write.table(tiny_data, file="tiny_data.txt", row.names=FALSE)