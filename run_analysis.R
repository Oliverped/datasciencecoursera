#1.Merges the training and the test sets to create one data set.
#setwd("D:/Dropbox/Data_Science/GettingAndCleaningData/week3")

trainData <- read.table("./Dataset/train/x_train.txt")
dim(trainData) # 7352*561 OK

trainLabel <- read.table("./Dataset/train/y_train.txt")
table(trainLabel)

trainSubject <- read.table("./Dataset/train/subject_train.txt")

testData <- read.table("./Dataset/test/X_test.txt")
dim(testData) # 2947*561 OK

testLabel <- read.table("./Dataset/test/y_test.txt") 
table(testLabel) 

testSubject <- read.table("./Dataset/test/subject_test.txt")

joinData <- rbind(trainData, testData)
dim(joinData) # 10299*561 OK

joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel) # 10299*1 OK

joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject) # 10299*1 OK




#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("./Dataset/features.txt")
dim(features)  # 561*2 OK

meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])

length(meanStdIndices) # 66 OK

joinData <- joinData[, meanStdIndices]
dim(joinData) # 10299*66 OK


names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 


#3.Uses descriptive activity names to name the activities in the data set
activity <- read.table("./Dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))

activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"


#4.Appropriately labels the data set with descriptive activity names
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData) # 10299*68
write.table(cleanedData, "mergedData.txt") # write out the 1st dataset

#5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
        for(j in 1:activityLen) {
                result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
                result[row, 2] <- activity[j, 2]
                bool1 <- i == cleanedData$subject
                bool2 <- activity[j, 2] == cleanedData$activity
                result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
                row <- row + 1
        }
}

#head(result)
write.table(result, "dataWithMeans.txt") # write out the 2nd dataset

# data <- read.table("./dataWithMeans.txt")
# data[1:12, 1:3]
