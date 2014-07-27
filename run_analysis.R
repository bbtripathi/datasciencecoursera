

## Function to return the file name of the data set
## ASSUME THAT THE UCI HAR DATA SET FOLDER IS IN THE CURRENT DIRECTORY

readFileName <- function (type, x = X) {
        dataDir <- paste(getwd(), "/UCI HAR Dataset/", sep="")
        if (type == "features") {
                fileName <- paste(dataDir, "features.txt", sep="")
        } else if (type == "activities") {
                fileName <- paste(dataDir, "activity_labels.txt", sep="") 
        } else {
                fileName <- paste(dataDir, type,"/",x,"_",type,".txt", sep="") 
        }
}

## 1. CREATE ONE DATA SET BY MERGING TEST AND TRAIN DATA SETS
## read the features data
features <- read.table(readFileName("features"))

## read the activities label data
activities <- read.table(readFileName("activities"))

## Read test data set
testX <- read.table(readFileName("test", "X"))
testY <- read.table(readFileName("test", "y"))
testSubject <- read.table(readFileName("test", "subject"))

## read training data set
trainX <- read.table(readFileName("train", "X"))
trainY <- read.table(readFileName("train", "y"))
trainSubject <- read.table(readFileName("train", "subject"))

## combine individual data sets
dataX <- rbind(trainX, testX)
dataY <- rbind(trainY, testY)
dataSubject <- rbind(trainSubject, testSubject)

## 4. LABEL DATA SET WITH DESCRIPTIVE VARIABLE NAMES
## apply labels to the data
names(dataX) <- features[,2]
names(dataY) <- "Activity"
names(dataSubject) <- "Subject"
names(activities) <- c("Activity","ActivityName")

## 3. USES DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATA SET
dataY <- merge(dataY, activities)

## combine into one data set
harData <- cbind(dataX, dataY, dataSubject)

## clear datasets from memory
rm(testX, testY, testSubject, trainX, trainY, trainSubject)
rm(dataX, dataY, dataSubject)

## 2. EXTRACT MEAN AND STANDARD DEVIATION MEASUREMENTS

## find features with mean or std in their name
stdmeanF <- grep("std|mean", names(harData), ignore.case=TRUE)

## now extract only those Extracts only the measurements on the mean and 
## standard deviation for each measurement.
stdmeanData <- harData[,stdmeanF]

## add the features and subject to this
stdmeanData <- cbind(stdmeanData, harData$Activity, harData$Subject)
colnames(stdmeanData)[which(names(stdmeanData) == "harData$Activity")] <- "Activity"
colnames(stdmeanData)[which(names(stdmeanData) == "harData$Subject")] <- "Subject"


## 5. SECOND TIDY DATA SET WITH AVERAGE each variable for each activity and each subject.
dt <- data.table(stdmeanData)
tidyData<- dt[, lapply(.SD, mean), by=c("Subject", "Activity")]

## add descriptive activity names
tidyData <- merge(tidyData, activities, by="Activity")

## write the data to a file
write.table(tidyData,file="tidyHARData.csv",sep=",",row.names = FALSE)



