library(dplyr)
library(reshape2)
##loading the test and train data
test <- read.table("./test/X_test.txt",sep="",strip.white = TRUE)
train <- read.table("./train/X_train.txt",sep="",strip.white = TRUE)
##merginging both into one dataset
totaldata <- rbind(test,train)
##loading the features table
features <- read.table("./features.txt",sep="\n",strip.white = TRUE)
features <- features$V1
features <- as.character(features)
##removing the numbers from the features
features <- sub("^\\d+\\s","",features)
##labelling the columns of totaldata
colnames(totaldata)<- features
##Stripping columns which do not contain mean and standard ddeviation
req_ind <- grep("mean\\(\\)|std\\(\\)",colnames(totaldata))
totalmeanstd <- totaldata[,req_ind]
## loading and merging the subject and activity respectively
test_y <- read.table("./test/y_test.txt",sep="",strip.white = TRUE)
test_sub <- read.table("./test/subject_test.txt",sep="",strip.white = TRUE)
train_y <- read.table("./train/y_train.txt",sep="",strip.white = TRUE)
train_sub <- read.table("./train/subject_train.txt",sep="",strip.white = TRUE)
total_y <- rbind(test_y,train_y)
total_sub <- rbind(test_sub,train_sub)
## the descriptive activity labels
activity_labels <- read.table("./activity_labels.txt",sep=" ",strip.white = TRUE)
total_rowdata <- cbind(cbind(total_y,total_sub),totalmeanstd)
names(total_rowdata)[1:2] <- c("exercise","subject")
##Renaming the descriptive names for activity
total_rowdata <- merge(activity_labels,total_rowdata,by.x="V1",by.y="exercise",all.y = TRUE)
total_rowdata <- total_rowdata[,-1]
names(total_rowdata)[1] <- "exercise"
## The data ic converted to required format
by_activity <- melt(total_rowdata,id.vars = c("activity","subject"))
##Grouping the data by activity and subject
grouped_by_activity <- group_by(by_activity,activity,subject,variable)
##Resultant summary statistics
result <- summarise(grouped_by_activity,mean(value))
