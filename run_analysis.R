## This function will clear the data from "Human Activity Recognition Using Smartphones Dataset - Version 1.0" 
# and generate average of mean & std values for each subject and each activity
# The working directory should point to the path of directory 'UCI HAR Dataset' where Samsung data file exist
run_analysis <- function (){

library(dplyr)
library(stringr)

setwd("C:\\Users\\edwards\\OneDrive - PSA International\\ADAPT\\R working\\course getting and cleaning data\\week4\\UCI HAR Dataset")

# 1. Merges the training and the test sets to create one data set.
dsall <- rbind(read.table("test\\X_test.txt"),read.table("train\\X_train.txt"))
subject <- rbind(read.table("test\\subject_test.txt"),read.table("train\\subject_train.txt"))$V1
lbln <- rbind(read.table("test\\y_test.txt"),read.table("train\\y_train.txt"))$V1
dsall <- cbind(dsall, subject, lbln)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement
fl <- read.table("features.txt")
mnstd <- rbind(fl %>% filter(str_detect(V2,"\\-std\\(")),fl %>% filter(str_detect(V2,"\\-mean\\(")))
mnstd <- mnstd[order(mnstd$V1),]
ind <- c(mnstd$V1,562,563)
dsall_sub <- dsall[,ind]


# 3. Uses descriptive activity names to name the activities in the data set
label <- rep(NA,nrow(dsall_sub))
dsall_sub <- data.frame(dsall_sub,label)
lbl <- read.table("activity_labels.txt")
dsall_sub$label <- lbl$V2[match(dsall_sub$lbln,lbl$V1)]

# 4. Appropriately labels the data set with descriptive variable names.
colnames(dsall_sub) <- c(gsub("\\(\\)","",mnstd$V2),"subject","actn","activity")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
dsall_grp <- data.frame(dsall_sub %>% group_by(subject,activity) %>% summarise_at(.vars = names(.)[1:66],list(~mean(., na.rm=TRUE))) )

## writing the data frame from step #5 to file
write.table(dsall_grp,"subject_activity_mean.txt", row.name=FALSE)
}
