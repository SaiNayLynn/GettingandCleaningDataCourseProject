This is created for Peer-graded Assignment for "Getting and Cleaning Data Course Project" by Sai Nay Lynn.

Running the R program to perform data analysis

R file name: run_analysis.R
R Function: run_analysis()
Preparation before calling the run_analysis() function:
- setwd("<the path to directory 'UCI HAR Dataset' where Samsung data file exist>")
- put the R file "run_analysis.R" into above directory
- source("run_analysis.R")
- run_analysis()

Description of the DATA

Output file: subject_activity_mean.txt

1. leading t or f is based on time or frequency measurements.
2. Body = related to body movement.
3. Gravity = acceleration of gravity
4. Acc = accelerometer measurement
5. Gyro = gyroscopic measurements
6. Jerk = sudden movement acceleration
7. Mag = magnitude of movement
8. mean and SD are calculated for each subject for each activity for each mean and SD measurements.

The units given are g�s for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the corresponding jerks.

These signals were used to estimate variables of the feature vector for each pattern:
�-XYZ� is used to denote 3-axial signals in the X, Y and Z directions. They total 33 measurements including the 3 dimensions - the X,Y, and Z axes.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

The set of variables that were estimated from these signals are:

mean(): Mean value
std(): Standard deviation

The values in output data file are the average of each variable for each activity and each subject.

