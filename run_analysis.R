run_analysis.R <- function() {
    ## This function uses the test data downloaded from https://d396qusza40orc.
    ## cloudfront.net/getdata/projectfiles/UCI HAR Dataset.zip and    
    ## 1. Merges the training and the test sets to create one data set.
    ## 2. Extracts only the measurements on the mean and standard deviation for 
    ##    each measurement. 
    ## 3. Uses descriptive activity names to name the activities in the data set
    ## 4. Appropriately labels the data set with descriptive variable names. 
    ## 5. From the data set in step 4, creates a second, independent tidy data 
    ##    set with the average of each variable for each activity and each subject.
    
    ## This function uses plyr library.
    ## The result data set from step 5 is written into a file 
    ## UCI_HAR_Tidy_Data.txt located in the same directory as of this script.
    
    
    # Read the activity labels into a data frame. 
    # First column is the activity label and second column is activity name
    al <- read.csv("activity_labels.txt", sep=" ", header=FALSE, 
                   stringsAsFactors=F)
    
    ############## Handling Training Data Sets ########################
    
    # Read the training labels into a data frame.
    # Each row has single column indicating the activity label for each record
    ytrain <-read.csv("train/y_train.txt", header=FALSE)
    
    # Replace the activity label with the activity name in the training labels
    for (aid in al$V1) {
        ytrain[ytrain$V1 == aid,] <- al[al$V1 == aid, 2]
    }
    
    # Read the training subjects into a data frame.
    # Each row has single column and it identifies the subject for each record
    train_subjects <- read.csv("train/subject_train.txt", header=FALSE)
    
    # Read the training set into a data frame.
    # Read only the columns that have mean or standard deviation measurement.
    xtrain <- read.csv("train/X_train.txt", sep="", header=FALSE, 
                       colClasses = c(rep(NA, 6), rep("NULL", 34), rep(NA, 6), 
                                      rep("NULL", 34), rep(NA, 6), rep("NULL", 34), 
                                      rep(NA, 6), rep("NULL", 34), rep(NA, 6), 
                                      rep("NULL", 34), NA, NA, rep("NULL", 11), 
                                      NA, NA, rep("NULL", 11), NA, NA, 
                                      rep("NULL", 11), NA, NA, rep("NULL", 11), 
                                      NA, NA, rep("NULL", 11), rep(NA, 6), 
                                      rep("NULL", 73), rep(NA, 6), rep("NULL", 73), 
                                      rep(NA, 6), rep("NULL", 73), NA, NA, 
                                      rep("NULL", 11), NA, NA, rep("NULL", 11), 
                                      NA, NA, rep("NULL", 11), NA, NA, 
                                      rep("NULL", 18)))
    
    # Create the training dataset by joining the activity name, subjects and 
    # training set.
    training_set <- cbind(ytrain, train_subjects, xtrain)
    
    ################ Handling Test Data Sets ##########################
    
    # Read the test labels into a data frame.
    # Each row has single column indicating the activity label for each record
    ytest <-read.csv("test/y_test.txt", header=FALSE)
    
    # Replace the activity label with the activity name in the training labels
    for (aid in al$V1) {
        ytest[ytest$V1 == aid,] <- al[al$V1 == aid, 2]
    }
    
    # Read the test subjects into a data frame.
    # Each row has single column and it identifies the subject for each record
    test_subjects <- read.csv("test/subject_test.txt", header=FALSE)
    
    # Read the test set into a data frame.
    # Read only the columns that have mean or standard deviation measurement.
    xtest <- read.csv("test/X_test.txt", sep="", header=FALSE, 
                      colClasses = c(rep(NA, 6), rep("NULL", 34), rep(NA, 6), 
                                     rep("NULL", 34), rep(NA, 6), rep("NULL", 34), 
                                     rep(NA, 6), rep("NULL", 34), rep(NA, 6), 
                                     rep("NULL", 34), NA, NA, rep("NULL", 11), 
                                     NA, NA, rep("NULL", 11), NA, NA, 
                                     rep("NULL", 11), NA, NA, rep("NULL", 11), 
                                     NA, NA, rep("NULL", 11), rep(NA, 6), 
                                     rep("NULL", 73), rep(NA, 6), rep("NULL", 73), 
                                     rep(NA, 6), rep("NULL", 73), NA, NA, 
                                     rep("NULL", 11), NA, NA, rep("NULL", 11), 
                                     NA, NA, rep("NULL", 11), NA, NA, 
                                     rep("NULL", 18)))
    
    # Make the test dataset by joining the activity name, subjects and test set.
    test_set <- cbind(ytest, test_subjects, xtest)
    
    ############## Create and Process Whole Data Set ####################
    
    # Merge the training and test data sets
    whole_set <- rbind(training_set, test_set)
    
    # Name the measurements in this data set.
    # Get the column names by 
    #  a. Reading the features.txt file to get the measurement names 
    #  b. Filtering out the measurement names that are named as mean or standard 
    #     deviation 
    #  c. Removing the parenthesis in the names 
    #  d. Replacing - (hyphen) with _ (underscore) in the names.
    
    all_features <- read.csv("features.txt", sep="", header=FALSE, 
                             stringsAsFactors=F)
    mean_sd_features <- subset(all_features, 
                               grepl("mean\\(\\)", V2) | grepl("std\\(\\)", V2))
    names(whole_set) <- c("activity", 
                          "subject", 
                          t(as.matrix(sub("\\(\\)", 
                                          "", 
                                          mean_sd_features$V2)))[1,])
    names(whole_set) <- gsub("-", "_", names(whole_set))
    
    # Create a tidy data set with the average of each variable for each activity 
    # and each subject.
    
    # include plyr library to use ddply
    
    library(plyr)
    tidy_data <- ddply(whole_set, c("activity", "subject"), summarise, 
                       meanOf_tBodyAcc_mean_X =  mean(tBodyAcc_mean_X),
                       meanOf_tBodyAcc_mean_Y =  mean(tBodyAcc_mean_Y),
                       meanOf_tBodyAcc_mean_Z =  mean(tBodyAcc_mean_Z),
                       meanOf_tBodyAcc_std_X =  mean(tBodyAcc_std_X),
                       meanOf_tBodyAcc_std_Y =  mean(tBodyAcc_std_Y),
                       meanOf_tBodyAcc_std_Z =  mean(tBodyAcc_std_Z),
                       meanOf_tGravityAcc_mean_X =  mean(tGravityAcc_mean_X),
                       meanOf_tGravityAcc_mean_Y =  mean(tGravityAcc_mean_Y),
                       meanOf_tGravityAcc_mean_Z =  mean(tGravityAcc_mean_Z),
                       meanOf_tGravityAcc_std_X =  mean(tGravityAcc_std_X),
                       meanOf_tGravityAcc_std_Y =  mean(tGravityAcc_std_Y),
                       meanOf_tGravityAcc_std_Z =  mean(tGravityAcc_std_Z),
                       meanOf_tBodyAccJerk_mean_X =  mean(tBodyAccJerk_mean_X),
                       meanOf_tBodyAccJerk_mean_Y =  mean(tBodyAccJerk_mean_Y),
                       meanOf_tBodyAccJerk_mean_Z =  mean(tBodyAccJerk_mean_Z),
                       meanOf_tBodyAccJerk_std_X =  mean(tBodyAccJerk_std_X),
                       meanOf_tBodyAccJerk_std_Y =  mean(tBodyAccJerk_std_Y),
                       meanOf_tBodyAccJerk_std_Z =  mean(tBodyAccJerk_std_Z),
                       meanOf_tBodyGyro_mean_X =  mean(tBodyGyro_mean_X),
                       meanOf_tBodyGyro_mean_Y =  mean(tBodyGyro_mean_Y),
                       meanOf_tBodyGyro_mean_Z =  mean(tBodyGyro_mean_Z),
                       meanOf_tBodyGyro_std_X =  mean(tBodyGyro_std_X),
                       meanOf_tBodyGyro_std_Y =  mean(tBodyGyro_std_Y),
                       meanOf_tBodyGyro_std_Z =  mean(tBodyGyro_std_Z),
                       meanOf_tBodyGyroJerk_mean_X =  mean(tBodyGyroJerk_mean_X),
                       meanOf_tBodyGyroJerk_mean_Y =  mean(tBodyGyroJerk_mean_Y),
                       meanOf_tBodyGyroJerk_mean_Z =  mean(tBodyGyroJerk_mean_Z),
                       meanOf_tBodyGyroJerk_std_X =  mean(tBodyGyroJerk_std_X),
                       meanOf_tBodyGyroJerk_std_Y =  mean(tBodyGyroJerk_std_Y),
                       meanOf_tBodyGyroJerk_std_Z =  mean(tBodyGyroJerk_std_Z),
                       meanOf_tBodyAccMag_mean =  mean(tBodyAccMag_mean),
                       meanOf_tBodyAccMag_std =  mean(tBodyAccMag_std),
                       meanOf_tGravityAccMag_mean =  mean(tGravityAccMag_mean),
                       meanOf_tGravityAccMag_std =  mean(tGravityAccMag_std),
                       meanOf_tBodyAccJerkMag_mean =  mean(tBodyAccJerkMag_mean),
                       meanOf_tBodyAccJerkMag_std =  mean(tBodyAccJerkMag_std),
                       meanOf_tBodyGyroMag_mean =  mean(tBodyGyroMag_mean),
                       meanOf_tBodyGyroMag_std =  mean(tBodyGyroMag_std),
                       meanOf_tBodyGyroJerkMag_mean =  mean(tBodyGyroJerkMag_mean),
                       meanOf_tBodyGyroJerkMag_std =  mean(tBodyGyroJerkMag_std),
                       meanOf_fBodyAcc_mean_X =  mean(fBodyAcc_mean_X),
                       meanOf_fBodyAcc_mean_Y =  mean(fBodyAcc_mean_Y),
                       meanOf_fBodyAcc_mean_Z =  mean(fBodyAcc_mean_Z),
                       meanOf_fBodyAcc_std_X =  mean(fBodyAcc_std_X),
                       meanOf_fBodyAcc_std_Y =  mean(fBodyAcc_std_Y),
                       meanOf_fBodyAcc_std_Z =  mean(fBodyAcc_std_Z),
                       meanOf_fBodyAccJerk_mean_X =  mean(fBodyAccJerk_mean_X),
                       meanOf_fBodyAccJerk_mean_Y =  mean(fBodyAccJerk_mean_Y),
                       meanOf_fBodyAccJerk_mean_Z =  mean(fBodyAccJerk_mean_Z),
                       meanOf_fBodyAccJerk_std_X =  mean(fBodyAccJerk_std_X),
                       meanOf_fBodyAccJerk_std_Y =  mean(fBodyAccJerk_std_Y),
                       meanOf_fBodyAccJerk_std_Z =  mean(fBodyAccJerk_std_Z),
                       meanOf_fBodyGyro_mean_X =  mean(fBodyGyro_mean_X),
                       meanOf_fBodyGyro_mean_Y =  mean(fBodyGyro_mean_Y),
                       meanOf_fBodyGyro_mean_Z =  mean(fBodyGyro_mean_Z),
                       meanOf_fBodyGyro_std_X =  mean(fBodyGyro_std_X),
                       meanOf_fBodyGyro_std_Y =  mean(fBodyGyro_std_Y),
                       meanOf_fBodyGyro_std_Z =  mean(fBodyGyro_std_Z),
                       meanOf_fBodyAccMag_mean =  mean(fBodyAccMag_mean),
                       meanOf_fBodyAccMag_std =  mean(fBodyAccMag_std),
                       meanOf_fBodyBodyAccJerkMag_mean =  mean(fBodyBodyAccJerkMag_mean),
                       meanOf_fBodyBodyAccJerkMag_std =  mean(fBodyBodyAccJerkMag_std),
                       meanOf_fBodyBodyGyroMag_mean =  mean(fBodyBodyGyroMag_mean),
                       meanOf_fBodyBodyGyroMag_std =  mean(fBodyBodyGyroMag_std),
                       meanOf_fBodyBodyGyroJerkMag_mean =  mean(fBodyBodyGyroJerkMag_mean),
                       meanOf_fBodyBodyGyroJerkMag_std =  mean(fBodyBodyGyroJerkMag_std))
    
    write.table(tidy_data, "UCI_HAR_Tidy_Data.txt", row.name=FALSE)
}
