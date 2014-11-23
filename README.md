GettingAndCleaningData
======================

Please refer to the README.txt file from the dataset downloaded from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip. 

The dataset is further processed using the run_analysis.R script to
 - extract only the measurements on the mean and standard deviation for each measurement from training set
 - merge this new training set with the training subjects and activity labels
 - extract only the measurements on the mean and standard deviation for each measurement from test set
 - merge this new test set with the test subjects and activity labels
 - use the descriptive names for activity labels in both the new sets
 - merge the new training and test sets to make a single set with all the data.
 - create a new dataset from this combined set with the average of each variable for each activity and each subject.
 
How to run the script:
========================

1. Download the dataset from the URL provided and extract the files into local directory.
2. Copy the run_analysis.R script to UCI HAR DataSet folder.
3. From R, set the current working directory to UCI HAR Dataset folder
4. From R, execute the run_analysis.R script.
5. The script should generate a file, UCI_HAR_Tidy_Data.txt file which contains the average of each variable for each activity and each subject.

The dataset includes the following files:
=========================================

- 'README.md'
 
- 'run_analysis.R': The R script used to process the dataset downloaded from the above URL.

- 'features_info.txt': Shows information about the variables used on the features listed in the downloaded dataset.

- 'UCI_HAR_Tidy_Data.txt': The set with the average of each variable for each activity and each subject.