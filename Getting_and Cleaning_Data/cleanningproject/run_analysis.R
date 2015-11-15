
########################################################################################
### The data files should be in the working directory.
### This part of the script assigns the filepaths to respective variables.
########################################################################################
library(reshape2)
train_file <- "./UCI HAR Dataset/train/X_train.txt"
train_subject_file <- "./UCI HAR Dataset/train/subject_train.txt"
train_activity_file <- "./UCI HAR Dataset/train/y_train.txt"
test_file <- "./UCI HAR Dataset/test/X_test.txt"
test_subject_file <- "./UCI HAR Dataset/test/subject_test.txt"
test_activity_file <- "./UCI HAR Dataset/test/y_test.txt"
header_file <- "./UCI HAR Dataset/features.txt"
activity_file <- "./UCI HAR Dataset/activity_labels.txt"

########################################################################################
### This part of the script loads the auxiliary information files in to R datasets
########################################################################################

col_names <- read.table(header_file, header = FALSE, colClasses = c("NULL", "character"), col.names = c("Id", "Variable"))
test_subject_id <- read.table(test_subject_file, header = FALSE, colClasses = c("numeric"), col.names = c("SUBJECT_ID"))
train_subject_id <- read.table(train_subject_file, header = FALSE, colClasses = c("numeric"), col.names = c("SUBJECT_ID"))
activity_labels <- read.table(activity_file, header = FALSE, colClasses = c("numeric", "character"), col.names = c("ACTIVITY_ID", "ACTIVITY"))
train_activity <- read.table(train_activity_file, header = FALSE, colClasses = c("numeric"), col.names = c("ACTIVITY_ID"))
test_activity <- read.table(test_activity_file, header = FALSE, colClasses = c("numeric"), col.names = c("ACTIVITY_ID"))

########################################################################################
### This part of the script modifies some datasets to make things easier
### The column "ORDER" is added to keeep track of the observations
########################################################################################

col_names <- as.character(col_names[, 1])
activity_labels[, 2] <- tolower(activity_labels[, 2])
train_activity$ORDER <- 1:nrow(train_activity)
test_activity$ORDER <- 1:nrow(test_activity)
train_activity <- merge(train_activity, activity_labels)
train_activity <- train_activity[order(train_activity$ORDER), ]
test_activity <- merge(test_activity, activity_labels)
test_activity <- test_activity[order(test_activity$ORDER), ]

########################################################################################
### This part of the script loads the main data files into R datasets
########################################################################################

trainData <- read.table(train_file, header = FALSE, col.names = col_names)
testData <- read.table(test_file, header = FALSE, col.names = col_names)

########################################################################################
### This part subsets the main data and merges the "test" and "training" datasets
### The column "OBS_NUMBER" is introduced to match the observations after merging
########################################################################################

sub_testData <- testData[, c(grep("^.*mean\\()", col_names, value = FALSE ), grep("^.*std\\()", col_names, value = FALSE ))]
sub_trainData <- trainData[, c(grep("^.*mean\\()", col_names, value = FALSE ), grep("^.*std\\()", col_names, value = FALSE ))]
sub_trainData$DATA_TYPE <- paste(character(length(sub_trainData[, 1])), "train", sep = "")
sub_testData$DATA_TYPE <- paste(character(length(sub_testData[, 1])), "test", sep = "")
sub_trainData$SUBJECT_ID <- train_subject_id[, 1]
sub_testData$SUBJECT_ID <- test_subject_id[, 1]
sub_trainData$ACTIVITY <- train_activity$ACTIVITY
sub_testData$ACTIVITY <- test_activity$ACTIVITY
sub_trainData$ACTIVITY <- train_activity$ACTIVITY
sub_testData$ACTIVITY <- test_activity$ACTIVITY
sub_trainData$OBS_NUMBER <- c(1:nrow(sub_trainData))
sub_testData$OBS_NUMBER <- c(1:nrow(sub_testData))
mergedData <- merge(sub_trainData, sub_testData, all = TRUE)
names(mergedData) <- gsub("...", ".", fixed = TRUE, names(mergedData))
names(mergedData) <- gsub("..", "", fixed = TRUE, names(mergedData))

########################################################################################
### This part of the script melts the previosly merged dataset and creates new
### columns to start cleaning the dataset
########################################################################################

meltedData <- melt(mergedData, id.vars = names(mergedData)[67:70], variable.name = "PARAMETER", value.name = "MEASURED_VALUE")
meltedData$DOMAIN <- paste(character(nrow(meltedData)), "time", sep = "")
meltedData$DOMAIN[substring(meltedData$PARAMETER, 1, 1) == "f"] <- "frequency"
meltedData$AXIS <- paste(character(nrow(meltedData)), "mag", sep = "")
meltedData$AXIS[substring(meltedData$PARAMETER, nchar(as.character(meltedData$PARAMETER)) , nchar(as.character(meltedData$PARAMETER))) == "X"] <- "x"
meltedData$AXIS[substring(meltedData$PARAMETER, nchar(as.character(meltedData$PARAMETER)) , nchar(as.character(meltedData$PARAMETER))) == "Y"] <- "y"
meltedData$AXIS[substring(meltedData$PARAMETER, nchar(as.character(meltedData$PARAMETER)) , nchar(as.character(meltedData$PARAMETER))) == "Z"] <- "z"
meltedData$STANDARD_DEVIATION[grep("std", meltedData$PARAMETER)] <- meltedData$MEASURED_VALUE[grep("std", meltedData$PARAMETER)]
meltedData <- meltedData[order(meltedData$DATA_TYPE, meltedData$OBS_NUMBER, meltedData$PARAMETER), ]

##########################################################################################
### This part of the script splits the melted dataset in two and changes column names
##########################################################################################

splitData1 <- meltedData[is.na(meltedData$STANDARD_DEVIATION), ]
splitData2 <- meltedData[!is.na(meltedData$STANDARD_DEVIATION), ]
splitData2$PARAMETER <- gsub(".std" ,"", splitData2$PARAMETER, fixed = TRUE)
splitData1$PARAMETER <- gsub(".mean" ,"", splitData2$PARAMETER, fixed = TRUE)
splitData1$PARAMETER <- substring(splitData1$PARAMETER, 2, nchar(splitData1$PARAMETER)-2)
splitData2$PARAMETER <- substring(splitData2$PARAMETER, 2, nchar(splitData2$PARAMETER)-2)
names(splitData1)[6] <- "MEAN_VALUE"
splitData1$STANDARD_DEVIATION <- NULL
names(splitData2)[6] <- "STANDARD_DEVIATION"
splitData2$STANDARD_DEVIATION <- NULL

##########################################################################################
### The two previously seperated datasets are merged together to generate a tidy dataset
##########################################################################################

tidyData <- merge(splitData1, splitData2)
tidyData$PARAMETER[substring(tidyData$PARAMETER, nchar(tidyData$PARAMETER), nchar(tidyData$PARAMETER)) == "M"] <- substring(tidyData$PARAMETER[substring(tidyData$PARAMETER, nchar(tidyData$PARAMETER), nchar(tidyData$PARAMETER)) == "M"], 1, nchar(tidyData$PARAMETER[substring(tidyData$PARAMETER, nchar(tidyData$PARAMETER), nchar(tidyData$PARAMETER)) == "M"])-1 )
tidyData <- tidyData[order(tidyData$DATA_TYPE, tidyData$OBS_NUMBER, tidyData$PARAMETER), ]
tidyData <- tidyData[order(tidyData$DOMAIN, decreasing = TRUE), ]
tidyData <- tidyData[, c(1, 3, 2, 5, 7, 6, 8, 9, 4)]
tidyData$OBS_NUMBER <- NULL
write.table(tidyData, file = "./UCI_HAR_tidyDataset.txt", row.name = FALSE, quote = FALSE, sep = "\t")
