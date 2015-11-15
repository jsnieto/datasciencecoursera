# Tidying the UCI_HAR dataset

The goal of this project is to prepare tidy data from the Human Activity Recognition Using Smartphones Dataset. Information about the dataset can be found [here](
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones). The dataset can be downloaded from the following [link](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip). In order for the script to work the data should be unzipped maintaining its native structure into the R working directory.

### How the script works
The first part of the script stores the filepaths of the required files in appropiate variables for further use:


```{r}
library(reshape2)
train_file <- "./UCI HAR Dataset/train/X_train.txt"
train_subject_file <- "./UCI HAR Dataset/train/subject_train.txt"
train_activity_file <- "./UCI HAR Dataset/train/y_train.txt"
test_file <- "./UCI HAR Dataset/test/X_test.txt"
test_subject_file <- "./UCI HAR Dataset/test/subject_test.txt"
test_activity_file <- "./UCI HAR Dataset/test/y_test.txt"
header_file <- "./UCI HAR Dataset/features.txt"
activity_file <- "./UCI HAR Dataset/activity_labels.txt"
```
Next, the auxiliary files containing labels and Id identification numbers are loaded into R datasets.

```{r}
col_names <- read.table(header_file, header = FALSE, colClasses = c("NULL", "character"), col.names = c("Id", "Variable"))
test_subject_id <- read.table(test_subject_file, header = FALSE, colClasses = c("numeric"), col.names = c("SUBJECT_ID"))
train_subject_id <- read.table(train_subject_file, header = FALSE, colClasses = c("numeric"), col.names = c("SUBJECT_ID"))
activity_labels <- read.table(activity_file, header = FALSE, colClasses = c("numeric", "character"), col.names = c("ACTIVITY_ID", "ACTIVITY"))
train_activity <- read.table(train_activity_file, header = FALSE, colClasses = c("numeric"), col.names = c("ACTIVITY_ID"))
test_activity <- read.table(test_activity_file, header = FALSE, colClasses = c("numeric"), col.names = c("ACTIVITY_ID"))
```
Now to make the job easier, some of the previously loaded datasets are modified and merged. A column indicating the order of the observation is added to keep track of the rows and reorder after the merging procedure. The objective here is to pair descriptive activity names to activity IDs.

```{r}
col_names <- as.character(col_names[, 1])
activity_labels[, 2] <- tolower(activity_labels[, 2])
train_activity$ORDER <- 1:nrow(train_activity)
test_activity$ORDER <- 1:nrow(test_activity)
train_activity <- merge(train_activity, activity_labels)
train_activity <- train_activity[order(train_activity$ORDER), ]
test_activity <- merge(test_activity, activity_labels)
test_activity <- test_activity[order(test_activity$ORDER), ]
```

Finally, the main datasets (train and test) are loaded into R. 
```{r}
trainData <- read.table(train_file, header = FALSE, col.names = col_names)
testData <- read.table(test_file, header = FALSE, col.names = col_names)
```
The objective now is to subset only the measurements coresponding to mean and standard deviations values and merge the train and test datasets into one. To do this the following lines of code first subset the required measured values by using the grep() function. Afterwards new columns are introduced in the datasets to accomodate the auxiliry information (SUBJECT_ID, ACTIVITY, OBS_NUMBER). The last value is used to keep track of the order after merging the datasets. The last two line of code just change the clummn names to make them more manageble.
```{r}
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
```

Once the merged dataset is generated we proceed to tidy it. The main problem is that the column headers are values, not variable names. To fix this we melt the dataset and add new columns to acomodate the values that are now going to be extracted from the melted column.

```{r}
meltedData <- melt(mergedData, id.vars = names(mergedData)[67:70], variable.name = "PARAMETER", value.name = "MEASURED_VALUE")
meltedData$DOMAIN <- paste(character(nrow(meltedData)), "time", sep = "")
meltedData$DOMAIN[substring(meltedData$PARAMETER, 1, 1) == "f"] <- "frequency"
meltedData$AXIS <- paste(character(nrow(meltedData)), "mag", sep = "")
meltedData$AXIS[substring(meltedData$PARAMETER, nchar(as.character(meltedData$PARAMETER)) , nchar(as.character(meltedData$PARAMETER))) == "X"] <- "x"
meltedData$AXIS[substring(meltedData$PARAMETER, nchar(as.character(meltedData$PARAMETER)) , nchar(as.character(meltedData$PARAMETER))) == "Y"] <- "y"
meltedData$AXIS[substring(meltedData$PARAMETER, nchar(as.character(meltedData$PARAMETER)) , nchar(as.character(meltedData$PARAMETER))) == "Z"] <- "z"
meltedData$STANDARD_DEVIATION[grep("std", meltedData$PARAMETER)] <- meltedData$MEASURED_VALUE[grep("std", meltedData$PARAMETER)]
meltedData <- meltedData[order(meltedData$DATA_TYPE, meltedData$OBS_NUMBER, meltedData$PARAMETER), ]
```

The created melted data set still has the values corresponding to mean and standard deviation in the same column. To fixed this we first split the melted data set in two parts. We then change the column names in the two seperated datasets to allow the use of the merge function once more time.

```{r}
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
```
The last part of the code merges the two previously created datasets into the final tidy one. The colum names are changed an reorder. The scrip prints the tidy dataset to a file.

```{r}
tidyData <- merge(splitData1, splitData2)
tidyData$PARAMETER[substring(tidyData$PARAMETER, nchar(tidyData$PARAMETER), nchar(tidyData$PARAMETER)) == "M"] <- substring(tidyData$PARAMETER[substring(tidyData$PARAMETER, nchar(tidyData$PARAMETER), nchar(tidyData$PARAMETER)) == "M"], 1, nchar(tidyData$PARAMETER[substring(tidyData$PARAMETER, nchar(tidyData$PARAMETER), nchar(tidyData$PARAMETER)) == "M"])-1 )
tidyData <- tidyData[order(tidyData$DATA_TYPE, tidyData$OBS_NUMBER, tidyData$PARAMETER), ]
tidyData <- tidyData[order(tidyData$DOMAIN, decreasing = TRUE), ]
tidyData <- tidyData[, c(1, 3, 2, 5, 7, 6, 8, 9, 4)]
tidyData$OBS_NUMBER <- NULL
write.table(tidyData, file = "./UCI_HAR_tidyDataset.txt", row.name = FALSE, quote = FALSE, sep = "\t")
```
### Code book

You can find the codebook of the tidy dataset in the codebook.txt file on this same repo.
