#load libraries
install.packages("dplyr")
library(dplyr)

#download and unzip data
if(file.exists("./acc_data") == FALSE) {
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","./data.zip")
    unzip("./data.zip", exdir="./acc_data")
}

#load standard data files
activity_labels <- read.csv("./acc_data/UCI HAR Dataset/activity_labels.txt", sep = " ", 
                            header = FALSE, col.names = c("activity_id", "activity_name"))
feature_labels <- read.csv("./acc_data/UCI HAR Dataset/features.txt", sep = " ",
                           header = FALSE, col.names = c("feature_id", "feature_label"))

#load test data using read.table
subject_test <- read.table("./acc_data/UCI HAR Dataset/test/subject_test.txt", col.names = c("subject_id"))
X_test <- read.table("./acc_data/UCI HAR Dataset/test/X_test.txt", col.names = feature_labels$feature_label)
y_test <- read.table("./acc_data/UCI HAR Dataset/test/y_test.txt", col.names = c("activity_id"))

#load train data
subject_train <- read.table("./acc_data/UCI HAR Dataset/train/subject_train.txt", col.names = c("subject_id"))
X_train <- read.table("./acc_data/UCI HAR Dataset/train/X_train.txt", col.names = feature_labels$feature_label)
y_train <- read.table("./acc_data/UCI HAR Dataset/train/y_train.txt", col.names = c("activity_id"))

#get list of mean and stdev columns
column_list <- grep("\\.([Mm][Ee][Aa][Nn]|[Ss][Tt][Dd])\\.{2}", names(X_test), value=TRUE)

#clean up test data
test_data <- y_test %>% 
    inner_join(activity_labels, by = "activity_id") %>% #get activity names
    bind_cols(subject_test, X_test) %>% #merge with other data
    select(all_of(c(names(subject_test), "activity_name", column_list))) %>% #select only mean+stdev
    rename_with( ~ gsub("\\.","_",gsub("\\.{2,3}","_",gsub("\\.{2}$","",.x)))) #replace dots in colnames with underscores

#clean up train data
train_data <- y_train %>% 
    inner_join(activity_labels, by = "activity_id") %>% #get activity names
    bind_cols(subject_train, X_train) %>% #merge with other data
    select(all_of(c(names(subject_train), "activity_name", column_list))) %>% #select only mean+stdev
    rename_with( ~ gsub("\\.","_",gsub("\\.{2,3}","_",gsub("\\.{2}$","",.x)))) #replace dots in colnames with underscores
    
#combine datasets
full_data <- bind_rows(test_data, train_data)
write.table(full_data, "./acc_data/UCI HAR Dataset/combined_data.txt", row.names = FALSE)

#groupdcwec
grouped_data <- full_data %>% group_by(subject_id, activity_name) %>% summarize_all(mean, na.rm = TRUE)
write.table(grouped_data, "./acc_data/UCI HAR Dataset/grouped_data.txt", row.names = FALSE)