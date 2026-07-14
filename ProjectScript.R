# Load the packages this script will use
library(readr); library(dplyr); library(stringr); library(reshape2); library(data.table)

# Read the training and testing data into data frames
train = read_table("~/CleaningData/UCI HAR Dataset/train/X_train.txt", col_names = FALSE)
test = read_table("~/CleaningData/UCI HAR Dataset/test/X_test.txt", col_names = FALSE)

# Combine the data frames and add variable names
data = bind_rows(train, test)
col_names = read_table("~/CleaningData/UCI HAR Dataset/features.txt", col_names = FALSE)$X2
colnames(data) = col_names

# Extract only the mean and standard deviation columns
desired_cols = grep(".*(mean|std)\\(\\)-(X|Y|Z)$", col_names, value = TRUE)
data = select(data, desired_cols)

# Add a variable indicating what activity is taking place
y_train = scan("~/CleaningData/UCI HAR Dataset/train/y_train.txt")
y_test = scan("~/CleaningData/UCI HAR Dataset/test/y_test.txt")
y = c(y_train, y_test)

name_idx =  c("1" = "WALKING", "2" = "WALKING_UPSTAIRS", 
               "3" = "WALKING_DOWNSTAIRS", "4" = "SITTING", "5" = "STANDING", 
               "6" = "LAYING")
activity = name_idx[y]

data$activity = activity

# Clean variable names to be clearer
cleaned_names = colnames(data) 

cleaned_names = cleaned_names %>% 
  gsub("\\(\\)", "", .) %>%
  gsub("-", "_", .) %>%
  gsub("Acc", "_acceleration", .) %>%
  gsub("Gyro", "_velocity", .) %>%
  gsub("Mag", "_magnitude", .) %>%
  gsub("max", "maximum", .) %>%
  gsub("min", "minimum", .) %>%
  gsub("std", "standard_deviation", .) %>%
  gsub("mad", "median_absolute_deviation", .) %>%
  gsub("sma", "signal_magnitude_area", .) %>%
  gsub("iqr", "inter_quartile_range", .) %>%
  gsub("arCoeff", "autoregression_coefficient", .) %>%
  gsub("maxInds", "maximum_frequency_index", .) %>%
  gsub("meanFreq", "mean_frequency", .) %>%
  gsub("bandsEnergy", "energy_bands", .) %>%
  tolower() 

colnames(data) = cleaned_names

# Create a new table with a column for subjects
sub_train = scan("~/CleaningData/UCI HAR Dataset/train/subject_train.txt")
sub_test = scan("~/CleaningData/UCI HAR Dataset/test/subject_test.txt")
subjects = c(sub_train, sub_test)

data2 = data
data2$subject = subjects
data2 = relocate(data2, activity, subject)

final_data = data2 %>%
  group_by(activity, subject) %>%
  summarise(across(where(is.numeric), mean), .groups = "drop")