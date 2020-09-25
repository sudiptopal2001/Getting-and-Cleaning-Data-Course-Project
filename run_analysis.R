activities <- read.table("Dataset\\activity_labels.txt", col.names = c("code", "activity"))
features <- read.table("Dataset\\features.txt", col.names = c("serial No.", "feature"))

test.file <- "Dataset\\test\\X_test.txt"
test.data <- read.table(test.file, header = FALSE, col.names = features$feature)

test.subject <- read.table("Dataset\\test\\subject_test.txt", col.names = "Subject")
test.lable <- read.table("Dataset\\test\\y_test.txt", col.names = "Activity")

test.data <- tbl_df(cbind(Subject = test.subject, Activity = test.lable, test.data))

train.file <- "Dataset\\train\\X_train.txt"
train.data <- read.table(train.file, header = FALSE, col.names = features$feature)

train.subject <- read.table("Dataset\\train\\subject_train.txt", col.names = "Subject")
train.lable <- read.table("Dataset\\train\\y_train.txt", col.names = "Activity")

train.data <- tbl_df(cbind(Subject = train.subject, Activity = train.lable, train.data))

#Step1: Merging.

data <- rbind(test.data, train.data)
data <- data[order(data$Subject), ]

#Step2: Extracting columns with 'mean' and 'std'.

data1 <- data[, grep("[Mm]ean|std", features$feature)]

#Step3: Renaming Activity column.

for (i in 1:6) {
  data1$Activity <- gsub(activities[i, 1], activities[i, 2], data1$Activity)
}

#Step4: Correcting column names to more descriptive form.

names(data1) <- gsub("tB", "Time B", names(data1))
names(data1) <- gsub("^t", "Time ", names(data1))
names(data1) <- gsub("^f", "Frequency ", names(data1))
names(data1) <- gsub("Acc", " Accelerometer ", names(data1))
names(data1) <- gsub("\\.", "", names(data1))
names(data1) <- gsub("mean", "Mean ", names(data1))
names(data1) <- gsub("Jerk", "Jerk ", names(data1))
names(data1) <- gsub("std", "std ", names(data1))
names(data1) <- gsub("Gyro", " Gyrometer ", names(data1))
names(data1) <- gsub("gravity", " Gravity ", names(data1))
names(data1) <- gsub("angle", "Angle ", names(data1))

#Step5: Taking average for all columns and summarizing. 

final_data <- group_by(data1, Subject, Activity)
final_data <- summarise_all(final_data, funs(mean))
final_data <- final_data[, -c(3,4)]

#Step6: Writing the final_data to table.

write.table(final_data, file = "Tidy_Data", row.names = FALSE)
