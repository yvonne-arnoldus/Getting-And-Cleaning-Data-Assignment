library(reshape2)

run <- function()
{
    # step 1.1: 
    # read the different files into R
    activity.label.filename <- "./UCI HAR Dataset/activity_labels.txt"
    features.filename <- "./UCI HAR Dataset/features.txt"
    x.train.filename <- "./UCI HAR Dataset/train/x_train.txt"
    y.train.filename <- "./UCI HAR Dataset/train/y_train.txt"
    subject.train.filename <- "./UCI HAR Dataset/train/subject_train.txt"
    x.test.filename <- "./UCI HAR Dataset/test/x_test.txt"
    y.test.filename <- "./UCI HAR Dataset/test/y_test.txt"
    subject.test.filename <- "./UCI HAR Dataset/test/subject_test.txt"
    
    activity.labels <- read.table(activity.label.filename)
    features <- read.table(features.filename)
    
    x.train <- read.table(x.train.filename)
    y.train <- read.table(y.train.filename)
    subject.train <- read.table(subject.train.filename)
    
    x.test <- read.table(x.test.filename)
    y.test<- read.table(y.test.filename)
    subject.test <- read.table(subject.test.filename)
    
    # I'm putting in labels now so it is easier for me to understand the data
    # when proceding along with the other steps.
    colnames(activity.labels) <- c( "id", "activity.label")
    colnames(features) <- c( "id", "feature.label")
    features.names <- as.vector(features$feature.label)
    colnames(x.train) <- features.names
    colnames(y.train) <- c( "activity.id" )
    colnames(subject.train) <- c( "subject.id" )
    colnames(x.test) <- features.names
    colnames(y.test) <- c( "activity.id" )
    colnames(subject.test) <- c( "subject.id" )
        
    # step 3:
    # I'm putting in the activity names now so it is easier for me to understand 
    # the data when proceding along with the other steps.
    named.y.train <- merge(y.train,activity.labels, by.x="activity.id", by.y="id", all.x=TRUE)
    named.y.test <- merge(y.test,activity.labels, by.x="activity.id", by.y="id", all.x=TRUE)
    
    # step 1.2: 
    # combine the different training files to a dataframe
    # combine the different test files to a dataframe
    train <- data.frame(subject.train,named.y.train,x.train)
    test <- data.frame(subject.test,named.y.test,x.test)
    
    # step 1.3
    # combine the training and test set to the data set.
    data <- rbind(train,test)
    
    # step 2
    # extracting only the measurements on the mean and standard deviation for each measurement. 
    # according to features.info.txt this are the coloms with mean() and std() 
    # on the end and we keep the columns: subject.id & activity.label
    pattern <- ".*\\.mean\\..*|.*\\.std\\..*"
    mean.std.data <- data[c("subject.id", "activity.label", grep(pattern, names(data), value = TRUE))]
        
    # step 4
    # Asign appropriately labels to the data set with descriptive variable names.
    # renaming the following using the Google's R Style Guide:
    new.names <- colnames(mean.std.data)
    new.names <- gsub( "\\.", "", new.names, ignore.case = FALSE )
    # subjectid -> subject.id
    new.names <- gsub( "subjectid", "subject.id", new.names, ignore.case = FALSE )    
    # activitylabel -> activity
    new.names <- gsub( "activitylabel", "activity", new.names, ignore.case = FALSE )
    # t -> time
    new.names <- gsub( "tB", "time.b", new.names, ignore.case = FALSE)
    new.names <- gsub( "tG", "time.G", new.names, ignore.case = FALSE )
    # f -> frequency
    new.names <- gsub( "fB", "frequency.b", new.names, ignore.case = FALSE)
    new.names <- gsub( "fG", "frequency.G", new.names, ignore.case = FALSE )    
    # Acc -> accelerometer
    new.names <- gsub( "Acc", ".accelerometer", new.names, ignore.case = FALSE )       
    # Gyro -> gyroscope
    new.names <- gsub( "Gyro", ".gyroscope", new.names, ignore.case = FALSE )
    # Mag -> magnitude
    new.names <- gsub( "Mag", ".magnitude", new.names, ignore.case = FALSE )
    # mean -> mean.value
    new.names <- gsub( "mean", ".mean.value", new.names, ignore.case = FALSE )
    # std -> standard.deviation
    new.names <- gsub( "std", ".standard.deviation", new.names, ignore.case = FALSE )
    # X -> x.axis
    new.names <- gsub( "X", ".x.axis", new.names, ignore.case = FALSE )
    # Y -> y.axis
    new.names <- gsub( "Y", ".y.axis", new.names, ignore.case = FALSE )
    # Z -> z.axis
    new.names <- gsub( "Z", ".z.axis", new.names, ignore.case = FALSE )
    # To lower
    new.names <- tolower(new.names)
    # bodybody -> body.body
    new.names <- gsub( "bodybody", "body.body", new.names, ignore.case = FALSE )
    # jerk -> .jerk
    new.names <- gsub( "jerk", ".jerk", new.names, ignore.case = FALSE )
    # assign the new column names to the data.    
    colnames(mean.std.data) <- new.names

    # step 5
    # creating a second, independent tidy data set with the average of each variable 
    # for each activity and each subject.
    #aggregated.data <- aggregate( mean.std.data, by=list(c( mean.std.data$subject.id, mean.std.data$activity )), FUN=mean, na.rm=TRUE)
    #aggregated.data <- dcast( mean.std.data )
    #aggregated.data <- dcast(mean.std.data, subject.id ~ activity, fun.aggregate = mean, na.rm = TRUE)
    #aggregated.data <- aggregate( mean.std.data[,3], mean.std.data$subject.id, mean )
    aggregated.data <- aggregate(mean.std.data[,3:68],by=list(mean.std.data$subject.id, mean.std.data$activity), mean)
    
    # step 6.1
    # set meaning full names
    aggregated.names <- colnames(aggregated.data)
    aggregated.names <- gsub( "Group.1", "subject.id", aggregated.names, ignore.case = FALSE )
    aggregated.names <- gsub( "Group.2", "activity", aggregated.names, ignore.case = FALSE )
    colnames(aggregated.data) <- aggregated.names
    
    # step 6.2
    # order the data so it is beter readable
    ordered.aggregated.data <- aggregated.data[ order(aggregated.data[,1], aggregated.data[,2]), ]
    
    # step 6.3
    # output the data to file
    write.table( ordered.aggregated.data, file = "output.txt", row.name=FALSE )
}