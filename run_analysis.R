#reading in data

features <- read.table("UCI HAR Dataset/features.txt")
x.train <- read.table("UCI HAR Dataset/train/X_train.txt")
y.train <- read.table("UCI HAR Dataset/train/y_train.txt")
y.test <- read.table("UCI HAR Dataset/test/y_test.txt")
X.test <- read.table("UCI HAR Dataset/test/X_test.txt")
id.test <- read.table("UCI HAR Dataset/test/subject_test.txt")
id.train <- read.table("UCI HAR Dataset/train/subject_train.txt")
labels <- read.table("UCI HAR Dataset/activity_labels.txt")
# turning headers into list of character variables

features.list <- unlist(features[,2])
features.list <- as.character(features.list)

#turning the headers to lower case, removing hyphens and brackets

funny <- function() {
  x <- as.list(length(features.list))
  y <- as.list(length(features.list))
  for(i in 1:length(features.list)){
    x[i] <- gsub("\\(|\\)", "", features.list[i])
  }
  x
  for(i in 1:length(x)){
    y[i] <- gsub("\\-|\\,", "", x[i])
  }
  z <- tolower(y)
}

features.list.f <- funny()

#removing unnecessary stuff from global environment

remove(features.list)
remove(features)

#assigning headers to X.test and x.train

names(X.test) <- features.list.f
names(x.train) <- features.list.f

#combining dataframes

new.data <- rbind(X.test, x.train)
new.data.2 <- rbind(y.test, y.train)
id <- rbind (id.test, id.train)
data.set <- cbind(id, new.data.2, new.data)
colnames(data.set)[1] <- "subjectid" #naming column from subject data
colnames(data.set)[2] <- "activity"  #naming column from activity data
#creating new dataframe

extract.data <- function () {
  x <- grep("mean", names(data.set))
  y <- grep("std", names(data.set))
  colindex <- c(1,2,x,y)
  header <- names(data.set)
  newdata <- as.data.frame(matrix(nrow= nrow(data.set), ncol=length(colindex)))
  newheader <- as.character()
  colindex2 <- sort(colindex)
  for(i in 1: length(colindex)){
    z <- colindex2[i]
    newdata[,i] <- data.set[,z]
    newheader[i] <- header [z]
  }
  names(newdata) <- newheader
  newdata
}

new.data <- extract.data()

#creating the tidy dataset

mean.data <- function() {
  x <- as.numeric()
  cycframe <- as.data.frame(matrix(nrow=6, ncol=88))
  tidydata <- as.data.frame(matrix(ncol=88, nrow=180))
  y <- names(new.data)
  z <- 1
  
  
  for(i in 1:30){
    cycdata <- new.data[new.data$subjectid==i,]
    for(j in 1:length(names(cycdata))){
      x <- tapply(cycdata[,j], cycdata$activity, mean)
      cycframe[,j]<-x
    }
    zz <- z+5
    tidydata[z:zz,] <- cycframe
    z <- 1+6*i
  }
  names(tidydata) <- y
  tidydata
}

tidydata <- mean.data()

#removing unneccesary stuff
remove(X.test, data.set, id, id.test, id.train, new.data, new.data.2, x.train, y.test, y.train, features.list)
#adding appropriate labels
labeling <- function(){
  label <- as.character(labels$V2)
  for(i in 1:length(label)){
    x <- as.numeric(i)
    tidydata$activity <- sapply(tidydata$activity, gsub, pattern=x, replacement=label[x])  
  }
  tidydata
}
tidydata <- labeling()