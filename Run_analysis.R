#Set the path to the necessary data/files
path_name<-"C:/Users/Jessy/Documents/Coursera/Getting and Cleaning Data/Course Project/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset"
setwd(path_name)

#Read the train sets
x_train<-read.table(paste(path_name,"/train/X_train.txt",sep=""), header=FALSE, dec=".")
y_train<-read.table(paste(path_name,"/train/y_train.txt",sep=""), header=FALSE, dec=".")
subject_train<-read.table(paste(path_name,"/train/subject_train.txt",sep=""), header=FALSE, dec=".")


#Read the test sets
x_test<-read.table(paste(path_name,"/test/X_test.txt",sep=""), header=FALSE, dec=".")
y_test<-read.table(paste(path_name,"/test/y_test.txt",sep=""), header=FALSE, dec=".")
subject_test<-read.table(paste(path_name,"/test/subject_test.txt",sep=""), header=FALSE, dec=".")

#merge the datasets to one set (1)
x<-rbind(x_test,x_train)
y<-rbind(y_test,y_train)
subject<-rbind(subject_test,subject_train)

#get the features names
features<-read.table(paste(path_name,"/features.txt",sep=""),header=FALSE)[,2]

#subset x to only take mean and std (2)
subset_factor<-grepl("mean()",features) | grepl("std()",features)
subset_x<-x[,subset_factor]


#merge to 1 dataset (1)
dataset<-cbind(subject, y, subset_x)


#subset the feature names accordingly (only mean and std variables)
features<-features[subset_factor]
features<-append(c("Subject","Activity_label"),features)

#Rename the dataset variables (4)
colnames(dataset)<-features


#get the activity labels
activity_labels<-read.table(paste(path_name,"/activity_labels.txt",sep=""),header=FALSE)


##convert Y and the index of activity labels from number to string, so that we can use str_replace_all
dataset$Activity_label<-sapply(dataset$Activity_label,toString)
activity_labels[,1]<-sapply(activity_labels[,1],toString)


library(stringr)

#For loop is to go through each entry and change to the corresponding activity name
for (i in 1:length(dataset$Activity_label)){
  for (j in 1:length(activity_labels[,1])){
    dataset$Activity_label[i]<-str_replace_all(dataset$Activity_label[i],activity_labels[,1][j],activity_labels[,2][j])
  }
}
  
#average table is an independent set, giving average value of each variable, by subject and by each activity
average_table<-aggregate(.~Activity_label+Subject,dataset,FUN=mean)

write.table(average_table,file="final_dataset.txt",row.names = FALSE)