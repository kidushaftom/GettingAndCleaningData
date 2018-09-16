#You should create one R script called run_analysis.R that does the following.
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement.
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names.
#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

#________________________________________________________________________________________________________________

setwd("C:/Users/Haftom/GettingAndCleaningData")
dir<-getwd()
install.packages("tidyr")
install.packages("dplyr")
install.packages("data.table")
require(tidyr)
require(dplyr)
require(data.table)
## set up data path for reading and writting
datapath<-file.path(dir,"project_data")

##merging subjects
subject<-rbind(fread(file.path(datapath, "train", "subject_train.txt")),
               fread(file.path(datapath, "test" , "subject_test.txt" )) )
names(subject)<-"subject"

##merging Activity

activity<-rbind(fread(file.path(datapath, "train", "Y_train.txt")),
                fread(file.path(datapath, "test" , "Y_test.txt" )) )
names(activity)<-"activity"

##merging measures
measures<-rbind(fread(file.path(datapath, "train", "X_train.txt")),
               fread(file.path(datapath, "test" , "X_test.txt" )) )

## combine activity,subject and measures
all_data_set<-cbind(subject,activity,measures)
setkey(all_data_set, subject, activity)


##merging activity names and Numbers
ActivityNames <- fread(file.path(datapath, "activity_labels.txt"))
names(ActivityNames)<-c("activity", "activity_name")
all_data_set_final<- merge(all_data_set, ActivityNames, by = "activity", 
                      all.x = TRUE)
#______________________________________________________________________________________
all_data_narrow<- data.table(melt(all_data_set_final, id=c("subject",  "activity_name"), 
                                        measure.vars = c(3:68), 
                                        variable.name = "code", 
                                        value.name="value"))
#______________________________________________________________________________
features <- fread(file.path(datapath, "features.txt"))
names(features)<-c("number","code_name")
features$code <-features[, paste0("V",number)]
#______________________________________________________________________________________
all_data_narrow_final <- merge(all_data_narrow,features,
                              by="code", all.x=TRUE)

#_________________________________________________________________________________________

##Finalizing
mean_std<-all_data_narrow_final%>%
  filter(grepl("(mean|std)\\(\\)", code_name))%>%
  dcast(subject + activity_name  ~ code_name,  mean)
mean_std
#Writing to disc
write.table(mean_std, file="tidyData.txt", row.name=FALSE, sep = "\t")

