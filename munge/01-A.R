
#Pre-processing steps:

########################################################################################################
#                                   F I L E   D E S C R I P T I O N                                    #
########################################################################################################
#                                                                                                      #
# This file contains the code related to Data Munging: transformations like, joining different tables, #
# merging tables row wise and column wise, text processing, melting table from wide format to long,    #
# column and row renaming, column and row deletion, calculating different statistics like column mean, #
# normalizing the data, etc.                                                                           #
########################################################################################################
#Reading all the enrollments files 

cyber_security_1_enrolments <- read_csv("data/cyber-security-1_enrolments.csv")

cyber_security_2_enrolments <- read_csv("data/cyber-security-2_enrolments.csv")

cyber_security_3_enrolments <- read_csv("data/cyber-security-3_enrolments.csv")

cyber_security_4_enrolments <- read_csv("data/cyber-security-4_enrolments.csv")

cyber_security_5_enrolments <- read_csv("data/cyber-security-5_enrolments.csv")

cyber_security_6_enrolments <- read_csv("data/cyber-security-6_enrolments.csv")

cyber_security_7_enrolments <- read_csv("data/cyber-security-7_enrolments.csv")

enrolments = rbind(cyber_security_1_enrolments, cyber_security_2_enrolments, cyber_security_3_enrolments, cyber_security_4_enrolments, cyber_security_5_enrolments, cyber_security_6_enrolments, cyber_security_7_enrolments)
#enrolments


# Finding the total number of enrollments over the course run  
enrollment_data=list(cyber_security_1_enrolments,cyber_security_2_enrolments,cyber_security_3_enrolments,cyber_security_4_enrolments,cyber_security_5_enrolments,cyber_security_6_enrolments,cyber_security_7_enrolments)
# to get a list of enrollments from each of the file 
student_count=lapply(enrollment_data, function(x) length(unique(x$learner_id)))
student_count =  array(as.numeric(unlist(student_count)), dim = c(7,1,1))
#student_enrollnmnent_cnt_un
stud_count = data.frame(student_count)
#adding an additional column run to plot the graph 
stud_count = tibble::rownames_to_column(stud_count, "Runs")
stud_count


#Plotting the percentage of students joining in each Iteration of the course.
data <- stud_count$student_count
data
chapters<-c('1st batch','2nd batch','3rd batch','4th batch','5th batch','6th batch','7th batch')
#calculates the percentage of the values
percentage<-round(data/sum(data)*100)

#concatenates the strings with percentages
labels_new<-paste(chapters,percentage)
labels_new

#concatenates the above output with the '%' symbol 
final_labels<-paste(labels_new,'%',sep = "")
final_labels
lab <- paste0(round(data/sum(data) * 100, 2), "%")
lab
