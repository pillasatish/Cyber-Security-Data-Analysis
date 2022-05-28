
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


#Reading all the leaving survey response files

cyber_security_1_leaving_survey_responses <- read_csv("data/cyber-security-1_leaving-survey-responses.csv")

cyber_security_2_leaving_survey_responses <- read_csv("data/cyber-security-2_leaving-survey-responses.csv")

cyber_security_3_leaving_survey_responses <- read_csv("data/cyber-security-3_leaving-survey-responses.csv")


cyber_security_4_leaving_survey_responses <- read_csv("data/cyber-security-4_leaving-survey-responses.csv")

cyber_security_5_leaving_survey_responses <- read_csv("data/cyber-security-5_leaving-survey-responses.csv")

cyber_security_6_leaving_survey_responses <- read_csv("data/cyber-security-6_leaving-survey-responses.csv")

cyber_security_7_leaving_survey_responses <- read_csv("data/cyber-security-7_leaving-survey-responses.csv")

leaving_survey= rbind(cyber_security_1_leaving_survey_responses, cyber_security_2_leaving_survey_responses, cyber_security_3_leaving_survey_responses, cyber_security_4_leaving_survey_responses, cyber_security_5_leaving_survey_responses, cyber_security_6_leaving_survey_responses, cyber_security_7_leaving_survey_responses)

#leaving_survey

#Reading all the step activity dataset files

cyber_security_1_step_activity <- read_csv("data/cyber-security-1_step-activity.csv")

cyber_security_2_step_activity <- read_csv("data/cyber-security-2_step-activity.csv")

cyber_security_3_step_activity <- read_csv("data/cyber-security-3_step-activity.csv")

cyber_security_4_step_activity <- read_csv("data/cyber-security-4_step-activity.csv")

cyber_security_5_step_activity <- read_csv("data/cyber-security-5_step-activity.csv")

cyber_security_6_step_activity <- read_csv("data/cyber-security-6_step-activity.csv")

cyber_security_7_step_activity <- read_csv("data/cyber-security-7_step-activity.csv")


step_activity= rbind(cyber_security_1_step_activity, cyber_security_2_step_activity, cyber_security_3_step_activity, cyber_security_4_step_activity, cyber_security_5_step_activity, cyber_security_6_step_activity, cyber_security_7_step_activity)




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




# Total no of students registered 
student_register = length(unique(enrolments$learner_id))
#student_register
# No of students unregistered 
student_unregistered=length(unique(leaving_survey$learner_id))
#student_unregistered
# percentage unregistered
Percentage_unregsiter=(student_unregistered/student_register)*100
#Percentage_unregistered
# percent students after unregistered
student_after_unregister = student_register - student_unregistered # no of students registered students - no of students unregistered
percentage_student_after_unregister = (student_after_unregister/student_register)*100
#percentage_student_after_unregistered
# No of students who started the course 
student_started = length(unique(step_activity$learner_id))
#student_started
# percentage of students who started the course 
percentage_started =  (student_started/student_register)*100
#percentage_started

# percentage who did not start the course 
student_not_started = student_after_unregister - student_started
#student_not_started
percentage_student_not_started = (student_not_started/student_register)*100
percentage_student_not_started


summary_g= data.frame( Summary=c("Total students Register","Students Un-register","Student did not start the Course","Students Started the Course"),Count_number=c(student_register,student_unregistered,student_started,student_not_started))
#summary_table=setDT(summary_g)
#summary_g
percentage_dataframe = data.frame(reason = c("Started the course","Did not start the course", "Un-enrolled from the course"), percent =c(percentage_started,percentage_student_not_started,Percentage_unregsiter ))


# Counting the number of time each reason was used in leaving survey:
leaving_reason = data.frame(table(leaving_survey$leaving_reason))
colnames(leaving_reason) = c('Reason', 'freq')
leaving_reason


#ploting the country heat map:

countries_count = data.frame(table(enrolments$detected_country))
#countries_count
countries_with_greater_100 = countries_count[countries_count$Freq>100,]
countries_count$Var1 = as.character(countries_count$Var1)

country_name=countrycode(sourcevar = countries_count$Var1, "iso2c", "country.name")
#country_name
countries_count <- cbind(countries_count, country = country_name)
countries_count

world <- map_data("world")
#world_map <- subset(world_map)

countries_with_greater_500 = countries_count[countries_count$Freq>500,]
country_500=na.omit(countries_with_greater_500)


# Removing unknown from each column(gender, education level, employment status, employment area, age range)...
gender = data.frame(gender = enrolments$gender[enrolments$gender != 'Unknown'])
gender = as.data.frame(table(gender$gender))

#gender


edu_lvl = data.frame(edu_lvl = enrolments$highest_education_level[enrolments$highest_education_level != 'Unknown'])
edu_lvl = as.data.frame(table(edu_lvl$edu_lvl))
#colnames(edu_lvl)<- c("Education Level","Freq")

emp_status = data.frame(emp_status = enrolments$employment_status[enrolments$employment_status != 'Unknown'])
emp_status = as.data.frame(table(emp_status$emp_status))
#colnames(emp_status)<- c("Employment status","Freq")


emp_area = data.frame(emp_area = enrolments$employment_area[enrolments$employment_area != 'Unknown'])
emp_area = as.data.frame(table(emp_area$emp_area))
#colnames(emp_area)<- c("Employment area","Freq")

age_range = data.frame(age_range = enrolments$age_range[enrolments$age_range != 'Unknown'])
age_range = as.data.frame(table(age_range$age_range))
#colnames(age_range)<- c("Age Range","Freq")









