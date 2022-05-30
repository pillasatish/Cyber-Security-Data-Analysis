
#Pre-processing steps:

########################################################################################################
#                                   F I L E   D E S C R I P T I O N                                    #
########################################################################################################
#                                                                                                      #
# This file contains the code related to Data Munging: transformations like, joining different tables, #
# merging tables row wise and column wise, melting table from wide format to long, column and row      #
# renaming, column and row deletion.                                                                   #
#                                                                                                      #
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

#Reading all the video_stat dataset files
cyber_security_3_video_stats <- read_csv("data/cyber-security-3_video-stats.csv")

cyber_security_4_video_stats <- read_csv("data/cyber-security-4_video-stats.csv")

cyber_security_5_video_stats <- read_csv("data/cyber-security-5_video-stats.csv")

cyber_security_6_video_stats <- read_csv("data/cyber-security-6_video-stats.csv")

cyber_security_7_video_stats <- read_csv("data/cyber-security-7_video-stats.csv")

video_stat= rbind(cyber_security_3_video_stats, cyber_security_4_video_stats, cyber_security_5_video_stats, cyber_security_6_video_stats, cyber_security_7_video_stats)

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


per_cont = data.frame(video_stat$europe_views_percentage, video_stat$north_america_views_percentage,  video_stat$asia_views_percentage, video_stat$africa_views_percentage, video_stat$south_america_views_percentage, video_stat$oceania_views_percentage)

colnames(per_cont) =  c("Europe", "North-America", "Asia","Africa","South-America","Oceania")
avg_cont_view = per_cont %>% summarise_if(is.numeric, mean)
#avg_cont_view
conti_name = data.frame(cont=c("Europe", "North-America", "Asia","Africa","South-America","Oceania"),avg =as.numeric(avg_cont_view))

avg= conti_name$avg
avg
round(avg, digits = 1)
cont = conti_name$cont #############

cont = cont[order(avg)]
avg = sort(avg)
round(avg,digits = 1)
cont.factor = factor(cont, levels = as.character(cont))#########

cont.factor

view_stat=data.frame(avg,cont.factor)
view_stat


# Finding which device is being use more often to access the course content.
desktop_percentage = video_stat %>% group_by(title) %>% summarize(desktop = round(mean(desktop_device_percentage),2))
mobile_percentage = video_stat %>% group_by(title) %>% summarize(mobile = round(mean(mobile_device_percentage),2))
tablet_percentage = video_stat %>% group_by(title) %>% summarize(tablet = round(mean(tablet_device_percentage),2))

#Dataframe
device = cbind(desktop_percentage, mobile_percentage[,2], tablet_percentage[,2])
#device
device = as.data.frame(round(colMeans(device[2:4]),2))
#device
colnames(device) = c("Usage")
#device


# unique no students visiting each chapter 
unique_student=step_activity %>%
  mutate(learner_id,chapter=trunc(step))
# function to remove duplicates and classify to each chapters 
chap_class=function(x){
  a=c()
  for(i in 1:3){
    y=x %>%
      filter(x$chapter==i)
    z=distinct(y,learner_id,.keep_all = TRUE)
    b=nrow(z)
    a=c(a,b)
    
  }
  return(a)
}

student_visited_count = chap_class(unique_student)
student_visited_count
student_visited_count_df=data.frame(chapter=c("1","2","3"),stud_per_chap=student_visited_count)
student_visited_count_df

# data frame by weeks 
week_1= step_activity %>%
  filter(week_number %in% c(1))
week_2= step_activity %>%
  filter(week_number %in% c(2))
week_3= step_activity %>%
  filter(week_number %in% c(3))

# To seperate out videos , articles and quiz 

# VIDEOS BY WEEK 
week_1_vid= week_1 %>%
  filter(step_number %in% c(5,14,17,19))
week_2_vid= week_2 %>%
  filter(step_number %in% c(4,11,17))
week_3_vid= week_3 %>%
  filter(step_number %in% c(2,14,15))



# ARTICLES BY WEEK
week_1_art= week_1 %>%
  filter(step_number %in% c(3,6,7,9,12,13,15,16,18))
week_2_art= week_2 %>%
  filter(step_number %in% c(2,5,7,9,10,12,13,14,15,16,18,19,21,22))
week_3_art= week_3 %>%
  filter(step_number %in% c(5,6,7,8,9,10,13,17,20))

# QUIZ BY WEEK 
week_1_qiz= week_1 %>%
  filter(step_number %in% c(8))
week_2_qiz= week_2 %>%
  filter(step_number %in% c(8,20))
week_3_qiz= week_3 %>%
  filter(step_number %in% c(11))

# DISCUSSION BY WEEK 
week_1_dis= week_1 %>%
  filter(step_number %in% c(2,10,11))
week_2_dis= week_2 %>%
  filter(step_number %in% c(6,23))
week_3_dis= week_3 %>%
  filter(step_number %in% c(4,12,16,19))


# COMBING VIDEOS , ARTICLES , QUIZ , DISCUSIION TO INDIVIDUAL DATA FRAME 
video_stat = rbind(week_1_vid, week_2_vid, week_3_vid)
article_stat=  rbind(week_1_art, week_2_art, week_3_art )
quiz_stat = rbind(week_1_qiz, week_2_qiz, week_3_qiz)
discussion_stat = rbind(week_1_dis, week_2_dis, week_3_dis)

# Filterning The Unique Numbers and Counting Through The Week 
unique_stat_2=function(x){ 
  a=c()
  for(i in 1:3){
    y=x %>%
      filter(x$week_number==i)
    z=distinct(y,learner_id,.keep_all = TRUE)
    b=nrow(z)
    a=c(a,b)
    
  }
  return(a)
}
unique_video_count = unique_stat_2(video_stat)


unique_article_count = unique_stat_2(article_stat)

unique_quiz_count = unique_stat_2(quiz_stat)

unique_discussion_count = unique_stat_2(discussion_stat)

# Dataframe 
stat_count = data.frame(Video = unique_video_count , article = unique_article_count, quiz= unique_quiz_count, disscusion = unique_discussion_count )
stat_count =t(stat_count)
colnames(stat_count) =  c('Module-1','Module-2', 'Module-3')
stat_count  = data.frame(stat_count)
# BELOW STEPS FOR CLUSTER PLOT 
stat_count_2 = tibble::rownames_to_column(stat_count, "Delivery_Method")
#cons_df_num_t2$Delivery_Method

cust_plotting = stat_count_2 %>% gather(key = Modules, value = Value, Module.1:Module.3)
cust_plotting









