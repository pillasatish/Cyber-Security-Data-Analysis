########################################################################################################
#                                   F I L E   D E S C R I P T I O N                                    #
########################################################################################################
#                                                                                                      #
# This file contains the code related to EDA: general data analysis by plotting various graphs and     #
# plots on different data to get most out the analysis. This file is using variables created in        #
# ./munge/01-A.R file. So, to check the variable declaration please refer 01-A.R file.                 #
########################################################################################################


########################################################################################################
# plotting the enrollments 
enrollments_plot=ggplot(data=stud_count,aes(Runs,student_count,student_count))+geom_bar(stat="identity", fill = viridis(7), alpha=.8, width=.5)+
  geom_text(aes(label = student_count, vjust = 1))+
  ggtitle("Number of Learners Enrolled Over Different Iteration")+
  xlab("Batches") +
  ylab("Number of Enrolment(learners)")+
  theme(plot.title = element_text(hjust = 0.5,face = 'bold'))
enrollments_pt=enrollments_plot+ geom_line(data = stud_count, mapping = aes(x = Runs, y = student_count, group =1), col = "blue",size = 1.5)
enrollments_pt

########################################################################################################

########################################################################################################
#Percentage of Learners Enrolled Over Each Batch
pie3d=pie3D(data,labels = final_labels,explode = 0.05,main='Percentage of Learners Enrolled Over Each Batch',labelcex =1.2,shade =1.75,radius =1)


########################################################################################################
summary_g= data.frame( Summary=c("Total students Register","Students Un-register","Student did not start the Course","Students Started the Course"),Count_number=c(student_register,student_unregistered,student_started,student_not_started))
#summary_table=setDT(summary_g)
#summary_g

#STUDENT STARTING ,UN???ENROLLED AND DID NOT START THE COURSE
pie_chart_overall_representation <- percentage_dataframe %>% plot_ly(labels = ~reason,
                      values = ~percent, 
                      marker = list(colors = c('#00aa7f', '#ff0000','#ccccc7')))
pie_chart_overall_representation <- pie_chart_overall_representation %>% 
  add_pie(hole = 0.55,
          textfont = list(size = 18))  %>%
  layout(legend = list(font = list(size = 18)),
         hoverlabel = list(font = list(size = 18)))
pie_chart_overall_representation
########################################################################################################

########################################################################################################
# Plot for Reason for leaving the course...
leaving_survey_plot=ggplot(leaving_reason,aes(reorder(Reason,freq),freq))+geom_bar(stat="identity", fill="red", alpha=.8, width=.5) +
  geom_text(aes(label = freq), hjust = 1)+
  coord_flip() +
  xlab(" Reasons") +
  ylab("No. of Students Un-enrolled") +
  ggtitle("Students Un-Enrolled Vs Reasons")+
  theme(plot.title = element_text(hjust = 0.5,face = 'bold'))
#leaving_survey_plot
# Hole size
hsize <-5

leaving_reason <- leaving_reason %>% 
  mutate(x = hsize)



leaving_donut_plot=ggplot(leaving_reason, aes(x = hsize, y = freq, fill = Reason)) +
  geom_col() +
  geom_text(aes(label = percent(freq/800)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  xlim(c(0.2, hsize + 0.5))+ggtitle("Percentage of Students Un-Enrolled Vs Reasons")+
  theme(plot.title = element_text(hjust = 0.5,face = 'bold'))+xlab("")+ ylab("Percentage of Students Un-enrolled")

#leaving_donut_plot

########################################################################################################

########################################################################################################

#represnting countires with most number of enrollments.
countries_plot=world %>%
  merge(countries_count, by.x = "region", by.y = "country", all.x = T) %>%
  arrange(group, order) %>%
  ggplot(aes(x = long, y = lat, group = group, fill = Freq)) + geom_polygon() + xlab(" Longitude") +
  ylab("Latitude") +
  ggtitle("Students Enrolling form Different Countries")+
  theme(plot.title = element_text(hjust = 0.5,face = 'bold'))

#countries_plot


# plot with countries with greater than 500 enrollments 

countries_with_500=ggplot(country_500,aes(reorder(country,-Freq),Freq))+geom_bar(stat="identity", fill=viridis(10), alpha=.8, width=.5) +
  geom_text(aes(label = Freq, vjust = 1))+
  xlab("Countries") +
  ylab("No. of Enrolment")+
  ggtitle("Countries With Enrollments Greater Than 500 Students")+
  theme(plot.title = element_text(hjust = 0.5,face = 'bold'))

########################################################################################################
