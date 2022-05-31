########################################################################################################
#                                   F I L E   D E S C R I P T I O N                                    #
########################################################################################################
#                                                                                                      #
# This file contains the code related to EDA: general data analysis by plotting various graphs and     #
# plots on different data to get most out the analysis. This file is using variables created in        #
# ./munge/01-A.R file. So, to check the variable declaration please refer 01-A.R file.                 #
########################################################################################################


########################################################################################################
# Numerical summary of each dataset:

#overview_enrollement=datatable(Numnerical_summary_enrollemts_1)
# Overview of the data - Type = 1
Numnerical_summary_enrollemts_1=ExpData(data=enrolments,type=1)

# Structure of the data - Type = 2
Numerical_summary_enrollents_2=ExpData(data=enrolments,type=2)


Numnerical_summary_leaving_survey=ExpData(data=leaving_survey,type=1)

Numnerical_summary_step_activity=ExpData(data=step_activity,type=1)

Numnerical_summary_video_stat=ExpData(data=video_stat,type=1)


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

########################################################################################################
#gender distribution
gender_plot=ggbarplot(gender, x = "Var1", y = "Freq",
          fill = "Var1", color = "Var1", palette = "jco")+xlab('Gender')+ylab("")+
  ggtitle("Gender Distrubtion")+theme(plot.title = element_text(hjust = 0.5,face = 'bold'))
#gender_plot


education_level_plot=ggbarplot(edu_lvl, x = "Var1", y = "Freq",
                      fill = "Var1", color = "Var1", palette = "jco")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlab('Education Level')+ylab("")+
  ggtitle("Eduation level Distrubtion")+theme(plot.title = element_text(hjust = 0.5,face = 'bold'))

#education_level_plot

#employmnet status
emp_status_plot=ggbarplot(emp_status, x = "Var1", y = "Freq",
                          fill = "Var1", color = "Var1", palette = "jco")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlab('Employment Status')+ylab("")+
  ggtitle("Employment status Distrubtion")+theme(plot.title = element_text(hjust = 0.5,face = 'bold'))
#emp_status_plot

# employment area
employment_area_plot=ggbarplot(emp_area, x = "Var1", y = "Freq",
                               fill = "Var1", color = "Var1", palette = "jco")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#employment_area_plot

#age range
age_range_plot=ggbarplot(age_range, x = "Var1", y = "Freq",
                         fill = "Var1", color = "Var1", palette = "jco")+xlab('Age')+ylab("")+
  ggtitle("Age Distrubtion")+theme(plot.title = element_text(hjust = 0.5,face = 'bold'))
#age_range_plot

########################################################################################################

########################################################################################################

#video views based on Geographical regions
p0 = ggplot() + theme_bw() + 
  geom_bar(aes(x = "", y = avg, fill = cont.factor), 
           stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + 
  ggtitle("Video Views Based on Geographical Regions") + 
  theme(plot.title = element_text(hjust = 0.1, size = 15),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid=element_blank(),
        panel.border = element_blank())
ypos = cumsum(avg) - 0.5 * avg
ypos = 100 - ypos
vid_stat = p0 + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  scale_fill_brewer(palette = "GnBu", name = "Regions") + 
  theme(legend.text=element_text(size=15),
        legend.title = element_text(hjust = 0.5, size=15),
        legend.key.size = unit(0.5,"cm")) + 
  geom_text(aes(x = "", y = ypos, label = paste0(round(avg, digits = 1), "%")), size = 2.2)+
  theme(plot.title = element_text(hjust = 0.5,face = 'bold'))
vid_stat

########################################################################################################

########################################################################################################
#Most widely used devices
device_used=ggplot(device, aes(x = "", y = Usage, fill = row.names(device))) +
  geom_col(color = "black") +
  geom_label(aes(label = Usage), color = c("white", "white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Usage")) +
  scale_fill_viridis_d() +
  coord_polar(theta = "y") +
  ggtitle("Most Widely Used Device to Access The Content")+
  theme(plot.title = element_text(hjust = 0.5,face = 'bold'))

########################################################################################################


########################################################################################################
#Unique number of students going through each module

Unique_visted=ggbarplot(student_visited_count_df, x = "chapter", y = "stud_per_chap",
                          fill = "chapter", color = "chapter", palette = "jco")+geom_text(aes(label = stud_per_chap, vjust = 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+xlab('Module Number')+ylab("Number of Unique Students")+
  ggtitle("Unique Number of Students Going Through Each Module")+theme(plot.title = element_text(hjust = 0.5,face = 'bold'))

Unique_visted
########################################################################################################

########################################################################################################

#Unique No of Students Engaging In Different Delivery Methods
Delivery_methods=ggplot(cust_plotting, aes(Modules, Value, fill = Delivery_Method)) + geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T) +facet_wrap(~Delivery_Method)+
  geom_text(
    aes(label = Value), size = 3,
    vjust = 1.5, position = position_dodge(.9)
  )+
  xlab("Module No ") +
  ylab("No. of Unique Students") +
  ggtitle("Unique No of Students Engaging In Different Delivery Methods")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face = 'bold'))

########################################################################################################
########################################################################################################
