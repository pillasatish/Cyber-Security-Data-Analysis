# plotting the enrollments 
enrollments_plot=ggplot(data=stud_count,aes(Runs,student_count,student_count))+geom_bar(stat="identity", fill = viridis(7), alpha=.8, width=.5)+
  geom_text(aes(label = student_count, vjust = 1))+
  ggtitle("Number of Learners Enrolled Over Different Iteration")+
  xlab("Batches") +
  ylab("Number of Enrolment(learners)")+
  theme(plot.title = element_text(hjust = 0.5,face = 'bold'))
enrollments_pt=enrollments_plot+ geom_line(data = stud_count, mapping = aes(x = Runs, y = student_count, group =1), col = "blue",size = 1.5)
enrollments_pt


#Percentage of Learners Enrolled Over Each Batch
pie3d=pie3D(data,labels = final_labels,explode = 0.05,main='Percentage of Learners Enrolled Over Each Batch',labelcex =1.2,shade =1.75,radius =1)


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
