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


