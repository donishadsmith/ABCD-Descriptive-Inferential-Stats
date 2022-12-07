pacman::p_load(dplyr, ggplot2, tidyr)
data = readxl::read_xlsx("~/Documents/ABCD Demographics Survey.xlsx")
data = data[2:nrow(data),]
data = data.frame(ID = 1:nrow(data), data)


Q6_4_Grouped = c()
for(i in data$Q6_4){
  if(i %in% c(0)){
    convert = 'Does not work w/ participants'
  }
  else if(i %in% c(NA)){
    convert = NA
  }
  else{
    convert = 'Works w/ Participants'
  }
  Q6_4_Grouped = c(Q6_4_Grouped ,convert)
}

data$Q6_4_Grouped  =Q6_4_Grouped 

Q5_4_Grouped = c()
for(i in data$Q5_4){
  if(i %in% c(0:10)){
    convert = '0-10'
  }
  else if(i %in% c(11:20)){
    convert = '11-20'
  }
  else if(i %in% c(21:30)){
    convert = '21-30'
  }
  else if(i %in% c(NA, NULL)){
    convert = NA
  }
  else{
    convert = '31-40'
  }
  Q5_4_Grouped= c(Q5_4_Grouped,convert)
}
data$Q5_4_Grouped =Q5_4_Grouped

Q5_4_Grouped_All = c()
for(i in data$Q5_4){
  if(i %in% c(0:10)){
    convert = '0-10'
  }
  else if(i %in% c(11:20)){
    convert = '11-20'
  }
  else if(i %in% c(21:30)){
    convert = '21-30'
  }
  else if(i %in% c(NA, NULL)){
    convert = NA
  }
  else{
    convert = '31-40'
  }
  Q5_4_Grouped_All= c(Q5_4_Grouped_All,convert)
}
data$Q5_4_Grouped_All =Q5_4_Grouped_All

Q6_4_Grouped_All = c()
for(i in data$Q6_4){
  if(i %in% c(0:10)){
    convert = '0-10'
  }
  else if(i %in% c(11:20)){
    convert = '11-20'
  }
  else if(i %in% c(21:30)){
    convert = '21-30'
  }
  else if(i %in% c(NA, NULL)){
    convert = NA
  }
  else{
    convert = '31-40'
  }
  Q6_4_Grouped_All= c(Q6_4_Grouped_All,convert)
}
data$Q6_4_Grouped_All =Q6_4_Grouped_All

#Creating a grouping variable for all survey participants
data$All = rep('All ABCD Survey Participants', nrow(data))

all_survey_participants = data[c('ID', "All" ,"Q27", "Q1", "Q2", "Q28", "Q29",
                               "Q30_Grouped", "Q3","Q4_Simplified", 
                               "Q5_4_Grouped_All", "Q6_4_Grouped_All", "Q7", "Q8_Simplified",
                               "Q9", "Q10","Q11_Simplified")]

all_survey_participants[is.na(all_survey_participants)] = "Filler"
#Q27


all_survey_participants_long= all_survey_participants %>% 
  group_by(All) %>%
  summarise("Yes" = sum(Q27=="Yes"),
            "No" = sum(Q27=="No")
  )

all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"

all_color = "#3d50b3"

ggsave(filename = "Q27_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("Our records indicate that you are currently engaged in the ABCD Study. If this is correct, please respond “Yes”.") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 10, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 4), width = 20, height = 8, dpi = 300, units = "in", device='png')


#Q1

all_survey_participants_long= all_survey_participants %>% 
  group_by(All) %>%
  summarise("Yes" = sum(Q1=="Yes"),
            "No" = sum(Q1=="No")
  )

all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"


ggsave(filename = "Q1_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("Are you willing to disclose your role in the ABCD Study as part of this survey (e.g., co-investigator, postdoctoral fellow, research assistant)?") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 23), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 14, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 6), width = 25, height = 9, dpi = 300, units = "in", device='png')


#Q2
all_survey_participants_expanded = separate_rows( all_survey_participants, "Q2", sep = ",")
all_survey_participants_expanded[is.na(all_survey_participants_expanded)] = "Filler"
summary(factor(all_survey_participants_expanded$Q2, exclude = NA))
all_survey_participants_long= all_survey_participants_expanded %>% 
  group_by(All) %>%
  summarise("Medical Professional (e.g., Nurse, Physician, Counselor, Phlebotomist)" = sum(Q2=="Medical Professional"),
            "RECOVER Coordinator" = sum(Q2=="RECOVER Coordinator"),
            "Research Assistant or Research Associate"= sum(Q2=="Research Assistant or Research Associate"),
            "Postdoctoral Fellow"=sum(Q2=="Postdoctoral Fellow"),
            "Graduate and/or Medical Student"=sum(Q2=="Graduate and/or Medical Student"),
            "Undergraduate Student"=sum(Q2=="Undergraduate Student"),
            "Site PI"=sum(Q2=="Site PI"),
            "Site Co-Investigator"=sum(Q2=="Site Co-Investigator"),
            "Project Coordinator or Project Manager or Project Assistant or Lab Manager"=sum(Q2=="Project Coordinator or Project Manager or Project Assistant or Lab Manager"),
            "Research Coordinator or Research Specialist or Research Scientist"=sum(Q2=="Research Coordinator or Research Specialist or Research Scientist")
            
  )

all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long$Variable = factor(all_survey_participants_long$Variable, levels = c(
  "Research Coordinator or Research Specialist or Research Scientist",
  "Project Coordinator or Project Manager or Project Assistant or Lab Manager",
  "Site Co-Investigator",
  "Site PI",
  "Undergraduate Student",
  "Graduate and/or Medical Student",
  "Postdoctoral Fellow",
  "Research Assistant or Research Associate",
  "RECOVER Coordinator",
  "Medical Professional (e.g., Nurse, Physician, Counselor, Phlebotomist)"
))
labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"
ggsave(filename = "Q2_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("What is your current role in the ABCD Study? Select all that apply to you:") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 11, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 5), width = 22, height = 8, dpi = 300, units = "in", device='png')



#Q28
summary(factor(all_survey_participants$Q28, exclude = NA))
all_survey_participants[is.na(all_survey_participants)] = "Filler"
all_survey_participants_long= all_survey_participants %>% 
  group_by(All) %>%
  summarise("Adjunct Professor" = sum(Q28=="Adjunct Professor"),
            "Assistant Professor (NTT)" = sum(Q28=="Assistant Professor (NTT)"),
            "Assistant Professor (TT)"= sum(Q28=="Assistant Professor (TT)"),
            "Associate Professor (NTT)"=sum(Q28=="Associate Professor (NTT)"),
            "Associate Professor (TT)"=sum(Q28=="Associate Professor (TT)"),
            "Full Professor (Non-Tenure Track; TT)"=sum(Q28=="Full Professor (Non-Tenure Track; TT)"),
            "Full Professor (Tenure Track; TT)"=sum(Q28=="Full Professor (Tenure Track; TT)")
  )

all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long$Variable = factor(all_survey_participants_long$Variable, levels = c(
  "Adjunct Professor",
  "Assistant Professor (NTT)",
  "Assistant Professor (TT)",
  "Associate Professor (NTT)",
  "Associate Professor (TT)",
  "Full Professor (Non-Tenure Track; TT)",
  "Full Professor (Tenure Track; TT)"
))
labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"

ggsave(filename = "Q28_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("Site PI/MPI") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 9, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 4), width = 13, height = 8, dpi = 300, units = "in", device='png')


#Q29
summary(factor(all_survey_participants$Q29, exclude = NA))
all_survey_participants[which(all_survey_participants$Q29=="Full Professor (TT)"), "Q29"] = "Full Professor (Tenure Track; TT & Non-Tenure Track; NTT)"
all_survey_participants[which(all_survey_participants$Q29=="Full Professor (NTT)"), "Q29"] = "Full Professor (Tenure Track; TT & Non-Tenure Track; NTT)"
all_survey_participants[which(all_survey_participants$Q29=="Assistant Professor (NTT)"), "Q29"] = "Assistant Professor (TT & NTT)"
all_survey_participants[which(all_survey_participants$Q29=="Assistant Professor (TT)"), "Q29"] = "Assistant Professor (TT & NTT)"
all_survey_participants[which(all_survey_participants$Q29=="Associate Professor (TT)"), "Q29"] = "Associate Professor (TT & NTT)"
all_survey_participants[which(all_survey_participants$Q29=="Associate Professor (NTT)"), "Q29"] = "Associate Professor (TT & NTT)"

all_survey_participants_long= all_survey_participants %>% 
  group_by(All) %>%
  summarise(
    "Adjunct Professor" = sum(Q29=="Adjunct Professor"),
            "Assistant Professor (TT & NTT)" = sum(Q29=="Assistant Professor (TT & NTT)"),
            "Associate Professor (TT & NTT)"=sum(Q29=="Associate Professor (TT & NTT)"),
            "Full Professor (Tenure Track; TT & Non-Tenure Track; NTT)"=sum(Q29=="Full Professor (Tenure Track; TT & Non-Tenure Track; NTT)")
  )


all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long$Variable = factor(all_survey_participants_long$Variable, levels = c(
  "Adjunct Professor",
  "Assistant Professor (TT & NTT)",
  "Associate Professor (TT & NTT)",
  "Full Professor (Tenure Track; TT & Non-Tenure Track; NTT)"
  
))
labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"

ggsave(filename = "Q29_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("Co-Investigator") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 9, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 4), width = 13, height = 8, dpi = 300, units = "in", device='png')


#Q30
summary(factor(all_survey_participants$Q30_Grouped, exclude = NA))
all_survey_participants_long= all_survey_participants %>% 
  group_by(All) %>%
  summarise(
    "Yes, I manage/supervise other ABCD team members." = sum(Q30_Grouped=="Managers/Supervisors"),
    "No, I do not manage/supervise other ABCD team members." = sum(Q30_Grouped=="Non-manager"),
  )


all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"

ggsave(filename = "Q30_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("Do you have a managerial role on the ABCD Study (i.e., do you supervise other ABCD team members)?") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 22), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 13.5, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 6), width = 24, height = 8, dpi = 300, units = "in", device='png')


#Q3
summary(factor(all_survey_participants$Q3, exclude = NA))
all_survey_participants_long= all_survey_participants %>% 
  group_by(All) %>%
  summarise(
    "2022" = sum(Q3=="2022"),
    "2021" = sum(Q3=="2021"),
    "2020" = sum(Q3=="2020"),
    "2019" = sum(Q3=="2019"),
    "2018" = sum(Q3=="2018"),
    "2017" = sum(Q3=="2017"),
    "2016" = sum(Q3=="2016"),
    "2015" = sum(Q3=="2015")
  )


all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long$Variable = factor(all_survey_participants_long$Variable, levels = c(
  "2022",
  "2021",
  "2020",
  "2019",
  "2018",
  "2017", 
  "2016",
  "2015" 
  
))



labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"

ggsave(filename = "Q3_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("What year did you join the ABCD Study?") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 9, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 4), width = 13, height = 8, dpi = 300, units = "in", device='png')


#Q4
colnames(all_survey_participants)
summary(factor(all_survey_participants$Q4_Simplified, exclude = NA))
 #Participants could select multiple choices for this variable 
# Each choice is seperated by a comma 
all_survey_participants_expanded = separate_rows( all_survey_participants, "Q4_Simplified", sep = ",")
all_survey_participants_expanded[is.na(all_survey_participants_expanded)] = "Filler"
summary(factor(all_survey_participants_expanded$Q4_Simplified, exclude = NA))
all_survey_participants_long= all_survey_participants_expanded %>% 
  group_by(All) %>%
  summarise("No, but I volunteer or receive course credit for my effort on the ABCD Study" = sum(Q4_Simplified=="Volunteer"),
            "Yes, I receive financial support from a source outside of ABCD" = sum(Q4_Simplified=="Trainee Award"),
            "Yes, I am paid directly from the main ABCD Study"= sum(Q4_Simplified=="Main Study"),
 
  )

all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long$Variable = factor(all_survey_participants_long$Variable, levels = c(
  "No, but I volunteer or receive course credit for my effort on the ABCD Study",
  "Yes, I receive financial support from a source outside of ABCD",
  "Yes, I am paid directly from the main ABCD Study"
))
labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"
ggsave(filename = "Q4_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("Are you financially compensated for your contributions to the ABCD Study? Select all that apply to you:") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 25), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 15, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 7), width = 29, height = 8, dpi = 300, units = "in", device='png')


#Q5

summary(factor(all_survey_participants$Q5_4_Grouped_All, exclude = NA))
all_survey_participants_long= all_survey_participants %>% 
  group_by(All) %>%
  summarise(
    "31-40" = sum(Q5_4_Grouped_All=="31-40"),
    "21-30" = sum(Q5_4_Grouped_All=="21-30"),
    "11-20" = sum(Q5_4_Grouped_All=="11-20"),
    "0-10" = sum(Q5_4_Grouped_All=="0-10"),
  )


all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long$Variable = factor(all_survey_participants_long$Variable, levels = c(
  "31-40",
  "21-30",
  "11-20",
  "0-10"
  
))

labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"

ggsave(filename = "Q5_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("How many total hours per week (on average) do you spend working on the ABCD Study?") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 9, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 4), width = 15, height = 8, dpi = 300, units = "in", device='png')

#Q6

summary(factor(all_survey_participants$Q6_4_Grouped_All, exclude = NA))
all_survey_participants_long= all_survey_participants %>% 
  group_by(All) %>%
  summarise(
    "31-40" = sum(Q6_4_Grouped_All=="31-40"),
    "21-30" = sum(Q6_4_Grouped_All=="21-30"),
    "11-20" = sum(Q6_4_Grouped_All=="11-20"),
    "0-10" = sum(Q6_4_Grouped_All=="0-10"),
  )

all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long$Variable = factor(all_survey_participants_long$Variable, levels = c(
  "31-40",
  "21-30",
  "11-20",
  "0-10"
  
))

labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"

ggsave(filename = "Q6_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("How many hours per week (on average) is spent interacting (in person or virtually) with ABCD participants?
") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 11, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 5), width = 18, height = 8, dpi = 300, units = "in", device='png')

#Q7
summary(factor(all_survey_participants$Q7, exclude = NA))

all_survey_participants_long= all_survey_participants %>% 
  group_by(All) %>%
  summarise("Yes" = sum(Q7=="Yes"),
            "No" = sum(Q7=="No")
  )

all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"

ggsave(filename = "Q7_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("Are you willing to disclose your demographic information as part of this survey?") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 9, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 4), width = 13, height = 8, dpi = 300, units = "in", device='png')

#Q8
summary(factor(all_survey_participants_expanded$Q8_Simplified, exclude = NA))
all_survey_participants_long= all_survey_participants_expanded%>% 
  group_by(All) %>%
  summarise("Other" = sum(Q8_Simplified=="Other"),
            "PhD, MD, or Other Post-Graduate Professional Degree" = sum(Q8_Simplified=="PhD/Professional Degree"),
            "Master’s Degree" = sum(Q8_Simplified=="Master’s Degree"),
            "Bachelor’s Degree" = sum(Q8_Simplified=="Bachelor’s Degree"),
            "Associate’s Degree" = sum(Q8_Simplified=="Associate’s Degree"),
            "Some college" = sum(Q8_Simplified=="Some college"),
            "High school or equivalent" = sum(Q8_Simplified=="High school or equivalent"),
  )

all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long$Variable = factor(all_survey_participants_long$Variable, levels = c(
  "Other",
  "PhD, MD, or Other Post-Graduate Professional Degree",
  "Master’s Degree",
  "Bachelor’s Degree",
  "Associate’s Degree",
  "Some college",
  "High school or equivalent"
  
))

labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"

ggsave(filename = "Q8_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("What is your current highest level of education?") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 9, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 4), width = 13, height = 8, dpi = 300, units = "in", device='png')

#Q9
summary(factor(all_survey_participants$Q9, exclude = NA))
all_survey_participants_long= all_survey_participants %>% 
  group_by(All) %>%
  summarise("Other" = sum(Q9=="Other"),
            "Non-Binary/Gender Queer/Gender Non-Conforming" = sum(Q9=="Non-Binary/Gender Queer/Gender Non-Conforming"),
            "Woman/Trans Woman" = sum(Q9=="Woman/Trans Woman"),
            "Man/Trans Man" = sum(Q9=="Man/Trans Man"),
  )

all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long$Variable = factor(all_survey_participants_long$Variable, levels = c(
  "Other",
  "Non-Binary/Gender Queer/Gender Non-Conforming",
  "Woman/Trans Woman",
  "Man/Trans Man"
  
))

labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"


ggsave(filename = "Q9_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("What is your current gender identity?") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 9, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 4), width = 13, height = 8, dpi = 300, units = "in", device='png')

#Q10
colnames(all_survey_participants)
summary(factor(all_survey_participants$Q10, exclude = NA))
all_survey_participants_expanded = separate_rows( all_survey_participants, "Q10", sep = ",")
all_survey_participants_expanded[is.na(all_survey_participants_expanded)] = "Filler"
summary(factor(all_survey_participants_expanded$Q10, exclude = NA))

all_survey_participants_long= all_survey_participants_expanded %>% 
  group_by(All) %>%
  summarise(
 "Other race/ethnicity not listed here" = sum(Q10=="Other race/ethnicity not listed here"),
  "Multiracial or Biracial"= sum(Q10=="Multiracial or Biracial"),
  "White"= sum(Q10=="White"),
  "Native American or Alaskan Native"= sum(Q10=="Native American or Alaskan Native"),
  "Hispanic or Latino/a/x"= sum(Q10=="Hispanic or Latino/a/x"),
  "Black or African American"= sum(Q10=="Black or African American"),
  "Asian or Pacific Islander"= sum(Q10=="Asian or Pacific Islander"),
 "Middle Eastern/North African"= sum(Q10=="Middle Eastern/North African")
 
)

all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long$Variable = factor(all_survey_participants_long$Variable, levels = c(
  "Other race/ethnicity not listed here",
  "Multiracial or Biracial",
  "Middle Eastern/North African",
  "White",
  "Native American or Alaskan Native",
  "Hispanic or Latino/a/x",
  "Black or African American",
  "Asian or Pacific Islander"
))

labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"

ggsave(filename = "Q10_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("How would you describe yourself? Select all that apply to you:") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 9, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 4), width = 13, height = 8, dpi = 300, units = "in", device='png')

#Q11
colnames(all_survey_participants)
summary(factor(all_survey_participants$Q11_Simplified, exclude = NA))
all_survey_participants_expanded = separate_rows( all_survey_participants, "Q11_Simplified", sep = ",")
all_survey_participants_expanded[is.na(all_survey_participants_expanded)] = "Filler"
summary(factor(all_survey_participants_expanded$Q11_Simplified, exclude = NA))

all_survey_participants_long= all_survey_participants_expanded %>% 
  group_by(All) %>%
  summarise(
    "Other" = sum(Q11_Simplified=="Other"),
    "Born outside the United States"= sum(Q11_Simplified=="Born outside the United States"),
    "From a disadvantaged background"= sum(Q11_Simplified=="From a disadvantaged background"),
    "First generation college student/professional"= sum(Q11_Simplified=="First generation college student/professional"),
    "Woman"= sum(Q11_Simplified=="Woman"),
    "Actively engaged in caring responsibilities"= sum(Q11_Simplified=="Actively engaged in caring responsibilities"),
    "Neurodiverse"= sum(Q11_Simplified=="Neurodiverse"),
    "Have a disability"= sum(Q11_Simplified=="Have a disability"),
    "LGBTQIA+"= sum(Q11_Simplified=="LGBTQIA+"),
    "Hispanic or Latino/a/x"= sum(Q11_Simplified=="Hispanic or Latino/a/x"),
    "Black, Indigenous, or a Person of Color (BIPOC)"= sum(Q11_Simplified=="BIPOC")
    
  )

all_survey_participants_long =  all_survey_participants_long %>% 
  pivot_longer(cols = -All, names_to = "Variable", values_to = "Count")
all_survey_participants_long = data.frame(all_survey_participants_long)
all_survey_participants_long$Variable = factor(all_survey_participants_long$Variable, levels = c(
  "Other",
  "Born outside the United States",
  "From a disadvantaged background",
  "First generation college student/professional",
  "Woman",
  "Actively engaged in caring responsibilities",
  "Neurodiverse",
  "Have a disability",
  "LGBTQIA+",
  "Hispanic or Latino/a/x",
  "Black, Indigenous, or a Person of Color (BIPOC)"
))

labels = all_survey_participants_long$Count
labels[which(labels<10 & labels>0)] = "<10"


ggsave(filename = "Q11_Enlarged.png",ggplot(all_survey_participants_long , aes(Count, Variable, label = labels)) +  ggtitle("Do you identify as being a member of one of these groups? Select all that apply to you:") +
         geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 20), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 12.5, show.legend = FALSE, color = all_color) + 
         geom_text(color = "white", size = 5), width = 18, height = 8, dpi = 300, units = "in", device='png')

#############################

#Creating group data with all the necessary groups to join with rbind
data[is.na(data)] = "Filler"
Q2 = separate_rows(data, "Q2_Grouped", sep = ",")

PI_CoI = which(Q2$Q2_Grouped =="PIs/CoIs")

PI_CoI = Q2[PI_CoI,]

PI_CoI$Group = PI_CoI$Q2_Grouped
recover = which(Q2$Q2_Grouped == "RECOVER")
recover = Q2[recover,]
recover$Group = recover$Q2_Grouped


trainees = which(Q2$Q2_Grouped =="Trainees")
trainees = Q2[trainees,]
trainees$Group = trainees$Q2_Grouped

ra = which(Q2$Q2_Grouped =="RAs")
ra = Q2[ra,]
ra$Group = ra$Q2_Grouped

  
works_w_participants = which(data$Q6_4_Grouped== "Works w/ Participants")
works_w_participants = data[c(works_w_participants),]
works_w_participants$Group = works_w_participants$Q6_4_Grouped


Q4 = separate_rows(data, "Q4_Grouped", sep = ",")

summary(factor(Q4$Q4_Grouped))

finance = which(Q4$Q4_Grouped =="Compensated")
finance = Q4[finance,]
finance$Group = finance$Q4_Grouped

volunteers = which(Q4$Q4_Grouped =="Volunteers")
volunteers = Q4[volunteers,]
volunteers$Group = volunteers$Q4_Grouped

summary(factor(data$Q30_Grouped))

manage = which(data$Q30_Grouped=="Managers")
manage = data[manage,]
manage$Group = manage$Q30_Grouped

combined_data = rbind(PI_CoI,recover,trainees,volunteers,works_w_participants,finance,
                      manage,ra)

combined_data = data.frame(combined_data[c("ID","Group", "Q8_Simplified", "Q9", "Q10", "Q11_Simplified", "Q5_4_Grouped")])
combined_data$Group = factor(combined_data$Group, levels=c("Volunteers",
                                                           "Compensated",
                                                           "Managers",
                                                           "Works w/ Participants",
                                                           "RECOVER",
                                                           "RAs",
                                                           "Trainees",
                                                           "PIs/CoIs"))


colnames(combined_data) = c("ID", "Group","Education","Gender", "Race_Ethnicity","Group_Membership","Working_Hours")

 #Summarizing grouped data
Education = combined_data %>% 
  group_by(Group) %>%
  summarise("High School" = sum(Education=="High School"),
            "Some College" = sum(Education=="Some College"),
            "Associate’s" = sum(Education=="Associate’s"),
            "Bachelor’s" = sum(Education=="Bachelor’s"),
            "Master’s" = sum(Education=="Master’s"),
            "Ph.D./M.D." = sum(Education=="Ph.D./M.D."),
            "Other"= sum(Education=="Other"))

Gender= combined_data %>% 
  group_by(Group) %>%
  summarise(
    "Man/Trans Man" = sum(Gender=="Man/Trans Man"),
    "Woman/Trans Woman" = sum(Gender=="Woman/Trans Woman"),
    "Non-Binary/Gender Queer/Gender Non-Conforming" = sum(Gender=="Non-Binary/Gender Queer/Gender Non-Conforming"),
    "Other"= sum(Gender=="Other"))

#Participants could select multiple choices for this variable 
# Each choice is seperated by a comma 
group =  separate_rows(combined_data, "Group_Membership", sep = ",")  
Group_Membership=group %>% 
  group_by(Group) %>%
  summarise(
    "LGBTQIA+" = sum(Group_Membership=="LGBTQIA+"),
    "Have a disability" = sum(Group_Membership=="Have a disability"),
    "Neurodiverse" = sum(Group_Membership=="Neurodiverse"),
    "Actively engaged in caring responsibilities" = sum(Group_Membership=="Actively engaged in caring responsibilities"),
    "Born outside the United States" = sum(Group_Membership=="Born outside the United States"),
    "First generation college student/professional" = sum(Group_Membership=="First generation college student/professional"),
    "Other"= sum(Group_Membership=="Other"),
    "Woman"= sum(Group_Membership=="Woman"),
    "Hispanic or Latino/a/x"= sum(Group_Membership=="Hispanic or Latino/a/x"),
    "Black, Indigenous, or a Person of Color (BIPOC)"= sum(Group_Membership=="Black, Indigenous, or a Person of Color (BIPOC)"),
    "From a disadvantaged background"= sum(Group_Membership=="From a disadvantaged background"))

Working_Hours=combined_data %>% 
  group_by(Group) %>%
  summarise(
    "0-10" = sum(Working_Hours=="0-10" ),
    "11-20" = sum(Working_Hours=="11-20"),
    "21-30" = sum(Working_Hours=="21-30") ,
    "31-40" = sum(Working_Hours=="31-40")
  )
  
#Participants could select multiple choices for this variable 
# Each choice is seperated by a comma 
race =  separate_rows(combined_data, "Race_Ethnicity", sep = ",")
Race_Ethnicity=race %>% 
  group_by(Group) %>%
  summarise(
    "Asian or Pacific Islander" = sum(Race_Ethnicity== "Asian or Pacific Islander"),
    "Black or African American" = sum(Race_Ethnicity=="Black or African American"),
    "Hispanic or Latino/a/x" = sum(Race_Ethnicity=="Hispanic or Latino/a/x"),
    "Native American or Alaskan Native" = sum(Race_Ethnicity=="Native American or Alaskan Native" ),
    "White" = sum(Race_Ethnicity=="White"),
    "Multiracial or Biracial" = sum(Race_Ethnicity=="Multiracial or Biracial" ),
    "Middle Eastern/North African" = sum(Race_Ethnicity=="Middle Eastern/North African"),
    "Other race/ethnicity not listed here"=sum(Race_Ethnicity=="Other race/ethnicity not listed here"))


#Converting counts to percentages for each data frame using a function

create_long_dataframe = function(dataframe){
  
  long_dataframe =  dataframe %>% 
    pivot_longer(cols = -Group, names_to = "Variable", values_to = "Percentage")
  long_dataframe = data.frame(long_dataframe)
  n = summary(factor(combined_data[["Group"]], exclude = NA))
  
  summary(factor(long_dataframe[["Group"]], exclude = NA))
  
  long_dataframe[which(long_dataframe$Group=="Volunteers"), "Percentage"] = round(
    long_dataframe[which(long_dataframe$Group=="Volunteers"), "Percentage"]/n[[1]]*100,
    0
  )
  long_dataframe[which(long_dataframe$Group=="Compensated"), "Percentage"] = round(
    long_dataframe[which(long_dataframe$Group=="Compensated"), "Percentage"]/n[[2]]*100,
    0
  )
  long_dataframe[which(long_dataframe$Group=="Managers"), "Percentage"] = round(
    long_dataframe[which(long_dataframe$Group=="Managers"), "Percentage"]/n[[3]]*100,
    0
  )
  long_dataframe[which(long_dataframe$Group=="Works w/ Participants"), "Percentage"] = round(
    long_dataframe[which(long_dataframe$Group=="Works w/ Participants"), "Percentage"]/n[[4]]*100,
    0
  )
  long_dataframe[which(long_dataframe$Group=="RECOVER"), "Percentage"] = round(
    long_dataframe[which(long_dataframe$Group=="RECOVER"), "Percentage"]/n[[5]]*100,
    0
  )
  long_dataframe[which(long_dataframe$Group=="RAs"), "Percentage"] = round(
    long_dataframe[which(long_dataframe$Group=="RAs"), "Percentage"]/n[[6]]*100,
    0
  )
  long_dataframe[which(long_dataframe$Group=="Trainees"), "Percentage"] = round(
    long_dataframe[which(long_dataframe$Group=="Trainees"), "Percentage"]/n[[7]]*100,
    0
  )
  long_dataframe[which(long_dataframe$Group=="PIs/CoIs"), "Percentage"] = round(
    long_dataframe[which(long_dataframe$Group=="PIs/CoIs"), "Percentage"]/n[[8]]*100,
    0
  )
  return(long_dataframe)
  
}

Education = create_long_dataframe(Education)
Gender = create_long_dataframe(Gender)
Race_Ethnicity = create_long_dataframe(Race_Ethnicity)
Group_Membership = create_long_dataframe(Group_Membership)
Working_Hours = create_long_dataframe(Working_Hours)

#Ordering factors in each dataframe
Education$Variable = factor(Education$Variable, levels = c(
  "Other",
  "Ph.D./M.D.",
  "Master’s",
  "Bachelor’s",
  "Associate’s",
  "Some College",
  "High School"
  
))

Gender$Variable = factor(Gender$Variable, levels = c(
  "Other",
  "Non-Binary/Gender Queer/Gender Non-Conforming",
  "Woman/Trans Woman",
  "Man/Trans Man"
  
))




Group_Membership$Variable = factor(Group_Membership$Variable, levels = c(
  "Other",
  "Born outside the United States",
  "From a disadvantaged background",
  "First generation college student/professional",
  "Woman",
  "Actively engaged in caring responsibilities",
  "Neurodiverse",
  "Have a disability",
  "LGBTQIA+",
  "Hispanic or Latino/a/x",
  "Black, Indigenous, or a Person of Color (BIPOC)"
))



Race_Ethnicity$Variable = factor(Race_Ethnicity$Variable, levels = c(
  "Other race/ethnicity not listed here",
  "Multiracial or Biracial",
  "Middle Eastern/North African",
  "White",
  "Native American or Alaskan Native",
  "Hispanic or Latino/a/x",
  "Black or African American",
  "Asian or Pacific Islander"
))



Working_Hours$Variable = factor(Working_Hours$Variable, levels = c(
  "31-40",
  "21-30",
  "11-20",
  "0-10"
  
))

#Code to create a color vector the same length as the variable and plot the variable
#Education is replaced with the Working_Hours, Gender, Race_Ethnicity, and Group_Membership dataframes 
#to create different plots

colors = c("#6639B5","#9B25AE", "#1F95F1", "#009586", "#4AAE4F", "#FEC006", "#FE5521", "#E81C62")
new_colours = c()

#Function to create plots
create_plot = function(file_name,dataframe){
  for(x in colors){
    #Color vector needs to match the rows in the Group column
    new_colours = c(new_colours, rep(x, nrow(dataframe[dataframe$Group == "Volunteers",])))
  }

  
  ggsave(filename = file_name,ggplot(dataframe, aes(Percentage, Variable, label = paste0(Percentage, "%"))) + 
           geom_segment(aes(x = 0, y = Variable, xend = Percentage, yend = Variable), show.legend = FALSE, color = new_colours) + theme_bw() + facet_wrap(~Group, ncol = 4) + 
           theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 10), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x = element_text(size=8), axis.text.y = element_text(size=8),
                 strip.background=element_rect(colour="black",
                                               fill="grey95"))+ geom_point(size = 5, show.legend = FALSE, color = new_colours) +
           expand_limits(x=c(0,100)) + geom_text(color = "white", size = 2), width = 7, height = 3, dpi = 300, units = "in", device='png')
  
}

create_plot("Education.png", Education)
create_plot("Working_Hours.png", Working_Hours)
create_plot("Gender.png", Gender)
create_plot(" Group_Membership.png", Group_Membership)
create_plot("Race_Ethnicity.png", Race_Ethnicity)





