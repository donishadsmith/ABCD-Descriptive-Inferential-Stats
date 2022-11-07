#Will be adding more detailed comments
#This is a real dataframe that I had to analyze and generate lollipop plots for.
#Names of certain variables have been converted to Group_W, Group_M, for privacy reasons
#I generated many more plotsthan this. I just took a single example from my actual script
#This script creates the Education.png file located in this repo
pacman::p_load(dplyr, ggplot2, tidyr)
data = readxl::read_xlsx("~/data.xlsx")
data = data[2:nrow(data),]
data = data.frame(ID = 1:nrow(data), data)


Q6_4_Grouped = c()
for(i in data$Q6_4){
  if(i %in% c(0, NA)){
    convert = NA
  }
  else{
    convert = 'Group_W'
  }
  Q6_4_Grouped = c(Q6_4_Grouped ,convert)
}

data$Q6_4_Grouped  =Q6_4_Grouped 


#############################

#Grouped data
Q2 = separate_rows(data, "Q2_Grouped", sep = ",")
summary(factor(Q2$Q2_Grouped, exclude = NA))


Group_P = which(Q2$Q2_Grouped =="Group_P")
Group_P= Q2[Group_P,]
Group_P$Group = Group_P$Q2_Grouped

Group_R = which(Q2$Q2_Grouped == "Group_R")
Group_R = Q2[Group_R,]
Group_R$Group = Group_R$Q2_Grouped


Group_T = which(Q2$Q2_Grouped =="Group_T")
Group_T = Q2[Group_T,]
Group_T$Group = Group_T$Q2_Grouped

Group_R2 = which(Q2$Q2_Grouped =="Group_R2")
Group_R2 = Q2[Group_R2,]
Group_R2$Group = Group_R2$Q2_Grouped

  
Group_W= which(data$Q6_4_Grouped == "Group_W")
Group_W = data[c(Group_W),]
Group_W$Group = Group_W$Q6_4_Grouped


Q4 = separate_rows(data, "Q4_Grouped", sep = ",")

summary(factor(Q4$Q4_Grouped))

Group_C = which(Q4$Q4_Grouped =="Group_C")
Group_C = Q4[Group_C,]
Group_C$Group = Group_C$Q4_Grouped

Group_V = which(Q4$Q4_Grouped =="Group_V")
Group_V = Q4[Group_V,]
Group_V$Group = Group_V$Q4_Grouped

summary(factor(data$Q30_Grouped))

Group_M = which(data$Q30_Grouped=="Group_M")
Group_M = data[Group_M,]
Group_M$Group = Group_M$Q30_Grouped

combined_data = rbind(Group_P,Group_R,Group_T,Group_V,Group_W,Group_C,
                      Group_M,Group_R2)

combined_data = data.frame(combined_data[c("ID","Group", "Q8_Simplified")])
summary(factor(combined_data$Group))
combined_data$Group = factor(combined_data$Group, levels=c("Group_V",
                                                           "Group_C",
                                                           "Group_M",
                                                           "Group_W",
                                                           "Group_R",
                                                           "Group_R2",
                                                           "Group_T",
                                                           "Group_P"))


summary(factor(combined_data$Group))
colnames(combined_data) = c("ID", "Group","Education")

combined_data$ID = 1:nrow(combined_data)


binary_data = data.frame("ID"= combined_data$ID, "Group"= combined_data$`Group`, "High_school_or_equivalent"= rep(NA, nrow(combined_data)), "Some_college" = rep(NA, nrow(combined_data)), "Associates_Degree" = rep(NA, nrow(combined_data)),"Bachelors_Degree" = rep(NA, nrow(combined_data)),
                         "Masters_Degree" = rep(NA, nrow(combined_data)), "PhD_ProfessionalDegree" = rep(NA, nrow(combined_data)), "Other_Ed" = rep(NA, nrow(combined_data)))



summary(factor(combined_data$Education, exclude = NA))


binary_data[c(which(combined_data$Education== "High School")), "High_school_or_equivalent"] = "Yes"
binary_data[c(which(combined_data$Education == "Some College")), "Some_college"] = "Yes"
binary_data[c(which(combined_data$Education == "Associate’s")), "Associates_Degree"] = "Yes"
binary_data[c(which(combined_data$Education == "Bachelor’s")), "Bachelors_Degree"] = "Yes"
binary_data[c(which(combined_data$Education == "Master’s")), "Masters_Degree" ] = "Yes"
binary_data[c(which(combined_data$Education == "Ph.D./M.D.")), "PhD_ProfessionalDegree"] = "Yes"
binary_data[c(which(combined_data$Education == "Other")), "Other_Ed"] = "Yes"



binary_data[is.na(binary_data)] <- "No"





Education = binary_data %>% 
  group_by(Group) %>%
  summarise("High School" = sum(High_school_or_equivalent=="Yes"),
            "Some College" = sum(Some_college=="Yes"),
            "Associate’s" = sum(Associates_Degree=="Yes"),
            "Bachelor’s" = sum(Bachelors_Degree=="Yes"),
            "Master’s" = sum(Masters_Degree=="Yes"),
            "Ph.D./M.D." = sum(PhD_ProfessionalDegree=="Yes"),
            "Other"= sum(Other_Ed=="Yes"))



Education =  Education%>% 
  pivot_longer(cols = -Group, names_to = "Variable", values_to = "Percentage")
Education = data.frame(Education)
n = summary(factor(combined_data[["Group"]], exclude = NA))
n
summary(factor(Education[["Group"]], exclude = NA))

Education[which(Education$Group=="Group_V"), "Percentage"] = round(
  Education[which(Education$Group=="Group_V"), "Percentage"]/n[[1]]*100,
  0
)
Education[which(Education$Group=="Group_C"), "Percentage"] = round(
  Education[which(Education$Group=="Group_C"), "Percentage"]/n[[2]]*100,
  0
)
Education[which(Education$Group=="Group_M"), "Percentage"] = round(
  Education[which(Education$Group=="Group_M"), "Percentage"]/n[[3]]*100,
  0
)
Education[which(Education$Group=="Group_W"), "Percentage"] = round(
  Education[which(Education$Group=="Group_W"), "Percentage"]/n[[4]]*100,
  0
)
Education[which(Education$Group=="Group_R"), "Percentage"] = round(
  Education[which(Education$Group=="Group_R"), "Percentage"]/n[[5]]*100,
  0
)
Education[which(Education$Group=="Group_R2"), "Percentage"] = round(
  Education[which(Education$Group=="Group_R2"), "Percentage"]/n[[6]]*100,
  0
)
Education[which(Education$Group=="Group_T"), "Percentage"] = round(
  Education[which(Education$Group=="Group_T"), "Percentage"]/n[[7]]*100,
  0
)
Education[which(Education$Group=="Group_P"), "Percentage"] = round(
  Education[which(Education$Group=="Group_P"), "Percentage"]/n[[8]]*100,
  0
)




Education$Variable = factor(Education$Variable, levels = c(
  "Other",
  "Ph.D./M.D.",
  "Master’s",
  "Bachelor’s",
  "Associate’s",
  "Some College",
  "High School"
  
))



#labels = Education$Percentage
#labels[which(labels<10 & labels>0)] = "<10"
new_colors = c()

colors = c("#6639B5","#9B25AE", "#1F95F1", "#009586", "#4AAE4F", "#FEC006", "#FE5521", "#E81C62")
for(x in colors){
  new_colors = c(new_colors, rep(x,7))
}




ggsave(filename = "Education.png",ggplot(Education, aes(Percentage, Variable, label = paste0(Percentage, "%"))) + 
         geom_segment(aes(x = 0, y = Variable, xend = Percentage, yend = Variable), show.legend = FALSE, color = new_colors) + theme_bw() + facet_wrap(~Group, ncol = 4) + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 10), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x = element_text(size=8), axis.text.y = element_text(size=8),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 5, show.legend = FALSE, color = new_colors) +
         expand_limits(x=c(0,100)) + geom_text(color = "white", size = 2), width = 7, height = 3, dpi = 300, units = "in", device='png')


