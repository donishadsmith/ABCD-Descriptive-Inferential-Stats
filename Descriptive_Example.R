#Script to create the Dummy_Education_Plot_Example.png
#These were the type of plots were generated from the ABCD Descriptive script
pacman::p_load(dplyr, ggplot2, tidyr)
data = readxl::read_xlsx("~/data.xlsx")
data = data[2:nrow(data),]
data = data.frame(ID = 1:nrow(data), data)
data[is.na(data)] = "Filler"

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
#Survey was multiple choice and multiple choice answers were seperated by commas
Q2 = separate_rows(data, "Q2_Grouped", sep = ",")
summary(factor(Q2$Q2_Grouped, exclude = NA))


Group_R = which(Q2$Q2_Grouped == "Group_R")
Group_R = Q2[Group_R,]
Group_R$Group = Group_R$Q2_Grouped


Group_T = which(Q2$Q2_Grouped =="Group_T")
Group_T = Q2[Group_T,]
Group_T$Group = Group_T$Q2_Grouped

  
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

combined_data = rbind(Group_R,Group_T,Group_V,Group_W,Group_C,
                      Group_M)

combined_data = data.frame(combined_data[c("ID","Group", "Q8_Simplified")])
summary(factor(combined_data$Group))
combined_data$Group = factor(combined_data$Group, levels=c("Group_V",
                                                           "Group_C",
                                                           "Group_M",
                                                           "Group_W",
                                                           "Group_R",
                                                           "Group_T"
                                                           ))


summary(factor(combined_data$Group))
colnames(combined_data) = c("ID", "Group","Education")

combined_data$ID = 1:nrow(combined_data)

#Generate random data

groups = combined_data["Group"]; groups = as.character(groups$Group); groups = unique(groups)
groups = sample(groups, nrow(combined_data), replace = T)

combined_data["Group"] = groups

education_groups = combined_data["Education"]; education_groups = unique(education_groups$Education)


for(group in groups){
  prob = 0
  #While loop to create probabilities for each education variable
  #Purpose is to replicate a sample that where the probabilities do not exceed 100 and are not less than 80
  #Creates a unique distribution for each of the groups for plotting
  while(!(sum(prob) < 100 & sum(prob) > 80 )){
    #Max is 100/length(education_groups) so that even in the incredibly rare chance that all variables qual the max
    #The max will not exceed 100
    prob = runif(length(education_groups), min = 0, max = 100/length(education_groups))
  }
  #Convert to percent
  prob = prob/100
  combined_data[combined_data$Group  == group,"Education"] = sample(education_groups, nrow(combined_data[combined_data$Group  == group,]), replace = T, prob = prob)
  
}


#Using tidyr to summarize information by group
Education = combined_data %>% 
  group_by(Group) %>%
  summarise("High School" = sum(Education=="High School"),
            "Some College" = sum(Education=="Some College"),
            "Associate’s" = sum(Education=="Associate’s"),
            "Bachelor’s" = sum(Education=="Bachelor’s"),
            "Master’s" = sum(Education=="Master’s"),
            "Ph.D./M.D." = sum(Education=="Ph.D./M.D."),
            "Other"= sum(Education=="Other"))


Education =  Education%>% 
  pivot_longer(cols = -Group, names_to = "Variable", values_to = "Percentage")
Education = data.frame(Education)


#Convert counts to percentages
combined_data[which(combined_data$Group == groups[1]),]
n = summary(factor(combined_data[["Group"]], exclude = NA))


summary(factor(Education[["Group"]], exclude = NA))

Education[which(Education$Group=="Group_C"), "Percentage"] = round(
  Education[which(Education$Group=="Group_C"), "Percentage"]/n[[1]]*100,
  0
)

Education[which(Education$Group=="Group_M"), "Percentage"] = round(
  Education[which(Education$Group=="Group_M"), "Percentage"]/n[[2]]*100,
  0
)

Education[which(Education$Group=="Group_R"), "Percentage"] = round(
  Education[which(Education$Group=="Group_R"), "Percentage"]/n[[3]]*100,
  0
)

Education[which(Education$Group=="Group_T"), "Percentage"] = round(
  Education[which(Education$Group=="Group_T"), "Percentage"]/n[[4]]*100,
  0
)

Education[which(Education$Group=="Group_V"), "Percentage"] = round(
  Education[which(Education$Group=="Group_V"), "Percentage"]/n[[5]]*100,
  0
)


Education[which(Education$Group=="Group_W"), "Percentage"] = round(
  Education[which(Education$Group=="Group_W"), "Percentage"]/n[[6]]*100,
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



new_colors = c()

colors = c("#6639B5","#9B25AE", "#1F95F1", "#009586", "#4AAE4F", "#FEC006")
#Length of color vector needs to equal length of rows
#Each group needs its own unique color
for(x in colors){
  new_colors = c(new_colors, rep(x,7))
}



ggsave(filename = "Dummy_Education_Plot_Example.png",ggplot(Education, aes(Percentage, Variable, label = paste0(Percentage, "%"))) + 
         geom_segment(aes(x = 0, y = Variable, xend = Percentage, yend = Variable), show.legend = FALSE, color = new_colors) + theme_bw() + facet_wrap(~Group, ncol = 3) + 
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size = 10), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(), axis.text.x = element_text(size=8), axis.text.y = element_text(size=8),
               strip.background=element_rect(colour="black",
                                             fill="grey95"))+ geom_point(size = 4, show.legend = FALSE, color = new_colors) +
         expand_limits(x=c(0,100)) + geom_text(color = "white", size = 1.5), width = 7, height = 3, dpi = 300, units = "in", device='png')

for(group in unique(groups)){
 x = sum(Education[Education$Group  == group,"Percentage"])
  print(x)
}
