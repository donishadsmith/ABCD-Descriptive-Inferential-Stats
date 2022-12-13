#Script to create the Dummy_Education_Plot_Example.png
#These were the type of plots were generated from the ABCD Descriptive script
#Import excel sheet
pacman::p_load(dplyr, ggplot2, tidyr)
data = readxl::read_xlsx("~/Documents/ABCD Demographics Survey.xlsx")
data = data[2:nrow(data),]
data = data.frame(ID = 1:nrow(data), data)

#Renaming variables in dataframe

data = data %>% 
  mutate(Q6_4_Grouped = case_when(
    Q6_4 == '0' ~ 'Does not work w/ participants',
    Q6_4 != '0' ~ 'Works w/ Participants' 
  ))

data = data %>% 
  mutate(Q5_4_Grouped = case_when(
    Q5_4 %in% c(0:10) ~ '0-10',
    Q5_4 %in% c(11:20) ~ '11-20',
    Q5_4 %in% c(21:30) ~ '21-30',
    Q5_4 %in% c(31:40) ~ '31-40',
  ))



# For each question, count data is grouped into a longform dataframe for plotting
data[is.na(data)] = "Filler"
#############################
#Plotting for specific groups
#Creating group data with all the necessary groups to join with rbind
data = data[,c("Q2_Grouped","Q4_Grouped","Q6_4_Grouped","Q30_Grouped", "Q8_Simplified", "Q9", "Q10", "Q11_Simplified", "Q5_4_Grouped")]
Q2_seperated_data = separate_rows(data, "Q2_Grouped", sep = ",")
Q2_seperated_data = Q2_seperated_data  %>% mutate(Group = Q2_Grouped )
Q6_4_seperated_data = Q30_seperated_data = data
Q6_4_seperated_data = Q6_4_seperated_data  %>% mutate(Group = Q6_4_Grouped)
Q30_seperated_data = Q2_seperated_data  %>% mutate(Group = Q30_Grouped )
Q4_seperated_data = separate_rows(data, "Q4_Grouped", sep = ",")
Q4_seperated_data = Q4_seperated_data  %>% mutate(Group = Q4_Grouped )

combined_data = rbind(Q2_seperated_data[which(Q2_seperated_data$Group =="PIs/CoIs"),],
                      Q2_seperated_data[which(Q2_seperated_data$Group  =="RECOVER"),],
                      Q2_seperated_data[which(Q2_seperated_data$Group  =="Trainees"),],
                      Q2_seperated_data[which(Q2_seperated_data$Group  =="RAs"),],
                      Q6_4_seperated_data[which(Q6_4_seperated_data$Group == "Works w/ Participants"),],
                      Q30_seperated_data[which(Q30_seperated_data$Group == "Managers"),],
                      Q4_seperated_data[which(Q4_seperated_data$Group =="Volunteers"),],
                      Q4_seperated_data[which(Q4_seperated_data$Group=="Compensated"),]
)

combined_data = data.frame(combined_data[c("Group", "Q8_Simplified", "Q9", "Q10", "Q11_Simplified", "Q5_4_Grouped")]) 

colnames(combined_data ) = c("Group","Education","Gender", "Race_Ethnicity","Group_Membership","Working_Hours")

combined_data$Group = factor(combined_data$Group, levels=c("Volunteers",
                                                           "Compensated",
                                                           "Managers",
                                                           "Works w/ Participants",
                                                           "RECOVER",
                                                           "RAs",
                                                           "Trainees",
                                                           "PIs/CoIs"))


#Function to create dataframe for plotting 
create_dataframe = function(data, column_name, elongate){
  if (elongate == "Yes"){
    data = data.frame(separate_rows(data, column_name, sep = ","))
  }
  #Retrieve names of factors
  group_factor_levels = levels(factor(data[,"Group"]))
  variable_factor_levels =levels(factor(data[,column_name], exclude = c("Filler", NA)))
  
  #Create dataframe
  survey_participants = data.frame(matrix(nrow = length(group_factor_levels) * length(variable_factor_levels), ncol = 3))
  colnames(survey_participants) = c("Group", "Variable", "Percentage")
  
  counts = c()
  group_vector = c()
  variable_names = c()
  #Counting number of factors in dataframe column
  for(group_factor_level in group_factor_levels){
    group_vector  = c(group_vector,rep(group_factor_level, length(variable_factor_levels)))
    for(variable_factor_level in variable_factor_levels){
      variable_names = c(variable_names, variable_factor_level)
      counts = c(counts, nrow(data[which(data[,"Group"] == group_factor_level & data[,column_name] == variable_factor_level ),]))
    }
  }
  
  
  
  survey_participants$Group = group_vector
  survey_participants$Variable = variable_names
  survey_participants$Percentage= counts
  
  return(survey_participants)
}



Education = create_dataframe(data = combined_data, column_name = "Education", elongate = "No")

#Generate random data
groups = Education["Group"]; groups = as.character(groups$Group); groups = unique(groups)


education_groups = unique(Education$Variable)


for(group in groups){
  prob = 0
  #While loop to create probabilities for each education variable
  #Purpose is to replicate a sample that where the probabilities do not exceed 100 and are not less than 80
  #Creates a unique distribution for each of the groups for plotting
  while(!(sum(prob) < 100 & sum(prob) > 60)){
    #Max is 100/length(education_groups) so that even in the incredibly rare chance that all variables qual the max
    #The max will not exceed 100
    prob = round(runif(length(education_groups), min = 0, max = 100/length(unique(education_groups))),0)
  }
  #Convert to percent
  Education[Education$Group  == group,"Percentage"] = prob
  
}

Education[which(Education$Variable == "Other:______________"), "Variable"] = "Other"
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

create_plot("Education_Dummy_Plot.png", Education)

