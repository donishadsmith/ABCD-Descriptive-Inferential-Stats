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
#Color for all graphs
all_color = "#3d50b3"

#Function to create dataframe for plotting 
create_dataframe= function(data, column_name, elongate){
  if (elongate == "Yes"){
    data = data.frame(separate_rows(data, column_name, sep = ","))
  }
  #Retrieve names of factors
  factor_levels = levels(factor(data[,column_name], exclude = c("Filler", NA)))
  
  #Create dataframe
  all_survey_participants = data.frame(matrix(nrow = length(factor_levels), ncol = 2))
  colnames(all_survey_participants) = c("Variable", "Count")
  
  counts = c()
  #Counting number of factors in dataframe column
  for(factor_level in factor_levels){
    counts = c(counts,sum(data[,column_name] == factor_level))
  }
 
    all_survey_participants$Variable = factor_levels
    all_survey_participants$Count = counts
    
    return(all_survey_participants)
     
  }

#Creating plotting function 
#Individual function to intervene and make additions to dataframe before plotting
create_plot = function(file_name,data,labels,plot_title, element_text_size,
                       geom_point_size, geom_text_size,plot_width, plot_height){
  
  #Counts less than 10 = '<10'
  labels = data[,'Count']
  labels[which(labels<10)] = "<10"
  
  ggsave(filename = file_name,ggplot(data , aes(Count, Variable, label = labels)) +  ggtitle(plot_title) +
           geom_segment(aes(x = 0, y = Variable, xend = Count, yend = Variable), show.legend = FALSE, color = all_color) + theme_bw() + 
           theme(plot.title = element_text(hjust = 0.5),text = element_text(size = element_text_size), panel.grid.major = element_blank(),  axis.title.y=element_blank(), axis.title.x=element_blank(),
                 strip.background=element_rect(colour="black",
                                               fill="grey95"))+ geom_point(size = geom_point_size, show.legend = FALSE, color = "#3d50b3") + 
           geom_text(color = "white", size = geom_text_size), width = plot_width, height = plot_height, dpi = 300, units = "in", device='png')
  
}


#Q27
all_survey_participants = create_dataframe(data = data,column_name = "Q27", elongate = "No")
all_survey_participants= rbind(all_survey_participants, c("No",0))
all_survey_participants$Count = as.numeric(all_survey_participants$Count )

create_plot(file_name = 'Q27.png',
            data = all_survey_participants,
            plot_title = "Our records indicate that you are currently engaged in the ABCD Study. If this is correct, please respond “Yes”.",
            geom_point_size = 10,
            geom_text_size = 4,
            element_text_size = 20,
            plot_width = 20,
            plot_height = 8
)

#Q1

all_survey_participants = create_dataframe(data = data,column_name = "Q1", elongate = "No")

create_plot(file_name = 'Q1.png',
            data = all_survey_participants,
            plot_title = "Are you willing to disclose your role in the ABCD Study as part of this survey (e.g., co-investigator, postdoctoral fellow, research assistant)?",
            geom_point_size = 14,
            geom_text_size = 6,
            element_text_size = 23,
            plot_width = 25,
            plot_height = 9
)

#Q2

all_survey_participants = create_dataframe(data = data,column_name = "Q2", elongate = "Yes")

create_plot(file_name = 'Q2.png',
            data = all_survey_participants,
            plot_title = "What is your current role in the ABCD Study? Select all that apply to you:",
            geom_point_size = 11,
            geom_text_size = 5,
            element_text_size = 20,
            plot_width = 22,
            plot_height = 8
)

#Q28

all_survey_participants = create_dataframe(data = data,column_name = "Q28", elongate = "No")

create_plot(file_name = 'Q28.png',
            data = all_survey_participants,
            plot_title = "Site PI/MPI",
            geom_point_size = 9,
            geom_text_size = 5,
            element_text_size = 20,
            plot_width = 13,
            plot_height = 8
)

#Q29

all_survey_participants = create_dataframe(data = data,column_name = "Q29", elongate = "No")

create_plot(file_name = 'Q29.png',
            data = all_survey_participants,
            plot_title = "Co-Investigator",
            geom_point_size = 9,
            geom_text_size = 5,
            element_text_size = 20,
            plot_width = 13,
            plot_height = 8
)


#Q30

all_survey_participants = create_dataframe(data = data,column_name = "Q30", elongate = "No")

create_plot(file_name = 'Q30.png',
            data = all_survey_participants,
            plot_title = "Do you have a managerial role on the ABCD Study (i.e., do you supervise other ABCD team members)?",
            geom_point_size = 13.5,
            geom_text_size = 6,
            element_text_size = 24,
            plot_width = 24,
            plot_height = 8
)

#Q3

all_survey_participants = create_dataframe(data = data,column_name = "Q3", elongate = "No")

create_plot(file_name = 'Q3.png',
            data = all_survey_participants,
            plot_title = "What year did you join the ABCD Study?",
            geom_point_size = 9,
            geom_text_size = 4,
            element_text_size = 20,
            plot_width = 13,
            plot_height = 8
)

#Q4

all_survey_participants = create_dataframe(data = data,column_name = "Q4", elongate = "Yes")

create_plot(file_name = 'Q4.png',
            data = all_survey_participants,
            plot_title = "Are you financially compensated for your contributions to the ABCD Study? Select all that apply to you:",
            geom_point_size = 15,
            geom_text_size = 7,
            element_text_size = 25,
            plot_width = 29,
            plot_height = 8
)

#Q5

all_survey_participants = create_dataframe(data = data,column_name = "Q5", elongate = "No")

create_plot(file_name = 'Q5.png',
            data = all_survey_participants,
            plot_title = "How many total hours per week (on average) do you spend working on the ABCD Study?",
            geom_point_size = 9,
            geom_text_size = 4,
            element_text_size = 20,
            plot_width = 15,
            plot_height = 8)

#Q6

all_survey_participants = create_dataframe(data = data,column_name = "Q6", elongate = "No")

create_plot(file_name = 'Q6.png',
            data = all_survey_participants,
            plot_title = "How many hours per week (on average) is spent interacting (in person or virtually) with ABCD participants?",
            geom_point_size = 11,
            geom_text_size = 5,
            element_text_size = 20,
            plot_width = 18,
            plot_height = 8)

#Q7

all_survey_participants = create_dataframe(data = data,column_name = "Q7", elongate = "No")

create_plot(file_name = 'Q7.png',
            data = all_survey_participants,
            plot_title = "Are you willing to disclose your demographic information as part of this survey?",
            geom_point_size = 9,
            geom_text_size = 4,
            element_text_size = 20,
            plot_width = 13,
            plot_height = 8)

#Q8

all_survey_participants = create_dataframe(data = data,column_name = "Q8", elongate = "No")

create_plot(file_name = 'Q8.png',
            data = all_survey_participants,
            plot_title = "What is your current highest level of education?",
            geom_point_size = 9,
            geom_text_size = 4,
            element_text_size = 20,
            plot_width = 13,
            plot_height = 8)


#Q9

all_survey_participants = create_dataframe(data = data,column_name = "Q9", elongate = "No")

create_plot(file_name = 'Q9.png',
            data = all_survey_participants,
            plot_title = "What is your current gender identity?",
            geom_point_size = 9,
            geom_text_size = 4,
            element_text_size = 20,
            plot_width = 13,
            plot_height = 8)

#Q10

all_survey_participants = create_dataframe(data = data,column_name = "Q10", elongate = "Yes")

create_plot(file_name = 'Q10.png',
            data = all_survey_participants,
            plot_title = "How would you describe yourself? Select all that apply to you:",
            geom_point_size = 9,
            geom_text_size = 4,
            element_text_size = 20,
            plot_width = 13,
            plot_height = 8)

#Q11

all_survey_participants = create_dataframe(data = data,column_name = "Q11", elongate = "Yes")

create_plot(file_name = 'Q11.png',
            data = all_survey_participants,
            plot_title = "Do you identify as being a member of one of these groups? Select all that apply to you:",
            geom_point_size = 12.5,
            geom_text_size = 5,
            element_text_size = 20,
            plot_width = 18,
            plot_height = 8)




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
Group = create_dataframe(data = combined_data, column_name = "Group_Membership", elongate = "Yes")
Gender = create_dataframe(data = combined_data, column_name = "Gender", elongate = "No")
Race = create_dataframe(data = combined_data, column_name = "Race_Ethnicity", elongate = "Yes")
Working_Hours = create_dataframe(data = combined_data, column_name = "Working_Hours", elongate = "No")

#Function to convert counts into percents
convert_to_percent = function(dataframe){
  
  long_dataframe =  dataframe 
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

Education = convert_to_percent(Education)
Gender = convert_to_percent(Gender)
Race = convert_to_percent(Race)
Group_Membership = convert_to_percent(Group_Membership)
Working_Hours = convert_to_percent(Working_Hours)



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

