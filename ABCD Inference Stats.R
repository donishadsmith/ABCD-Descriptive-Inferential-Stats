#Importing excel file into R
pacman::p_load(dplyr, tidyr)
data = readxl::read_xlsx("~/ABCD.xlsx")
data = data[2:nrow(data),]
data = data.frame(ID = 1:nrow(data), data)


#Renaming variables in dataframe

data = data %>% 
  mutate(Race = case_when(
    Race == 'White' ~ 'Non-Hispanic White',
    Race != 'White' ~ 'POC' 
  ))

data = data %>% 
  mutate(Turnover = case_when(
    Turnover %in% c(0:10) ~ '0-10',
    Turnover %in% c(11:20) ~ '11-20',
    Turnover %in% c(21:30) ~ '21-30',
    Turnover %in% c(31:40) ~ '31-40',
  ))

data = data %>% 
  mutate(Time = case_when(
    Time %in% c(2015:2017) ~ 0,
    Time %in% c(2021:2022) ~ 1
  ))



#Creating a new column in dataframe named Finance_Groups
data$Finance_Groups = rep(NA, nrow(data))
#Categorizing individuals based on the certain criteria
# For instance, those that are PI/CoIs and are compensated will b classified as Compensated PIs/CoIs in the Finance_Groups column
#Used for chisquare tests
data = data %>% 
  mutate(Finance_Group = case_when(
    Roles == "PIs/CoIs" & Finance == "Compensated"  ~ 'Compensated PIs/CoIs',
    Roles == "PIs/CoIs" & Finance == "Volunteers"  ~ 'Volunteer PIs/CoIs',
    Roles == "RAs" & Finance == "Compensated"  ~ 'Compensated RAs',
    Roles == "RAs" & Finance == "Volunteers"  ~ 'Volunteer RAs',
    Roles == "RECOVER" & Finance == "Compensated"  ~ 'Compensated RECOVER',
    Roles == "RECOVER" & Finance == "Volunteers"  ~ 'Volunteer RECOVER',
    Roles == "Trainees" & Finance == "Compensated"  ~ 'Compensated Trainees',
    Roles == "Trainees" & Finance == "Volunteers"  ~ 'Volunteer Trainees',
    Roles == "Trainees" & Finance == "I’m not sure"  ~ 'Unsure Trainees'
    
    
  ))
#Categorizing individuals based on the certain criteria
#PIs/CoIs that are not managers are classified as 0 in the newly created Manager_Groups column. This is used for logistic regression
data$Manager_Groups = rep(NA, nrow(data))
data[c(which(data$Roles == "PIs/CoIs" & data$Manage == "Non-manager")),"Manager_Groups"] = 0
data[c(which(data$Roles == "PIs/CoIs" & data$Manage == "Managers")),"Manager_Groups"] = 1



#Creating new dataframe in preparation for chisquare test
Race_Finance = data[c("Finance_Groups", "Race_Grouped")]
#All NAs are renamed to "Filler"
Race_Finance[is.na(Race_Finance)] <- "Filler"

#Using tidyr's group_by and summaruse commands to create a summary table for each Finance Group
Race_Finance= Race_Finance%>% 
  group_by(Finance_Groups) %>%
  summarise(
    "White" = sum(Race_Grouped=="White"),
    "POC"= sum(Race_Grouped=="POC")
    
  )

#Removing the "Filler" group
Race_Finance = Race_Finance[-c(5),]
# Taking the names from the first column and reassigning them as rownames
names = Race_Finance$Finance_Groups
Race_Finance = Race_Finance[2:ncol(Race_Finance)] %>% data.frame() %>% t()
rownames(Race_Finance)=names


#chisquare test
chisq.test(Race_Finance, simulate.p.value = T)

#Post hoc w/Bonferonni correction
bonferroni = 0.05/28
x = 1
#While loop for contrasts
while(x  < 8){
  num = (x + 1):8
  for(y in num){
    test = chisq.test(Race_Finance[c(x,y)],simulate.p.value = T)
    #Only print columns that meet threshold
    if(test$p.value < bonferroni){
      print(sprintf('Columns %s & %s', names(Race_Finance[x]),names(Race_Finance[y])))
      print(test)
      
    }
  }
  x = x + 1
}


#Reordering factors for regression
data$Race = factor(data$Race, levels = c("White", "Hispanic or Latino/a/x", "Asian or Pacific Islander","Black or African American",
                                         "Multiracial or Biracial", "Middle Eastern/North African",
                                         "Other race/ethnicity not listed here"
))

data$Roles = factor(data$Roles, levels = c("PIs/CoIs",  "RAs", "RECOVER","Trainees"), exclude = NA)
data$ABCD_Roles = factor(data$ABCD_Roles, levels = c("PIs/CoIs",  "RAs", "RECOVER Coordinators", "Project Coordinators",
                                                     "Site Coordinators", "Research Coordinators", "Site Clinicians",
                                                     "Medical Professionals", 
                                                     "Trainees"), exclude = NA)


#Manager Races      
model_2 = glm(data$Manager_Groups~data$Race_Grouped, data = data, family = "binomial")
summary(model_2)      

#Average work hours
model_3 = lm(Time~Roles, data = data)
summary(model_3)

#Turnover Group
model_4 = glm(data$Turnover_Grouped~data$Roles, data = data, family = "binomial")
summary(model_4)
        
