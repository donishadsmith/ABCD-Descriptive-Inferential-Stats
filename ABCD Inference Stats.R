#Importing excel file into R
pacman::p_load(dplyr, ggplot2, tidyr, jtools)
data = readxl::read_xlsx("~/ABCD.xlsx")
data = data[2:nrow(data),]
data = data.frame(ID = 1:nrow(data), data)

# Creating for loop to categorize continuous data into 4 categories
Time_Grouped = c()
for(i in data$Time){
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
  Time_Grouped = c(Time_Grouped ,convert)
}
data$Time_Grouped  = Time_Grouped 

# Creating for loop to recode 2015:2017 to 0 and 2021:2022 to 1 for later logistic regression
Turnover_Grouped = c()
for(time in data$Turnover){
  if(as.numeric(time) %in% c(2015:2017)){
    convert = 0
  }
  else if(as.numeric(time) %in% c(2021:2022)){
    convert = 1
  }
  else{
    convert = NA
  }
  Turnover_Grouped = c(Turnover_Grouped, convert)
}

data$Turnover_Grouped = Turnover_Grouped 

# Creating for loop to categorize race into 2 categories
Race_Grouped = c()
for(i in data$Race){
  if(isTRUE(i=="White")==T){
    convert = i
  }
  else if(!(i %in% c("White"))){
    convert = "POC"
  }
  else{
    convert = NA
  }
  Race_Grouped  = c(Race_Grouped , convert)
}

data$Race_Grouped  = factor(Race_Grouped, levels = c("White", "POC"))

#Creating a new column in dataframe named Finance_Groups
data$Finance_Groups = rep(NA, nrow(data))
#Categorizing individuals based on the certain criteria
# For instance, those that are PI/CoIs and are compensated will b classified as Compensated PIs/CoIs in the Finance_Groups column
#Used for chisquare tests
data[c(which(data$Roles == "PIs/CoIs" & data$Finance == "Compensated")),"Finance_Groups"] = "Compensated PIs/CoIs"
data[c(which(data$Roles == "PIs/CoIs" & data$Finance == "Volunteers")),"Finance_Groups"] = "Volunteer PIs/CoIs"


data[c(which(data$Roles == "RAs" & data$Finance == "Compensated")),"Finance_Groups"] = "Compensated RAs"
data[c(which(data$Roles == "RAs" & data$Finance == "Volunteers")),"Finance_Groups"] = "Volunteer RAs"

data[c(which(data$Roles == "RECOVER" & data$Finance == "Compensated")),"Finance_Groups"] = "Compensated RECOVER"
data[c(which(data$Roles == "RECOVER" & data$Finance == "Volunteers")),"Finance_Groups"] = "Volunteer RECOVER"

data[c(which(data$Roles == "Trainees" & data$Finance == "Compensated")),"Finance_Groups"] = "Compensated Trainees"
data[c(which(data$Roles == "Trainees" & data$Finance == "Volunteers")),"Finance_Groups"] = "Volunteer Trainees"
data[c(which(data$Roles == "Trainees" & data$Finance == "I’m not sure")),"Finance_Groups"] = "Unsure Trainees"

data[c(which(data$Roles == "PIs/CoIs" & data$Finance == "Compensated")),"Finance_Groups"] = "Compensated PIs/CoIs"
data[c(which(data$Roles == "PIs/CoIs" & data$Finance == "Volunteers")),"Finance_Groups"] = "Volunteer PIs/CoIs"

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
Race_Finance = Race_Finance[2:ncol(Race_Finance)]
Race_Finance = data.frame(Race_Finance )
rownames(Race_Finance)=names
#Transpose dataframe
Race_Finance = t(Race_Finance)



#chisquare test
chisq.test(Race_Finance, simulate.p.value = T)

#Post hoc w/Bonferonni correction
bonferroni = 0.05/28

chisq.test(Race_Finance,simulate.p.value = T)

chisq.test(Race_Finance[c(1,2)],simulate.p.value = T)
chisq.test(Race_Finance[c(1,3)],simulate.p.value = T)
chisq.test(Race_Finance[c(1,4)],simulate.p.value = T)
chisq.test(Race_Finance[c(1,5)],simulate.p.value = T)
chisq.test(Race_Finance[c(1,6)],simulate.p.value = T)
chisq.test(Race_Finance[c(1,7)],simulate.p.value = T)
chisq.test(Race_Finance[c(1,8)],simulate.p.value = T)

chisq.test(Race_Finance[c(2,3)],simulate.p.value = T)
chisq.test(Race_Finance[c(2,4)],simulate.p.value = T)
chisq.test(Race_Finance[c(2,5)],simulate.p.value = T)
chisq.test(Race_Finance[c(2,6)],simulate.p.value = T)
chisq.test(Race_Finance[c(2,7)],simulate.p.value = T)
chisq.test(Race_Finance[c(2,8)],simulate.p.value = T)

chisq.test(Race_Finance[c(3,4)],simulate.p.value = T)
chisq.test(Race_Finance[c(3,5)],simulate.p.value = T)
chisq.test(Race_Finance[c(3,6)],simulate.p.value = T)
chisq.test(Race_Finance[c(3,7)],simulate.p.value = T)
chisq.test(Race_Finance[c(3,8)],simulate.p.value = T)

chisq.test(Race_Finance[c(4,5)],simulate.p.value = T)
chisq.test(Race_Finance[c(4,6)],simulate.p.value = T)
chisq.test(Race_Finance[c(4,7)],simulate.p.value = T)
chisq.test(Race_Finance[c(4,8)],simulate.p.value = T)

chisq.test(Race_Finance[c(5,6)],simulate.p.value = T)
chisq.test(Race_Finance[c(5,7)],simulate.p.value = T)
chisq.test(Race_Finance[c(5,8)],simulate.p.value = T)

chisq.test(Race_Finance[c(6,7)],simulate.p.value = T)
chisq.test(Race_Finance[c(6,8)],simulate.p.value = T)

chisq.test(Race_Finance[c(7,8)],simulate.p.value = T)

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
        