#This is a real dataframe that I had to analyze and generate lollipop plots for.
#Names of certain variables have been converted to Group_W, Group_M, for privacy reasons
pacman::p_load(dplyr, ggplot2, tidyr,jtools)
data = readxl::read_xlsx("~/data.xlsx")
data = data.frame(ID = 1:nrow(data), data)

#For loop to look through each element in the vector and transform all those elements into binary information for logistic regression.
Turnover= c()
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
  Turnover= c(Turnover, convert)
}

data$Turnover= Turnover

#Another for loop to look through each element in the vector and reduce that information into three factors.

Race= c()
for(i in data$Race){
  if(isTRUE(i=="White")==T){
    convert = i
  }
  else if(i %in% c("Hispanic or Latino/a/x")){
    convert = i
  }
  else if(!(i %in% c("White", "Hispanic or Latino/a/x"))){
    convert = "Non-Hispanic POC"
  }
  else{
    convert = NA
  }
  Race = c(Race, convert)
}

data$Race =Race

#Another for loop to look through each element in the vector and reduce that information into two factors.
Race= c()
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
  Race = c(Race, convert)
}

data$Race =Race


#Here I am creating a new data column named finance and replicating "NA" to make this column the same length as the other columns in the dataframe
data$Finance_Groups = rep(NA, nrow(data))
# Renaming specific variables in certain columns. The "which()" function searches for ceartain rows that meet a certain condition.
#When those rows are found, then a new variable name is added to the new column for that specific row
data[c(which(data$Roles == "Group_P" & data$Finance == "C.")),"Finance_Groups"] = "C. Group_P"
data[c(which(data$Roles == "Group_P" & data$Finance == "V.")),"Finance_Groups"] = "V. Group_P"

data[c(which(data$Roles == "Group_R" & data$Finance == "C.")),"Finance_Groups"] = "C. Group_R"
data[c(which(data$Roles == "Group_R" & data$Finance == "V.")),"Finance_Groups"] = "V. Group_R"

data[c(which(data$Roles == "Group_R2" & data$Finance == "C.")),"Finance_Groups"] = "C. Group_R2"
data[c(which(data$Roles == "Group_R2" & data$Finance == "V.")),"Finance_Groups"] = "V."

data[c(which(data$Roles == "Group_T" & data$Finance == "C.")),"Finance_Groups"] = "C. Group_T"
data[c(which(data$Roles == "Group_T" & data$Finance == "V.")),"Finance_Groups"] = "V. Group_T"
data[c(which(data$Roles == "Group_T" & data$Finance == "I’m not sure")),"Finance_Groups"] = "Unsure Group_T"

data[c(which(data$Roles == "Group_P" & data$Finance == "C.")),"Finance_Groups"] = "C. Group_P"
data[c(which(data$Roles == "Group_P" & data$Finance == "V.")),"Finance_Groups"] = "V. PI/CoIs"

data$Manager_Groups = rep(NA, nrow(data))
data[c(which(data$Roles == "Group_P" & data$Manage == "M.")),"Manager_Groups"] = 1
data[c(which(data$Roles == "Group_P" & data$Manage == "Nm.")),"Manager_Groups"] = 0



Race_Finance = data[c("Finance_Groups", "Race")]
Race_Finance[is.na(Race_Finance)] <- "Filler"

Race_Finance= Race_Finance%>% 
  group_by(Finance_Groups) %>%
  summarise(
    "White" = sum(Race=="White"),
    "POC"= sum(Race=="POC")
    
  )


Race_Finance = Race_Finance[-c(5),]
names = Race_Finance$Finance_Groups
Race_Finance = Race_Finance[2:ncol(Race_Finance)]
Race_Finance = data.frame(Race_Finance )
rownames(Race_Finance) =names
Race_Finance = t(Race_Finance)
Race_Finance = data.frame(Race_Finance)


data$Race = factor(Race, levels = c("White", "Hispanic or Latino/a/x", "Non-Hispanic POC"))

data$Roles = factor(data$Roles, levels = c("Group_P",  "Group_R", "Group_R2","Group_T"), exclude = NA)
data$Roles2 = factor(data$Roles2, levels = c("Group_P",  "Group_R", "Group_R2", "Group_P",
                                                "Group_S", "Group_R3", "Group_S2",
                                                "Group_M", 
                                                "Group_T"), exclude = NA)


#
chisq.test(Race_Finance)

#Post hoc

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


data$Race = factor(data$Race, levels = c(
  "White",
  "Other race/ethnicity not listed here",
  "Multiracial or Biracial",
  "Middle Eastern/North African",
  "Native American or Alaskan Native",
  "Hispanic or Latino/a/x",
  "Black or African American",
  "Asian or Pacific Islander"
))

summary(data$Race)
model_2 = glm(Manager_Groups~Race, data = data,family = "binomial")
summary(model_2)      

#Average work hours
model_3 = lm(Time~Roles2, data = data)
summary(model_3)

#Turnover Group
model_4 = glm(Turnover~Roles2, data = data, family = "binomial")
summary(model_4)

data$Roles2
        
