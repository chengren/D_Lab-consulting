library(readxl)
setwd('C:/Users/cheng/Downloads')
sheet_list=excel_sheets('destig3_data.xlsx')

for (i in 1:length(sheet_list))
  assign(sheet_list[i],read_excel('destig3_data.xlsx',sheet=i))
# I would encourage you to use Clean_Data rather than the CV sheets
aggregate(Clean_Data$`Consider the applicants' propensity for VIOLENCE. Which applicant would you consider MORE VIOLENT? - Christopher Williams:Jonathan Smith`, 
          by = list(Clean_Data$`In what country are you taking this survey?`),summary)
# combine two colums
Clean_Data$group <- paste(Clean_Data$`FL_17 - Block Randomizer - Display Order`,Clean_Data$`FL_22 - Block Randomizer - Display Order`,sep="")
Clean_Data$group <- gsub("NA","",Clean_Data$group)# replace NA to  empty since when combine columns, CWCV1+NA will become CWCV1NA
aggregate(Clean_Data$`Consider the applicants' propensity for VIOLENCE. Which applicant would you consider MORE VIOLENT? - Christopher Williams:Jonathan Smith`, 
          by = list(Clean_Data$group),summary)
