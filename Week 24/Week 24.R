#-----African-American Achievements--------
library(tidyverse)
library(dplyr)
library(stringr)
tuesdata <- tidytuesdayR::tt_load(2020, week = 24)
afam_firsts<-tuesdata$firsts
afam_science<-tuesdata$science

#Cleaning data 
afam_first_c<-afam_firsts%>%
  mutate(female=case_when(
          str_detect(tolower(gender), "female")~1, 
          TRUE~0),
        person = str_remove_all(person, "\\(.*"),
        person = str_remove_all(person, "\\(.*\\)"), 
        person = str_remove_all(person, "\\[.*\\]"),
        accomplishment = str_remove_all(accomplishment, "\\(.*"),
        accomplishment = str_remove_all(accomplishment, "\\(.*\\)"), 
        accomplishment = str_remove_all(accomplishment, "\\[.*\\]"),
        row_num=row_number(),
        p_count=str_count(person, "\\.")
        )
x<-as.factor(afam_first_c$person)
afam_first_c$person<-gsub("\\.$", '', x)
afam_first_c$person<-str_trim(afam_first_c$person, "right")

#function to uppercase the first letter of a string
capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}
afam_first_c$person<-capFirst(afam_first_c$person)



#creating a flag for what should automatically get a period at the end    
afam_first_c$p_flag<-ifelse(str_detect(tolower(afam_first_c$person),"jr|sr")==TRUE|
   str_count(afam_first_c$person,"\\s")>10&
   str_count(afam_first_c$person, "\\,")>=3, 
    1, 0)   

#returning periods to the end of descriptions
afam_first_c$person_c<-ifelse(afam_first_c$p_flag==1, paste(afam_first_c$person, ".", sep=""), afam_first_c$person)



write.csv(afam_first_c, "afam_firsts_cleaned.csv")


  
