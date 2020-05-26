#This is where I am starting my FINAL project for CSIROs DataSchool Focus Course
#Not sure what will need to happen yet, but for now let's set it up#
#Step 1- bringing in my data#
library(tidyverse)
library(readxl)
library(lubridate)
library(gganimate)
library(RColorBrewer)


#asldata <- read_xlsx('Data/ASL_DATA_only.xlsx')
#life is grand!#

#Changed the focus of my project to learning program evaluation data. 

#There are 3 types of evaluations so far: -pre program, lesson evaluation & mid program

#To start let's bring in the data

presurveyF3 <- read_xlsx('Data/PRE-SURVEY_FOCUS-3_23MAR2020.xls.xlsx')
lsn9n10F3 <- read_xlsx('Data/FOCUS Lesson Days 9 & 10 Feedback - Intro to Data Visualisation(1-16).xlsx')
lsn11n12F3 <- read_xlsx('Data/FOCUS Lesson Days 11 & 12 Feedback - Using Ggplot 2 & Intro to RMarkdown(1-8).xlsx')
lsn13n14F3 <- read_xlsx('Data/FOCUS Lesson Days 13 & 14 Feedback - Data Management(1-14).xlsx')
midsurveyF3 <- read_xlsx('Data/FOCUS Mid-Program Participant Survey(1-25) (2).xlsx')

#for lessons, adding in a topic & lesson reference prior to join

clean9n10 <- lsn9n10F3 %>% mutate(lesson = "9/10") %>% mutate (topic = "Analysing & Gathering Insight")
clean11n12 <- lsn11n12F3 %>% mutate(lesson = "11/12") %>% mutate (topic = "Analysing & Gathering Insight")
clean13n14 <- lsn13n14F3 %>% mutate(lesson = "13/14") %>% mutate (topic = "Data Management")

#combining all of the lesson evlauations

combined <- bind_rows(clean9n10, clean11n12, clean13n14) %>% rename(start_time = 'Start time', complete_time = 'Completion time')

#Assigning topics to the pre program
#first need to tidy data

#Some participants completed the survey multiple times- will take ONLY the most recent completion

#Parse datetime for completion


#parse_date_time2(combined, ymd_hms('complete_time'))


presurveyF3 %>% sep
withdate <- presurveyF3 %>% mutate(Created = dmy_hms(Created)) %>%  arrange(desc(Created)) %>% 
  group_by(Ident) %>% filter(Created == first(Created)) %>% mutate(DEID=runif(1,1,100)) 


gathered <- gather(withdate, key = question, value = answer, -DEID, ) 
spread <- spread (gathered, key=DEID, answer) %>% group_by(question)


#UP TO HERE- NEED TO SUMMARISE!!

summary <- gathered %>% group_by(question, answer) %>% tally() %>% filter(question!='Ident') %>% 
  filter(question!='Created')%>%   filter(question!='Other - please specify')
numeric <- summary %>% mutate(answer2 = 
                     str_replace_all(answer,c("Extensive+" = "3. Extensive", "Extremely+"= "4. Extremely",
                                              "Moderately+"= "3. Moderately", "Not at+"= "1. Not",
                                              "Slightly+"= "2. Slightly", "Some+"= "2. Some",
                                              "Unsure+"= "0. Unsure", "Little+"= "1. Little",
                                              "Not app+"= "0. Note app"))) %>% select(-answer)


?str_replace
#spread %>% length(unique(1.04962268145755))

#deidentify


#gathered <- gather(withdate, key = question, value = answer, -Ident, ) %>% group_by(Ident) %>% mutate(grouped_id = row_number())
#spread <- spread (gathered, key=Ident, answer)

#Ideally add a topic here
#mutate(spread, topic)


#replace_scale_num <- function(scale_var){
 # recode(scale_var, "Strongly Disagree"="1", "Disagree"="2", "Slightly Disagree"="3", "Neither Agree or Disagree"="4",
 #        "Slightly Agree"="5", "Agree"="6", "Strongly Agree"="7")
 # scale_var <- as.integer(scale_var)

filtered <- numeric %>% filter(answer2%in%c("2. Slightly able", "3. Moderately able", "1. Not all able", "4. Extremely able"))

nb.cols <- 12
mycolors <- colorRampPalette(brewer.pal(8, "Spectral"))(nb.cols)

coolplot <- ggplot(filtered, aes(y=n, x=answer2, fill = answer2))+ geom_bar(position = "stack", stat = "identity")+
  transition_states(question, state_length = 9, transition_length = 4)+
  scale_fill_manual(values = mycolors)+labs(title="{closest_state}")+
  theme_bw() + theme(panel.grid.minor = element_blank(),plot.title=element_text(face="bold"),
                      strip.background = element_blank(), panel.grid.major = element_line(size = 1),
                      axis.title = element_blank(),
                      legend.position = "none")

ggplot(numeric, aes(y=n, x=answer2, fill = answer2))+ geom_bar(position = "stack", stat = "identity")+
  facet_wrap(~question)+
  scale_fill_manual(values = mycolors)

?geom_bar
?transition_states
#NOTES FOR OD
#1. Need good naming conventions for data files that can be easily read in - just numbers less titling etc
#2. Consider consistency for likert scales. some questions are 3 point, others 4. Not a huge drama but would make
#things cleaner

combined
?sample




