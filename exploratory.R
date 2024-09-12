### exploratory data analysis

library(dplyr)
#questionposts <- read.csv("questionposts.csv")
qp_sub <- sample_n(questionposts, 2000)
questionposts[42,]
questionposts[43,]
questionposts[44,]

temp <- questionposts[15,]
temp$PostText
temp2 <- data.frame(temp)
typeof(temp2)
names(temp2)
temp2$PostText

############### time ################
### from Questions
###### AskedOnUtc : when client submitted question
###### TakenOnUtc : when attorney selected the client's question

## yday, wday(label=TRUE, abbr = FALSE)
library(tidyverse)
library(lubridate)
questions <- read.csv("questions.csv")
head(questions$AskedOnUtc)
typeof(questions$AskedOnUtc)
temp2 <- questions[1, 'AskedOnUtc']
temp3 <- ymd_hms(temp2)
second(temp3)

## week day
questions %>% 
  mutate(wday = wday(ymd_hms(questions$AskedOnUtc), label = TRUE)) %>% 
  ggplot(aes(x = wday)) +
  geom_bar()

## month
questions %>% 
  mutate(month = month(ymd_hms(questions$AskedOnUtc), label = TRUE)) %>% 
  ggplot(aes(x = month)) +
  geom_bar()

## hour of day
questions %>% 
  mutate(hour = hour(ymd_hms(questions$AskedOnUtc))) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()

unique(questions$StateAbbr)
questions %>% 
  filter(StateAbbr == "ID") %>%
  mutate(hour = hour(ymd_hms(questions$AskedOnUtc))) %>% 
  ggplot(aes(x = hour)) +
  geom_bar()
  
questions_state_ID <- questions %>% 
  filter(StateAbbr == "ID")

names(questions_state_ID)
names(questions)
#sum(questions$ClosedByAttorneyUno == NULL)
sum(is.null(questions_state_ID$ClosedByAttorneyUno))
t <- as.data.frame(questions_state_ID)
is.null(t$ClosedByAttorneyUno)
