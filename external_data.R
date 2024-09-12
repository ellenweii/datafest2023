##### external data use
family <- get_ts(full=questions, category="Family and Children")
other_main <- get_ts(full=questions, category = "Other")
housing <- get_ts(full=questions, category = "Housing and Homelessness")
financial <- get_ts(full= questions, category = "Consumer Financial Questions")
work <- get_ts(full=questions, category = "Work, Employment and Unemployment")
individualrights <- get_ts(full = questions, category = "Individual Rights")

unemployment_rate = Quandl(code="FRED/UNRATENSA",
                           type="ts",  
                           collapse="monthly", 
                           order="asc", 
                           start_date="2019-01-01",
                           end_date="2022-01-01",
                           meta=TRUE)

housing_supply_ratio = Quandl(code="FRED/MSACSRNSA", 
                              type="ts", 
                              collapse="monthly",
                              order="asc", 
                              start_date="2017-01-01",
                              end_date="2022-01-01",
                              meta=TRUE)

covid <- read.csv("covid.csv")
covid <- covid %>% 
  select(submission_date, new_case)

head(covid$submission_date)
covid$submission_date <- as.Date(covid$submission_date, format = "%m/%d/%Y")

covid <- covid %>% 
  filter(submission_date <= "2022-07-01")
covid$ym <- floor_date(covid$submission_date, unit = "month")
tt <- covid %>% 
  group_by(ym) %>% 
  summarize(sum = sum(new_case))
template <- data.frame(ym = floor_date(seq(as.Date("2020-01-01"),
                                           as.Date("2022-07-01"), by = "month"), 
                                       unit = "month"))

df <- full_join(tt, template, by = "ym") %>% 
  arrange(ym) %>% 
  mutate(sum = case_when(is.na(sum)~0, T~sum))

df <- ts(df, start = c(2020, 01), end = c(2022, 07), 
         frequency = 12)
covid_ts <- df[, 2]
dygraph(covid_ts, main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month")


dygraph(cbind(unemployment_rate*400, housing_supply_ratio*400,
              questions_submitted, family, other_main, 
              housing, financial, 
              work,individualrights), 
        main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month") %>%
  dyLegend(width = 400)

###################### unemployment rate, covid #################
dygraph(cbind(unemployment_rate*400, covid_ts*0.0003,
              questions_submitted, financial, work, 
              other_main, family), 
        main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month") %>%
  dyLegend(width = 500) %>% 
  dySeries(drawPoints = TRUE, strokeWidth = 3, 
           color = "firebrick") %>% 
  dySeries(drawPoints = TRUE, color = "darkorange", strokeWidth = 3)


library(ggfortify)
