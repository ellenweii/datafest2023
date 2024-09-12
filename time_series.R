### time series
questions <- read.csv("questions.csv")
q_sub <- sample_n(questions, 2000)

########## Questions submitted

library(dygraphs)

questions$ym <- floor_date(as.Date(questions$AskedOnUtc), unit = "month")
tt <- questions %>% 
  group_by(ym) %>% 
  summarize(count = n())
template <- data.frame(ym = floor_date(seq(as.Date("2012-10-01"),
                                           as.Date("2022-01-01"), by = "month"), 
                                       unit = "month"))

df <- full_join(tt, template, by = "ym") %>% 
  arrange(ym) %>% 
  mutate(count = case_when(is.na(count)~0, T~count))

df <- ts(df, start = c(2012, 10), end = c(2022, 1), 
         frequency = 12)
questions_submitted <- df[, 2]
dygraph(questions_submitted, main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month")

## multiplicative decomposition
plot(decompose(q_submitted, type="multiplicative"))

########## Questions submitted by category
barplot(sort(table(questions$Category), decreasing = TRUE),
        las=2, cex.names = 0.5)

(table(questions$Category))
sort(table(questions$Category), decreasing = TRUE)[1:5]
dim(questions)
### top 3: Family and Children , Other, Housing and Homelessness

####### Family and Children 861, includes Family/Divorce/Custody
## lots of overlapping subcategories!!
family <- questions %>% 
  filter(Category == "Family and Children")
tt <- family %>% 
  group_by(ym) %>% 
  summarize(count = n())
template <- data.frame(ym = floor_date(seq(as.Date("2012-12-01"),
                                           as.Date("2022-01-01"), by = "month"), 
                                       unit = "month"))

df <- full_join(tt, template, by = "ym") %>% 
  arrange(ym) %>% 
  mutate(count = case_when(is.na(count)~0, T~count))

df <- ts(df, start = c(2012, 12), end = c(2022, 1), 
         frequency = 12)
ts_family <- df[, 2]
dygraph(cbind(questions_submitted, ts_family), main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month")

###### Housing and Homelessness 341
housing <- questions %>% 
  filter(Category == "Housing and Homelessness")
tt <- housing %>% 
  group_by(ym) %>% 
  summarize(count = n())
template <- data.frame(ym = floor_date(seq(as.Date("2012-12-01"),
                                           as.Date("2022-01-01"), by = "month"), 
                                       unit = "month"))

df <- full_join(tt, template, by = "ym") %>% 
  arrange(ym) %>% 
  mutate(count = case_when(is.na(count)~0, T~count))

df <- ts(df, start = c(2012, 12), end = c(2022, 1), 
         frequency = 12)
ts_housing <- df[, 2]
dygraph(cbind(questions_submitted, ts_housing, ts_family), main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month")



########### Other
other_main <- questions %>% 
  filter(Category == "Other")
tt <- other_main %>% 
  group_by(ym) %>% 
  summarize(count = n())
template <- data.frame(ym = floor_date(seq(as.Date("2012-12-01"),
                                           as.Date("2022-01-01"), by = "month"), 
                                       unit = "month"))

df <- full_join(tt, template, by = "ym") %>% 
  arrange(ym) %>% 
  mutate(count = case_when(is.na(count)~0, T~count))

df <- ts(df, start = c(2012, 12), end = c(2022, 1), 
         frequency = 12)
ts_other_main <- df[, 2]
dygraph(cbind(questions_submitted, ts_housing, ts_family, ts_other_main), 
        main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month")


######### Consumer Financial Questions
financial <- questions %>% 
  filter(Category == "Consumer Financial Questions")
tt <- financial %>% 
  group_by(ym) %>% 
  summarize(count = n())
template <- data.frame(ym = floor_date(seq(as.Date("2012-12-01"),
                                           as.Date("2022-01-01"), by = "month"), 
                                       unit = "month"))

df <- full_join(tt, template, by = "ym") %>% 
  arrange(ym) %>% 
  mutate(count = case_when(is.na(count)~0, T~count))

df <- ts(df, start = c(2012, 12), end = c(2022, 1), 
         frequency = 12)
ts_financial <- df[, 2]
dygraph(cbind(questions_submitted, ts_housing, ts_family, 
              ts_other_main, ts_financial), 
        main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month")

######### subcategories in family
unique(family$Subcategory)
barplot(sort(table(family$Subcategory), decreasing = TRUE),
        las=2, cex.names = 0.5)
sort(table(family$Subcategory), decreasing = TRUE)


########## Subcategory Family/Divorce/Custody 327
custody <- q_sub %>% 
  filter(Subcategory == "Family/Divorce/Custody")
tt <- custody %>% 
  group_by(ym) %>% 
  summarize(count = n())
template <- data.frame(ym = floor_date(seq(as.Date("2012-12-01"),
                                           as.Date("2022-01-01"), by = "month"), 
                                       unit = "month"))

df <- full_join(tt, template, by = "ym") %>% 
  arrange(ym) %>% 
  mutate(count = case_when(is.na(count)~0, T~count))

df <- ts(df, start = c(2012, 12), end = c(2022, 1), 
         frequency = 12)
ts_custody <- df[, 2]
dygraph(cbind(q_submitted, ts_family, ts_custody), main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month")

###### details about january 2020
jan2020 <- questions %>% 
  filter(ym == "2020-01-01")
sort(table(jan2020$Category), decreasing = TRUE)
sort(table(questions$Subcategory), decreasing = TRUE)[1:10]
sort(table(questions$Category), decreasing = TRUE)[1:10]

############## categories selected from subcategories ##############
temp <- questions %>% 
  filter(Subcategory == "Civil/Constitutional Rights")
unique(temp$Category)

# Family and Children; Other; Housing and Homelessness; 
# Consumer Financial Questions; Work, Employment and Unemployment;
# Individual Rights

get_ts <- function(full, category=NULL, 
                   start_str = "2012-10-01", end_str ="2022-01-01",
                   start_year = 2012, start_month = 10, 
                   end_year = 2022, end_month = 1) {
  
  if (is.null(category)){
    temp <- full
  }
  
  ## returns ts based on category
  else {
    temp <- full %>% 
      filter(Category == category)
  }

  tt <- temp %>% 
    filter(ym >= start_str, ym <= end_str) %>% 
    group_by(ym) %>% 
    summarize(count = n())
  template <- data.frame(ym = floor_date(seq(as.Date(start_str),
                                             as.Date(end_str), by = "month"), 
                                         unit = "month"))
  
  df <- full_join(tt, template, by = "ym") %>% 
    arrange(ym) %>% 
    mutate(count = case_when(is.na(count)~0, T~count))
  
  df <- ts(df, start = c(start_year, start_month), 
           end = c(end_year, end_month), 
           frequency = 12)
  ts_object <- df[, 2]
  return(ts_object)
}

family <- get_ts(full=questions, category="Family and Children")
other_main <- get_ts(full=questions, category = "Other")
housing <- get_ts(full=questions, category = "Housing and Homelessness")
financial <- get_ts(full= questions, category = "Consumer Financial Questions")
work <- get_ts(full=questions, category = "Work, Employment and Unemployment")
individualrights <- get_ts(full = questions, category = "Individual Rights")


dygraph(cbind(questions_submitted, family, other_main, 
              housing, financial, 
              work,individualrights), 
        main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month") %>%
  dyLegend(width = 400)

################## Aug 2016 to Jan 2022 #########
df_window <- function(df, start, end){
  temp <- df %>% 
    filter(ym >= start & ym <= end)
  
  return(temp)
}

#aug16jan22 <- df_window(questions, start="2016-08-01", end = "2022-01-01")


sort(table(aug16jan22$Subcategory), decreasing = TRUE)[1:10]
sort(table(aug16jan22$Category), decreasing = TRUE)[1:10]

aug16jan22 <- get_ts(full=questions, start_str = "2016-08-01", 
                     start_year = 2016, start_month = 10)
family <- get_ts(full=questions, category="Family and Children", 
                 start_str = "2016-08-01", 
                 start_year = 2016, start_month = 10)

dygraph(cbind(aug16jan22, family), 
        main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month") %>%
  dyLegend(width = 400)

##################### Feb 2020 to Jan 2022 ###
feb20jan22 <- df_window(df = questions, 
                        start = "2020-02-01", 
                        end = "2022-01-01")
sort(table(feb20jan22$Subcategory), decreasing = TRUE)[1:10]
sort(table(feb20jan22$Category), decreasing = TRUE)[1:10]




############## decomposition #########
q_decomp <- decompose(questions_submitted, "additive")
plot(q_decomp)

family_decomp <- decompose(family, "additive")
plot(family_decomp)

head(family_decomp$seasonal)

boxplot(family ~ cycle(family), 
        xlab = "Month",
        ylab = "Questions : Family", 
        main = "Seasonal Questions : Family")

boxplot(questions_submitted ~ cycle(questions_submitted), 
        xlab = "Month",
        ylab = "Questions", 
        main = "Seasonal Questions")

plot(cbind(q_decomp$trend, family_decomp$trend))

random.term <- q_decomp$random
random.term <- random.term[!is.na(random.term)]
acf(random.term) #2
pacf(random.term) #1

arima.model = arima(questions_submitted,
                    order = c(1,0,2),
                    seasonal = list(order = c(0,1,0), 12))
acf(arima.model$residuals, lag = 100)
forecast <- predict(arima.model, 24)
forecast.value <- ts((forecast$pred), start = c(2022, 1), freq = 12)
ts.plot(cbind(questions_submitted, forecast.value),
        lty = c(1,3),
        lwd = 2,
        col = c("black", "red"), 
        main = "Total Questions with 2-year predictions")
abline(v = 2022, lwd = 15,
       col = adjustcolor("grey", alpha = 0.5))

library(ggplot2)
p <- as.data.frame(cbind(questions_submitted, forecast.value))

ggplot(p,
       aes(x =ym, y = count))
#dygraph(cbind(questions_submitted, forecast.value))


########## unemployment rate
library(Quandl)
Quandl.api_key("L-ywzPEgfS1cqHss39zN")
unemployment_rate = Quandl(code="FRED/UNRATENSA",
                           type="ts",  
                           collapse="monthly", 
                           order="asc", 
                           start_date="2012-10-01",
                           end_date="2022-01-01",
                           meta=TRUE)
head(unemployment_rate*1000)
dygraph(cbind(unemployment_rate*400,
              questions_submitted, family, other_main, 
              housing, financial, 
              work,individualrights), 
        main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month") %>%
  dyLegend(width = 400)

tail(questions_submitted)

dygraph(cbind(unemployment_rate*400,
              questions_submitted, financial, 
              work), 
        main = "Questions submitted by Month", 
        ylab = "Questions", xlab = "Month") %>%
  dyLegend(width = 400)



