#compare attorney records, client records, external data
attorney <- read.csv("attorneys.csv")
clients <- read.csv("clients.csv")

attorney_new <- get_ts(attorney, start_str = "2011-04-01", 
       start_year = 2011, start_month = 4)

clients_new <- get_ts(clients, start_str = "2012-11-01",
                      start_year = 2012, start_month = 11)

clients$CreatedUtc <- as.Date(clients$CreatedUtc)
clients$ym <- floor_date(as.Date(clients$CreatedUtc), 
                         unit = "month")
tt <- clients %>% 
  group_by(ym) %>% 
  summarize(count = n())
template <- data.frame(ym = floor_date(seq(as.Date("2012-11-01"),
                                           as.Date("2022-01-01"), by = "month"), 
                                       unit = "month"))

df <- full_join(tt, template, by = "ym") %>% 
  arrange(ym) %>% 
  mutate(count = case_when(is.na(count)~0, T~count))

df <- ts(df, start = c(2012, 11), end = c(2022, 1), 
         frequency = 12)
clients_new <- df[, 2]


dygraph(cbind(attorney_new, clients_new, questions_submitted), 
        main = "New Attorneys vs. New Clients", 
        ylab = "Number of Users", xlab = "Month")
