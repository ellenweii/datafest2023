clusters <- read.csv("clusters.csv")
dim(clusters)
head(clusters)
clusters <- clusters %>% 
  select(Category, Subcategory, CreatedUtc, cluster)


clusters$CreatedUtc <- as.Date(clusters$CreatedUtc)
c1 <- clusters %>% 
  filter(cluster == 1)

c1$ym <- floor_date(as.Date(c1$CreatedUtc), 
                         unit = "month")
tt <- c1 %>% 
  group_by(ym) %>% 
  summarize(count = n())
template <- data.frame(ym = floor_date(seq(as.Date("2018-04-01"),
                                           as.Date("2022-01-01"), by = "month"), 
                                       unit = "month"))

df <- full_join(tt, template, by = "ym") %>% 
  arrange(ym) %>% 
  mutate(count = case_when(is.na(count)~0, T~count))

df <- ts(df, start = c(2018, 4), end = c(2022, 1), 
         frequency = 12)
c1_ts <- df[, 2]


dygraph(c1_ts, 
        main = "Clusters", 
        ylab = "Questions", xlab = "Month")



get_cluster_ts <- function(full, cluster, 
                   start_str = "2012-10-01", end_str ="2022-01-01",
                   start_year = 2012, start_month = 10, 
                   end_year = 2022, end_month = 1) {
  
  ## returns ts based on cluster
  temp <- full %>% 
    filter(cluster == cluster)
  
  tt <- temp %>% 
    group_by(ym) %>% 
    summarize(count = n())
  
  start_str <-
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




