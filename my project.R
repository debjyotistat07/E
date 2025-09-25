install.packages(c("readxl","dplyr","lubridate","ggplot2","zoo","TTR","janitor","broom","sandwich","lmtest"))
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)     # rollapply
library(TTR)     # runSD etc (optional)
library(janitor) # clean_names
library(broom)   # tidy() results
library(sandwich); library(lmtest) # robust SE
path="D:\\project folder\\NIFTY 50-01-01-2013-to-31-12-2019.xlsx"
df = readxl::read_excel(path, sheet = 1)
glimpse(df)
head(df)
names(df)
df <- janitor::clean_names(df)
names(df)
names(df) <- names(df) %>%
  trimws() %>%
  gsub(" ", "_", .) %>%
  gsub("[^A-Za-z0-9_]", "", .)
df <- df %>%
  mutate(date = as.Date(date)) 
df = df %>% arrange(date)
str(df)
df
df <- df %>%
  arrange(date) %>%
  mutate(
    close = as.numeric(close),  # ensure numeric
    ret = (close / lag(close)) - 1,        # arithmetic return
    log_ret = log(close) - log(lag(close)) # log return (useful in stats)
  )
# quick summary
summary(df$ret)
# choose window length (30 trading days ≈ 1.5 months)
w <- 30

df <- df %>%
  mutate(
    vol_30 = zoo::rollapply(ret, width = w, FUN = sd, fill = NA, align = "right", na.rm = TRUE),
    # 30-day moving average of turnover (turnover column name might differ after clean_names)
    turnover = as.numeric(get("turnover_rs_cr")), # replace with your exact turnover column if different
    turnover_30ma = zoo::rollapply(turnover, width = w, FUN = mean, fill = NA, align = "right", na.rm = TRUE),
    turnover_ratio = turnover / turnover_30ma
  )

# define election windows and result days
e2014_start <- as.Date("2014-04-07"); e2014_end <- as.Date("2014-05-12"); e2014_result <- as.Date("2014-05-16")
e2019_start <- as.Date("2019-04-11"); e2019_end <- as.Date("2019-05-19"); e2019_result <- as.Date("2019-05-23")

df <- df %>%
  mutate(
    election_period = if_else((date >= e2014_start & date <= e2014_end) |
                                (date >= e2019_start & date <= e2019_end), 1, 0),
    result_day = if_else(date %in% c(e2014_result, e2019_result), 1, 0)
  )
table(df$election_period, useNA="ifany")
table(df$result_day, useNA="ifany")
# Define election windows and result days
e2014_start  <- as.Date("2014-04-07")
e2014_end    <- as.Date("2014-05-12")
e2014_result <- as.Date("2014-05-16")

e2019_start  <- as.Date("2019-04-11")
e2019_end    <- as.Date("2019-05-19")
e2019_result <- as.Date("2019-05-23")
table(df$election_period)
table(df$result_day)

compute_event <- function(df, event_date, est_start = -250, est_end = -31, win_start = -15, win_end = 15) {
  # ensure date class
  event_date <- as.Date(event_date)
  # estimation period
  est0 <- df %>%
    filter(date >= (event_date + est_start) & date <= (event_date + est_end))
  mu_hat <- mean(est0$ret, na.rm = TRUE)  # constant-mean expected return
  
  # event window
  ev <- df %>%
    filter(date >= (event_date + win_start) & date <= (event_date + win_end)) %>%
    mutate(
      event_date = event_date,
      rel_day = as.integer(date - event_date),
      expected = mu_hat,
      ar = ret - expected
    ) %>%
    arrange(rel_day) %>%
    mutate(car = cumsum(replace_na(ar, 0)))
  return(ev)
}
mutate(car = cumsum(ifelse(is.na(ar), 0, ar))
       
       
       
       compute_event <- function(df, event_date, est_start = -250, est_end = -31, win_start = -15, win_end = 15) {
         event_date <- as.Date(event_date)
         
         est0 <- df %>%
           filter(date >= (event_date + est_start) & date <= (event_date + est_end))
         mu_hat <- mean(est0$ret, na.rm = TRUE)
         
         ev <- df %>%
           filter(date >= (event_date + win_start) & date <= (event_date + win_end)) %>%
           mutate(
             event_date = event_date,
             rel_day = as.integer(date - event_date),
             expected = mu_hat,
             ar = ret - expected
           ) %>%
           arrange(rel_day) %>%
           mutate(car = cumsum(ifelse(is.na(ar), 0, ar)))   # base R alternative
         
         return(ev)
       }
       ev2014 <- compute_event(df, e2014_result)
       ev2019 <- compute_event(df, e2019_result)
       
       ev_all <- bind_rows(ev2014, ev2019)
       head(ev_all)
       # CAR plot (one facet per event)
       ggplot(ev_all, aes(x = rel_day, y = car, group = as.factor(event_date))) +
         geom_line() +
         geom_vline(xintercept = 0, linetype = "dashed") +
         facet_wrap(~ event_date, scales = "free_y") +
         labs(x = "Days from result", y = "Cumulative Abnormal Return (CAR)",
              title = "Event Study: CAR around election result days")
       
       # AR boxplot pre/post
       ev_all %>%
         mutate(period = if_else(rel_day < 0, "pre", if_else(rel_day > 0, "post", "day0"))) %>%
         ggplot(aes(x = period, y = ar)) +
         geom_boxplot() +
         labs(title = "Abnormal returns before/after result day")
       #i want to see the box plot of the year 2014 vs 2019 for better comparison
       
       ev2014 <- compute_event(df, e2014_result) %>% 
         mutate(year = "2014")
       
       ev2019 <- compute_event(df, e2019_result) %>% 
         mutate(year = "2019")
       
       ev_all <- bind_rows(ev2014, ev2019)
       ev_all <- ev_all %>%
         mutate(period = case_when(
           rel_day < 0 ~ "pre",
           rel_day == 0 ~ "day0",
           rel_day > 0 ~ "post"
         ))
       library(ggplot2)
       
       ggplot(ev_all, aes(x = period, y = ar, fill = year)) +
         geom_boxplot(alpha = 0.6, position = position_dodge(width = 0.8)) +
         labs(title = "Abnormal returns before/after election result day",
              y = "Abnormal Return (AR)", x = "Period") +
         theme_minimal()
       
       # WE WILL DO HERE PAIRED T TEST TO SEE IF THEY SIGNIFICENTLY DIFFER OR NOT
       
       # t-test of ARs in event window vs 0 for each event
       ev_all %>% group_by(event_date) %>%
         summarise(
           n = sum(!is.na(ar)),
           mean_ar = mean(ar, na.rm=TRUE),
           sd_ar = sd(ar, na.rm=TRUE),
           t_test_p = t.test(ar, mu = 0)$p.value
         )
       
       # Compare turnover_ratio pre vs post result (example for 2019)
       ev2019 %>%
         mutate(period = if_else(rel_day < 0, "pre", if_else(rel_day > 0, "post", "day0"))) %>%
         group_by(period) %>%
         summarise(mean_turnover_ratio = mean(turnover_ratio, na.rm=TRUE),
                   sd_turn = sd(turnover_ratio, na.rm=TRUE),
                   n = sum(!is.na(turnover_ratio)))
       # t.test or wilcox.test:
       with(ev2019 %>% filter(rel_day != 0), t.test(turnover_ratio ~ (rel_day > 0)))
       
       
       ev2014 %>%
         mutate(period = if_else(rel_day < 0, "pre", if_else(rel_day > 0, "post", "day0"))) %>%
         group_by(period) %>%
         summarise(mean_turnover_ratio = mean(turnover_ratio, na.rm=TRUE),
                   sd_turn = sd(turnover_ratio, na.rm=TRUE),
                   n = sum(!is.na(turnover_ratio)))
       # t.test or wilcox.test:
       with(ev2014 %>% filter(rel_day != 0), t.test(turnover_ratio ~ (rel_day > 0)))
       
       library(ggplot2)
       
       # Combine CAR data for both events
       car_compare <- bind_rows(
         ev2014 %>% select(rel_day, car) %>% mutate(event = "2014"),
         ev2019 %>% select(rel_day, car) %>% mutate(event = "2019")
       )
       
       ggplot(car_compare, aes(x = rel_day, y = car, color = event)) +
         geom_line(size = 1.2) +
         geom_hline(yintercept = 0, linetype = "dashed") +
         labs(
           title = "Cumulative Abnormal Returns (CAR) around Election Results",
           x = "Days relative to result day",
           y = "Cumulative Abnormal Return",
           color = "Election Year"
         ) +
         theme_minimal(base_size = 14)
       turnover_compare <- bind_rows(
         ev2014 %>% select(rel_day, turnover_ratio) %>% mutate(event = "2014"),
         ev2019 %>% select(rel_day, turnover_ratio) %>% mutate(event = "2019")
       )
       
       ggplot(turnover_compare, aes(x = rel_day, y = turnover_ratio, color = event)) +
         geom_line(size = 1.2) +
         geom_point() +
         geom_hline(yintercept = 1, linetype = "dashed") +
         labs(
           title = "Turnover Ratio around Election Results",
           x = "Days relative to result day",
           y = "Turnover Ratio",
           color = "Election Year"
         ) +
         theme_minimal(base_size = 14)
       
       stat_summary <- tibble(
         Year = c(2014, 2019),
         Mean_AR = c(0.00252, 0.00143),
         p_AR = c(0.259, 0.577),
         Mean_Turnover_Pre = c(1.01, 1.07),
         Mean_Turnover_Post = c(1.38, 1.02),
         p_Turnover = c(0.0117, 0.5717)
       )
       
       stat_summary
       
       # regression of daily return on being in an election period
       model1 <- lm(ret ~ election_period, data = df)
       summary(model1)
       
       # robust SE
       coeftest(model1, vcov = sandwich::vcovHC(model1, type = "HC1"))
       
       # regression including volatility / turnover controls (example)
       model2 <- lm(ret ~ election_period + lag(ret,1) + vol_30 + turnover_ratio, data = df)
       coeftest(model2, vcov = sandwich::vcovHC(model2, type = "HC1"))
       df <- df %>%
         mutate(
           election2014 = if_else(date >= e2014_start & date <= e2014_end, 1, 0),
           election2019 = if_else(date >= e2019_start & date <= e2019_end, 1, 0)
         )
       
       model_both <- lm(ret ~ election2014 + election2019 + lag(ret,1) + vol_30 + turnover_ratio, data = df)
       coeftest(model_both, vcov = sandwich::vcovHC(model_both, type = "HC1"))
       install.packages("car")
library(car)
       linearHypothesis(model_both, "election2014 = election2019", vcov = sandwich::vcovHC(model_both, type = "HC1"))
       
       