library(tidyverse)
library(lubridate)
library(zoo)

# read project data
data <- read_csv("TEJ.csv")
colnames(data) <- c("secu_code", 
                    "list_type", 
                    "UNI", 
                    "indust", 
                    "date",
                    "mktcap",
                    "beta",
                    "ret",
                    "ret_minus_market")
data <- data %>%
  mutate(
    date = as.Date(as.character(date), "%Y%m%d"),
    year = year(date),
    month = months(date),
    ret = ret / 100,
    ret_minus_market = ret_minus_market / 100
  ) %>%
  group_by(year, month) %>%
  filter(
    date == max(date)
  ) %>%
  ungroup() %>%
  select(
    date,
    secu_code,
    UNI,
    list_type,
    indust,
    mktcap,
    ret,
    ret_minus_market,
    beta
  ) %>%
  drop_na()

# calculate roll beta mean
data_trail <- data %>%
  mutate(
    beta_avg = rollmean(beta, k = 12, na.pad=TRUE, align="right")
  ) %>%
  drop_na() %>%
  select(-beta)

# create beta breakpoints
beta_breakpoints <- data_trail %>%
  group_by(date) %>%
  summarise(
    beta_q20 = quantile(beta_avg, 0.2, na.rm = TRUE),
    beta_q40 = quantile(beta_avg, 0.4, na.rm = TRUE),
    beta_q60 = quantile(beta_avg, 0.6, na.rm = TRUE),
    beta_q80 = quantile(beta_avg, 0.8, na.rm = TRUE)
  ) %>%
  ungroup()

# flag beta type
data_merged <- data_trail %>%
  inner_join(beta_breakpoints, by = "date") %>%
  mutate(
    beta_type = case_when(
      beta_avg < beta_q20 ~ "beta_1",
      beta_avg >= beta_q20 & beta_avg < beta_q40 ~ "beta_2",
      beta_avg >= beta_q40 & beta_avg < beta_q60 ~ "beta_3",
      beta_avg >= beta_q60 & beta_avg < beta_q80 ~ "beta_4",
      beta_avg > beta_q80 ~ "beta_5"
    )
  )

data_weight <- data_merged %>%
  select(date, 
         secu_code, 
         mktcap, 
         ret, 
         ret_minus_market, 
         beta_avg, 
         beta_type) %>%
  group_by(secu_code) %>%
  mutate(weight = lag(mktcap)) %>%
  ungroup() %>%
  drop_na

portf_beta <- data_weight %>%
  group_by(date, beta_type) %>%
  summarise(vwret = weighted.mean(ret, w = weight)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = date,
    values_from = vwret,
    names_from = c("beta_type"),
    names_sep = ""
  )