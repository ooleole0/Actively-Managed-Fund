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
                    "ret_minus_market",
                    "pvol")
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

# create beta median
beta_breakpoints <- data_trail %>%
  group_by(date) %>%
  summarise(
    beta_q50 = quantile(beta_avg, 0.5, na.rm = TRUE),
  ) %>%
  ungroup()

# flag beta type
data_merged <- data_trail %>%
  inner_join(beta_breakpoints, by = "date") %>%
  mutate(
    beta_type = case_when(
      beta_avg < beta_q50 ~ "L",
      beta_avg >= beta_q50 ~ "H"
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
  summarise(vwret = mean(ret)) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = date,
    values_from = vwret,
    names_from = c("beta_type"),
    names_sep = ""
  ) %>%
  mutate(year = year(date), month = month(date))

# read market data
index_data <- read_csv("twindex.csv")
colnames(index_data) <- c("date", "close", "open", "hi", "lo", "vol", "Mk_ret")
index_data <- index_data %>%
  mutate(
    date = ymd(date),
    year = year(date),
    month = month(date),
    Mk_ret = as.numeric(sub("%", "", Mk_ret)) / 100
  ) %>%
  mutate(
    date = ceiling_date(date, "month") - days(1)
  ) %>%
  arrange(date) %>%
  select(date, year, month, Mk_ret)

portf_beta <- portf_beta %>%
  inner_join(index_data, by = c("year", "month")) %>%
  select(-date.y)

portf_cum <- portf_beta %>%
  mutate(
    date = date.x,
    H_cum = cumprod(1 + H) - 1,
    L_cum = cumprod(1 + L) - 1,
    Mk_ret_cum = cumprod(1 + Mk_ret) -1
  ) %>%
  select(date, H_cum, L_cum, Mk_ret_cum)

portf_plot <- portf_cum %>%
  pivot_longer(
    cols = H_cum:Mk_ret_cum,
    names_to = "beta_type",
    values_to = "ret"
  )


portf_plot %>%
  ggplot(aes(x = date, y = ret, color = beta_type)) +
  geom_line() +
  labs(y = "Cumulative Returns") +
  scale_y_continuous(labels = scales::percent)