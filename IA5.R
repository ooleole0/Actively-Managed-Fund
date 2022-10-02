library(tidyverse)
library(lubridate)
library(zoo)

# read project data
ret_data <- read_csv("return.csv")
colnames(ret_data) <- c("secu_code", 
                        "UNI", 
                        "date",
                        "ret",
                        "mktcap")

ret_data <- ret_data %>%
  mutate(
    date = ym(date),
    year = year(date),
    month = month(date),
    ret = ret / 100
  ) %>%
  filter(
    substr(secu_code, 1, 2) != "28",
    substr(secu_code, 1, 2) != "91"
  ) %>%
  select(
    date,
    year,
    month,
    secu_code,
    UNI,
    ret,
    mktcap
  ) %>%
  group_by(year) %>%
  mutate(
    sorting_year = case_when(
      month < 4 ~ (year - 1),
      month >= 4 ~ year
    )
  ) %>%
  ungroup() %>%
  group_by(secu_code) %>%
  mutate(weight = lag(mktcap)) %>%
  ungroup() %>%
  drop_na()

ia_data <- read_csv("IA.csv")
colnames(ia_data) <- c("secu_code",
                            "UNI",
                            "date",
                            "inv",
                            "fix_asset",
                            "asset")

ia_data <- ia_data %>%
  mutate(
    date = ym(date),
    year = year(date),
    month = month(date),
    sorting_year = year + 1,
  ) %>%
  group_by(year) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(-month) %>%
  group_by(secu_code) %>%
  mutate(
    IA = ((inv - lag(inv)) + (fix_asset - lag(fix_asset)) / asset)
    )

ia_breakpoints <- ia_data %>%
  group_by(sorting_year) %>%
  summarise(
    IA_q20 = quantile(IA, 0.2, na.rm = TRUE),
    IA_q40 = quantile(IA, 0.4, na.rm = TRUE),
    IA_q60 = quantile(IA, 0.6, na.rm = TRUE),
    IA_q80 = quantile(IA, 0.8, na.rm = TRUE)
  )
ia_data <- ia_data %>%
  left_join(ia_breakpoints, by = "sorting_year")

size_breakpoints <- ret_data %>%
  group_by(sorting_year) %>%
  summarise(
    size_med = quantile(mktcap, 0.5, na.rm = TRUE)
  )

ret_data <- ret_data %>%
  left_join(size_breakpoints, by = "sorting_year")

type_data <- ia_data %>%
  mutate(
    ia_type = case_when(
      IA <= IA_q20 ~ "ia_1",
      IA > IA_q20 & IA <= IA_q40 ~ "ia_2",
      IA > IA_q40 & IA <= IA_q60 ~ "ia_3",
      IA > IA_q60 & IA <= IA_q80 ~ "ia_4",
      IA > IA_q80 ~ "ia_5"
    )
  ) %>%
  select(sorting_year,secu_code, UNI, ia_type) %>%
  drop_na()


ret_type_data <- ret_data %>%
  inner_join(type_data, by = c("sorting_year", "UNI")) %>%
  select(-secu_code.y) %>%
  mutate(
    size_type = case_when(
      mktcap <= size_med ~ "S",
      mktcap > size_med ~ "B"
    )
  )

size_cnt <- ret_type_data %>%
  group_by(sorting_year, ia_type) %>%
  count(size_type) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = c(sorting_year, ia_type),
    names_from = size_type,
    values_from = n,
    names_sep = ""
  )



portf <- ret_type_data %>%
  group_by(date, ia_type) %>%
  summarise(
    vwret = weighted.mean(ret, w = weight)
  ) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = date,
    values_from= vwret,
    names_from = ia_type,
    names_sep = ""
  )

rf_data <- read_csv("twbank.csv")
colnames(rf_data) <- c("secu_code",
                       "date",
                       "rf")
rf_data <- rf_data %>%
  select(-secu_code) %>%
  mutate(
    date = ym(date),
    rf = rf /100,
  )

rm_data <- read_csv("TTRI.csv")
colnames(rm_data) <- c("date",
                       "price",
                       "open",
                       "high",
                       "low",
                       "vol",
                       "market_ret")

rm_data <- rm_data %>%
  select(date, market_ret) %>%
  mutate(
    date = mdy(date),
    market_ret = as.numeric(gsub("\\%", "", market_ret)) / 100
  )

portf <- portf %>%
  left_join(rf_data, by = "date") %>%
  inner_join(rm_data, by = "date")


portf_cum <- portf %>%
  mutate(
    ia_1_cum = cumprod(1 + ia_1) - 1,
    ia_2_cum = cumprod(1 + ia_2) - 1,
    ia_3_cum = cumprod(1 + ia_3) - 1,
    ia_4_cum = cumprod(1 + ia_4) - 1,
    ia_5_cum = cumprod(1 + ia_5) - 1,
    market_ret_cum = cumprod(1 + market_ret) - 1
  ) %>%
  select(date, ia_1_cum:market_ret_cum)