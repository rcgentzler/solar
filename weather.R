library(googlesheets4)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)

options(scipen = 999)

w <- read_csv("USW00053908.csv") |>
  janitor::clean_names() |>
  select(date, tmax, tmin) |>
  mutate(hi = (tmax/5.5)+32)

w |>
  ggplot(aes(date, hi)) +
  geom_line() +
  geom_hline(aes(yintercept = 75))

usage <- read_sheet("https://docs.google.com/spreadsheets/d/1FRiHkkYHltUrBbGEa5L9zOGv2PiuWP06XJDQXcDqO7U/edit#gid=919308853",
                    sheet = "Energy bills",
                    skip = 4) |>
  janitor::clean_names() |>
  rename(bill_days = x4,
         bill_30 = x30_day) |>
  select(start_date, end_date, usage, cost, unit_cost)

w <- w |>
  select(date, hi) |>
  left_join(usage |>
              mutate(date = start_date,
                     bill_date = start_date) |>
              select(date, bill_date)) |>
  fill(bill_date)

wm <- w |>
  mutate(deg_over_75 = if_else(hi > 75,
                               hi - 75,
                               0)) |>
  group_by(start_date = bill_date) |>
  filter(!is.na(start_date),
         date <= ymd("2022-08-23")) |>
  summarize(deg75days = sum(deg_over_75, na.rm = TRUE)) |>
  left_join(usage)

ggplot(wm, aes(deg75days, usage)) +
  geom_point() +
  geom_label(aes(label = start_date))

lmfit <- lm(usage ~ deg75days, wm)


