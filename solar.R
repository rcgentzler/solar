library(googlesheets4)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(timetk)
library(sweep)
library(forecast)

usage <- read_sheet("https://docs.google.com/spreadsheets/d/1FRiHkkYHltUrBbGEa5L9zOGv2PiuWP06XJDQXcDqO7U/edit#gid=919308853",
                    sheet = "Energy bills",
                    skip = 4) |>
  janitor::clean_names() |>
  rename(bill_days = x4,
         bill_30 = x30_day) |>
  select(end_date, bill_30, cost) |>
  pivot_longer(cols = c("bill_30", "cost"))


ggplot(usage, aes(end_date, value)) +
  geom_line() +
  facet_wrap(~name, scales = "free")

u <- usage |>
  group_by(name) |>
  nest()

# Use sweep package
u_ts <- u |>
  mutate(data.ts = map(.x = data,
                       .f = tk_ts,
                       start = 2019,
                       freq = 12))

u_ts_2 <- u_ts |>
  mutate(fit.ets = map(data.ts, ets))

aug <- u_ts_2 |>
  mutate(augment = map(fit.ets, sw_augment, timetk_idx = TRUE, rename_index = "date")) %>%
  unnest(augment)

ggplot(aug, aes(date, .actual)) +
  geom_line() +
  geom_line(aes(y = .fitted),
            linetype = "dotted") +
  facet_wrap(~name,
             scales = "free")

u_ts_fcast <- u_ts_2 |>
  mutate(fcast.ets = map(fit.ets, forecast, h = 12)) |>
  mutate(sweep = map(fcast.ets, sw_sweep, fitted = FALSE, timetk_idx = TRUE)) %>%
  unnest(sweep)

u_ts_fcast |>
  ggplot() +
  geom_line(aes(x = index, y = value, color = key)) +
  facet_wrap(~name,
             scales = "free")
