library(readxl)
library(data.table)
library(tidyverse)
library(lubridate)
source("src/functions.R")

data <- setNames(lapply(excel_sheets("data/raw/balancesheet.xlsx"), function(x) {
  as.data.table(read_xlsx("data/raw/balancesheet.xlsx", sheet = x))}),
                   excel_sheets("data/raw/balancesheet.xlsx"))

last_pay <- max(data$Payroll$PayDay) %>% as.Date()

dt <- forecast_account_balance(data$BalanceSheet[Type == "Recurring Liability"], 
                               assetForecast(last_payday = last_pay, 
                                             n_paychecks = 10,
                                             HrlyWage = sum(data$Payroll$NetPay)/sum(data$Payroll$Hrs), 
                                             stat_holidays = data$StatHolidays$Date), 
                               start_date = last_pay,
                               initial_balance = sum(data$BalanceSheet[Type == "Asset"]$Amount))

ggplot(data = dt, aes(x = date, y = balance)) +
  geom_line() +
  labs(title = "Forecasted Account Balance",
       x = "Date",
       y = "Balance") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal() +
  geom_hline(yintercept = 3000, linetype = "dashed", color = "red") +
  expand_limits(y = 0) 
