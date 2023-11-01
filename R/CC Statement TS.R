library(data.table)
library(tidyverse)
source("R/functions.R")

statement <- load_statement(cc = "AMEX") %>% 
  .[!str_detect(Merchant, "INSTALLMENT PLAN") | Amount < 0, Balance := cumsum(Amount)]

ggplot(statement[!is.na(Balance)]) +
  geom_line(aes(x = Date, y = Balance)) +
  theme_minimal()
