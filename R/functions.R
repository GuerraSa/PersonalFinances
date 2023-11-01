assetForecast <- function(last_payday, n_paychecks, balance_sheet, stat_holidays, target_balance, HrlyWage) {
  wk1 <- seq(as.Date(last_payday), length.out = n_paychecks, by = "2 weeks")
  wk2 <- seq(as.Date(last_payday) + weeks(1), length.out = n_paychecks, by = "2 weeks")
  next_pay <- seq(as.Date(last_payday) + weeks(2), length.out = n_paychecks, by = "2 weeks")
  
  dt <- data.table(Week1 = wk1,
                   Week2 = wk2,
                   Payday = next_pay)[, Hrs := (10-mapply(function(start_date, end_date, holidays) {
                     return(sum(start_date <= holidays & holidays <= end_date))
                   }, floor_date(Week1, unit = "weeks", week_start = 1), Week2, list(as.Date(stat_holidays))))*7
                   ][, NetPay := Hrs*HrlyWage]
  return(dt)
}

# Create a function to forecast account balance
forecast_account_balance <- function(future_payments, expected_income, start_date, initial_balance) {
  # Convert dueDate and payDay columns to Date format
  future_payments[, ":="(Terms = as.numeric(Terms), domDue = as.numeric(domDue))
                  ][, ":="(DueDate = floor_date(start_date + months(1), unit = "months") + days(domDue - 1))]
  expected_income[, Payday := as.Date(Payday)]
  
  # Calculate the range of dates for the forecast
  end_date <- max(expected_income$Payday)
  all_dates <- seq(start_date, end_date, by = "days")
  
  # Initialize an empty data table to store the forecasted results
  forecasted_balance <- data.table(date = all_dates, balance = NA_real_)
  
  
  # Loop through each date in the forecast
  for(d in all_dates) {
    d <- as.Date(d, "1970-01-01")
    # Calculate the total payments due on this date
    total_payments_due <- sum(future_payments[DueDate == d & Terms > 0]$Amount)
    
    # Calculate the total income expected on this date
    total_income_expected <- sum(expected_income[Payday == d]$NetPay)
    
    # Calculate the balance for this date. Update the initial balance for the next date
    balance <- initial_balance - total_payments_due + total_income_expected
    initial_balance <- balance 
    
    # Add the date and balance to the forecasted data table
    forecasted_balance[date == d,]$balance <- balance
    
    # Update nTerms for future payments that are due on this date
    if(future_payments[DueDate == d, .N] != 0) {
      # print(future_payments[DueDate == d])
      future_payments[DueDate == d & !is.infinite(Terms) & 
                        Terms > 0, ":="(Terms = Terms - 1,
                                        DueDate = DueDate + months(1))]
      future_payments[DueDate == d & is.infinite(Terms), DueDate := DueDate + months(1)]
      
    }
  }
  
  # Return the forecasted balance data table
  return(forecasted_balance)
}

load_statement <- function(cc, delete.raw = FALSE) {
  glossary <- fread("data/raw/statement_categories.csv")
  
  path <- c("raw" = list.files(paste0("data/raw/statements/", cc), pattern = ".csv$", full.names = T),
            "processed" = paste0("data/processed/", cc, ".csv"))
  if(cc == "AMEX") {
    if(file.exists(path["processed"])) {
      dt <- fread(path["processed"], sep = ",") %>% 
        .[, Date := as.Date(Date)]
    } else if(file.exists(path["raw"])) {
      tx_data <- readxl::read_xls("data/raw/statements/AMEX/Summary.xls", skip = 11) %>% 
        select(Date, `Date Processed`, Description, Merchant, Amount, 
               `Foreign Spend Amount`, `Exchange Rate`) %>% 
        mutate(Date = dmy(Date), year = year(Date), month = month(Date, label = TRUE),
               `Date Processed` = dmy(`Date Processed`),
               Amount = as.numeric(gsub("\\$|,", "", Amount)),
               `Exchange Rate` = as.numeric(`Exchange Rate`)) %>% 
        mutate(Description = gsub("[^[:alpha:][:space:]]","", Description),
               Description = case_when(str_detect(Description, 'MCDONALDS') ~ 'MCDONALDS',
                                       str_detect(Description, 'WALMART') ~ 'WALMART',
                                       str_detect(Description, 'TIM HORTONS') ~ 'TIM HORTONS',
                                       str_detect(Description, 'AMAZON') ~ 'AMAZON',
                                       str_detect(Description, 'BC LIQUOR') ~ 'BC LIQUOR',
                                       str_detect(Description, 'TERYX') ~ 'ARCTERYX',
                                       str_detect(Description, 'SELF SERVE TICK') ~ 'BC FERRIES',
                                       str_detect(Description, 'YOUTUBEPREMIUM') ~ 'YOUTUBE PREMIUM',
                                       str_detect(Description, 'COMPASS') ~ 'COMPASS',
                                       str_detect(Description, 'HUSKY') ~ 'HUSKY',
                                       str_detect(Description, 'LONDON DRUGS') ~ 'LONDON DRUGS',
                                       str_detect(Description, 'LUZ TACOS') ~ 'LUZ TACOS',
                                       str_detect(Description, 'MEDITERRANEAN GRILL') ~ 'MEDITERRANEAN GRILL',
                                       str_detect(Description, 'ALIPAY') ~ 'ALIBABA',
                                       str_detect(Description, 'DOORDASH') ~ 'DOORDASH',
                                       str_detect(Description, 'MECMOUNTAIN') ~ 'MEC',
                                       str_detect(Description, 'SUSHI KOO') ~ 'SUSHI KOO',
                                       str_detect(Description, 'XPRESS.*DONAIR') ~ 'XPRESS DONAIR',
                                       str_detect(Description, 'YELLOW DOG') ~ 'YELLOW DOG',
                                       str_detect(Description, 'DOLLARAMA') ~ 'DOLLARAMA',
                                       str_detect(Description, 'PAYMENT') ~ 'PAYMENT',
                                       str_detect(Description, 'MACS') ~ 'MACS CONVENIENCE',
                                       str_detect(Description, 'EBAY') ~ 'EBAY',
                                       str_detect(Description, 'BCS|BCF') ~ 'BC FERRIES',
                                       str_detect(Description, 'VAPE') ~ 'VAPE',
                                       str_detect(Description, 'COLUMBIA SPORTSWEAR') ~ 'COLUMBIA SPORTSWEAR',
                                       str_detect(Description, 'DISNEY PLUS') ~ 'DISNEY PLUS',
                                       str_detect(Description, 'CACTUS CLUB') ~ 'CACTUS CLUB',
                                       str_detect(Description, 'UBER TRIP') ~ 'UBER',
                                       str_detect(Description, 'HOTEL MIO') ~ 'HOTEL MIO',
                                       str_detect(Description, "CHV") ~ 'CHEVRON',
                                       str_detect(Description, 'STARBUCKS') ~ 'STARBUCKS',
                                       .default = str_remove_all(Description, "(PORT|) COQUITLAM|(NORTH|) VANCOUV(|ER)|WHISTLER|BURNABY|VICTORIA|PAYPAL "))) %>% 
        group_by(year, month, Description) %>% 
        summarise(Total = sum(Amount, na.rm = TRUE),
                  .groups = 'drop')
      
      raw <- fread(path["raw"], select = 1:5,
                   col.names = c("Date", "Reference ID", "Amount", "Merchant", "Notes"))
      raw <-  raw[, .(Date = as.Date(mdy(Date)), Amount,
                      `Reference ID` = str_remove_all(`Reference ID`, "Reference: "),
                      Merchant = str_replace(Merchant, "[:digit:].*", ""),
                      Notes)]
      
     if(any(str_detect(ls(), "dt"))){
       raw <- raw[!dt, on = "Reference ID"]
       dt <- rbind(dt, raw)
     } else dt <- raw
    } 
  } else if(cc == "RBC") {
    if(file.exists(path["processed"])) {
      dt <- fread(path["processed"])[, Date := as.Date(Date)]
      setorder(dt, -Date)
    }
    if(file.exists(path["raw"])) {
      raw <- fread(path["raw"],
                  select = c(1,3,5,6,7),
                  col.names = c("Account", "Date", "Merchant", "Notes", "Amount")) %>% 
        .[, Date := as.Date(mdy(Date))]
      
      if(any(str_detect(ls(), "dt"))){
        raw <- raw[!dt, on = c("Account", "Date", "Merchant", "Amount")]
        dt <- rbind(dt, raw)
      } else dt <- raw
    } 
  }
  if(delete.raw) unlink(path["raw"])
  setorder(dt, Date)
  write.csv(dt, paste0("data/processed/", cc, ".csv"), row.names = F)
  return(dt)
}
