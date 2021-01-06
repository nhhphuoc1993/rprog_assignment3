require(data.table)
rankall <- function(input_outcome, input_num = "best") {
  
  if(input_outcome == 'heart attack') selected_cols <- c('Hospital Name', 'State', 'Hospital 30-Day Death (Mortality) Rates from Heart Attack')
  else if (input_outcome == 'heart failure') selected_cols <- c('Hospital Name', 'State', 'Hospital 30-Day Death (Mortality) Rates from Heart Failure')
  else if (input_outcome == 'pneumonia') selected_cols <- c('Hospital Name', 'State', 'Hospital 30-Day Death (Mortality) Rates from Pneumonia')
  else stop('Invalid input outcome!')
  
  data <- fread("outcome-of-care-measures.csv", select = selected_cols)
  setnames(data, selected_cols, c('hospital_name', 'state', 'rate'))
  
  data <- data[, rate:= as.numeric(rate)]
  data <- data[!is.na(as.numeric(rate))]
  data <- data[order(state,rate,hospital_name)]
  
  if(input_num == 'best') return(data[, .SD[1], by = state])#return(data[data[, .I[rate == min(rate)], by=state]$V1])
  else if(input_num == 'worst') return(data[, .SD[.N], by = state])#return(data[data[, .I[rate == max(rate)], by=state]$V1])
  else if(is.numeric(input_num) & input_num != 0) return(data[, .SD[input_num], by = state])
  else stop('Invalid input num!')
}

# source('rankall.R')