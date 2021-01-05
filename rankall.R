require(data.table)
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- fread("outcome-of-care-measures.csv")
  
  if(input_outcome == 'heart attack') col_name <- 'Hospital 30-Day Death (Mortality) Rates from Heart Attack'
  else if (input_outcome == 'heart failure') col_name <- 'Hospital 30-Day Death (Mortality) Rates from Heart Failure'
  else if (input_outcome == 'pneumonia') col_name <- 'Hospital 30-Day Death (Mortality) Rates from Pneumonia'
  else stop('Invalid input outcome!')
  
  if(input_num == 'best') {
    numeric_data <- data[!is.na(as.numeric(col_name))]
    
    
    order_numeric_data <- numeric_data[order(numeric_data$State, numeric_data$`Hospital Name`)]
    return(numeric_data[numeric_data[, .I[as.numeric(`Hospital 30-Day Death (Mortality) Rates from Heart Failure`) == min(as.numeric(`Hospital 30-Day Death (Mortality) Rates from Heart Failure`))], by=State]$V1])
  } else if(input_num == 'worst') {
    numeric_data <- data[!is.na(as.numeric(data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`))]
    order_numeric_data <- numeric_data[order(numeric_data$State, numeric_data$`Hospital Name`)]
    return(numeric_data[numeric_data[, .I[as.numeric(`Hospital 30-Day Death (Mortality) Rates from Heart Failure`) == max(as.numeric(`Hospital 30-Day Death (Mortality) Rates from Heart Failure`))], by=State]$V1])
  }
  else if(is.numeric(input_num) & input_num != 0) {
    return(specificRank(input_state, input_outcome, input_num))
  } else stop('Invalid input num!')
  
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  # numeric_data <- data[!is.na(as.numeric(data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`))]
}