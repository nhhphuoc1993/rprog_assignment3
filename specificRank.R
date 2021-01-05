require(data.table)
specificRank <- function(input_state, input_outcome, input_num = "best") {
  ## Read outcome data
  data <- fread("outcome-of-care-measures.csv")
  ## Check that state
  states <- data$State
  if(!(input_state %in% states)) stop('Invalid input state!')
  
  state_data <- data[data$State == input_state]
  if(input_outcome == 'heart attack') sort_data <- state_data[order(as.numeric(state_data$`Hospital 30-Day Death (Mortality) Rates from Heart Attack`),state_data$`Hospital Name` ,na.last = NA)]
  else if (input_outcome == 'heart failure') sort_data <- state_data[order(as.numeric(state_data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`),state_data$`Hospital Name` ,na.last = NA)]
  else if (input_outcome == 'pneumonia') sort_data <- state_data[order(as.numeric(state_data$`Hospital 30-Day Death (Mortality) Rates from Pneumonia`),state_data$`Hospital Name` ,na.last = NA)]
  else stop('Invalid input outcome!')
  
  if(!is.numeric(input_num) | input_num == 0) stop('Invalid input num!')
  else if(input_num > nrow(sort_data)) return(NA)
  else return(sort_data[input_num]$`Hospital Name`)
}