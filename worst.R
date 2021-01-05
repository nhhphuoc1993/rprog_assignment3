require(data.table)
worst <- function(input_state, input_outcome) {
  ## Read outcome data
  data <- fread("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  states <- data$State
  if(!(input_state %in% states)) stop('Invalid input state!')
  
  state_data <- data[data$State == input_state]
  if(input_outcome == 'heart attack') {
    values <- as.numeric(state_data$`Hospital 30-Day Death (Mortality) Rates from Heart Attack`)
    selected_value <- max(values, na.rm = TRUE)
    selected_value_rows <- state_data[as.numeric(state_data$`Hospital 30-Day Death (Mortality) Rates from Heart Attack`) == selected_value]
  }
  else if (input_outcome == 'heart failure') {
    values <- as.numeric(state_data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`)
    selected_value <- max(as.numeric(state_data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`), na.rm = TRUE)
    selected_value_rows <- state_data[as.numeric(state_data$`Hospital 30-Day Death (Mortality) Rates from Heart Failure`) == selected_value]
  }
  else if (input_outcome == 'pneumonia') {
    values <- as.numeric(state_data$`Hospital 30-Day Death (Mortality) Rates from Pneumonia`)
    selected_value <- max(values, na.rm = TRUE)
    selected_value_rows <- state_data[as.numeric(state_data$`Hospital 30-Day Death (Mortality) Rates from Pneumonia`) == selected_value]
  }
  else {
    stop('Invalid input outcome!')
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  hospital_names <- selected_value_rows$`Hospital Name`
  return(hospital_names[sort.list(hospital_names)][1])
}