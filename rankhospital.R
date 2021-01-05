require(data.table)
rankhospital <- function(input_state, input_outcome, input_num = "best") {
  ## Read outcome data
  data <- fread("outcome-of-care-measures.csv")
  ## Check that state
  states <- data$State
  if(!(input_state %in% states)) stop('Invalid input state!')
  ## Return hospital name in that state with the given rank
  if(input_num == 'best') {
    return(best(input_state, input_outcome))
  } else if(input_num == 'worst') {
    return(worst(input_state, input_outcome))
  }
  else if(is.numeric(input_num) & input_num != 0) {
    return(specificRank(input_state, input_outcome, input_num))
  } else stop('Invalid input num!')
}

# source('best.R')
# source('worst.R')
# source('specificRank.R')
# source('rankhospital.R')