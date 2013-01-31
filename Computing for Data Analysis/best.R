##best.R
best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Coerce certain numericl data to numeric
  data[, 11] <- as.numeric(data[, 11])
  data[, 17] <- as.numeric(data[, 17])
  data[, 23] <- as.numeric(data[, 23])
  
  ## Create character vector of possible valid outcomes
  outcome_vec <- c("heart attack", "heart failure", "pneumonia")    
  
  ## Check that state and outcome are valid
  if (!(state %in% data$State)) stop("Invalid State")            
  
  if (!(outcome %in% outcome_vec)) stop("Invalid Outcome")      
  
  ## Based on provided state, reduce data to a subset 
  red_data <- subset(data, select=c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                    Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                    Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),
                                    subset = (State == state) )

  ## Select data to order upon based upon provided outcome
  if(outcome == "heart attack") {outcome_mod = red_data[,2]}
  if(outcome == "heart failure") {outcome_mod = red_data[,3]}
  if(outcome == "pneumonia") {outcome_mod = red_data[,4]}
  
  ## Return hospital name in given state with lowest (best) 30-day death rate
  
  red_data_order <- red_data[order(outcome_mod, na.last = NA) ,]
  
  return(as.character(red_data_order[1,1]))
  
}




