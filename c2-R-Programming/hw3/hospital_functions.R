best <- function(state, outcome) {
  ## Read outcome data
  outcomes_all <- read.csv("outcome-of-care-measures.csv")
  valid_states <- unique(outcomes_all$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  if (!state %in% valid_states) {
    stop("Invalid state, province, or territory.")
  }
  
  if (!outcome %in% valid_outcomes) {
    stop("Invalid outcome.")
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  # Default on heart attack
  outcome_col <- 11
  if (outcome == "heart failure") {
    outcome_col <- 17
  }
  else if (outcome == "pneumonia") {
    outcome_col <- 23
  }
  # Collect all state relevant rows
  outcomes_state <- outcomes_all[outcomes_all$State == state,]
  # Remove "Not Available"
  outcomes_state <- outcomes_state[!outcomes_state[, outcome_col] == "Not Available", ]
  
  # In case we get a tie, build up a sublist, sort, then send the first alphabetic one
  min_val <- min(as.numeric(as.character(outcomes_state[, outcome_col])))
  outcomes_best <- outcomes_state[as.numeric(as.character(outcomes_state[, outcome_col])) == min_val,]
  return(as.character(sort(outcomes_best[, 2])[1]))
}

rankhospital <- function(state, outcome, num="best") {
  ## Read outcome data
  ## Read outcome data
  outcomes_all <- read.csv("outcome-of-care-measures.csv")
  valid_states <- unique(outcomes_all$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  ## Check that state and outcome are valid
  if (!state %in% valid_states) {
    stop("Invalid state, province, or territory.")
  }
  
  if (!outcome %in% valid_outcomes) {
    stop("Invalid outcome.")
  }
  ## Return hostpital name in state with the given rank
  ## 30-day death rate
  # Default on heart attack
  outcome_col <- 11
  if (outcome == "heart failure") {
    outcome_col <- 17
  }
  else if (outcome == "pneumonia") {
    outcome_col <- 23
  }
  
  # Collect all state relevant rows
  outcomes_state <- outcomes_all[outcomes_all$State == state,]
  # Remove "Not Available"
  outcomes_state <- outcomes_state[!outcomes_state[, outcome_col] == "Not Available", ]
  # Reduce to just columns we're interested in
  outcomes_state <- outcomes_state[, c(2, outcome_col)]
  # Sort
  outcomes_state <- outcomes_state[order(as.numeric(as.character(outcomes_state[, 2])), outcomes_state[, 1]), ]
  
  # Return hospital from requested num
  if (num == "best") {
    num <- 1
  }
  else if (num == "worst") {
    num <- nrow(outcomes_state)
  }
  return(as.character(outcomes_state[num, 1]))
}


rankall <- function(outcome, num="best") {
  outcomes_all <- read.csv("outcome-of-care-measures.csv")
  valid_states <- sort(unique(outcomes_all$State))
  df <- data.frame(hospital=character(), state=character(), stringsAsFactors=FALSE)
  for (state in valid_states) {
    hospital <- rankhospital(state, outcome, num)
    df[nrow(df) + 1, ] <- c(hospital, state)
  }
  return(df)
}