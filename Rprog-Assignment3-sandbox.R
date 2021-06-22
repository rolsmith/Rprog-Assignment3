##%######################################################%##
#                                                          #
#### Coursera > Rprogramming > Programming assignment 3 ####
####     Draft  > Roland Smith > 21 jun 2021            ####
#                                                          #
##%######################################################%##

getwd()

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
str(outcome)
dim(outcome)
names(outcome)

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])
outcome[,7]

#### Finding the min value for heart attack
heart.attack.min <- min(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, na.rm=TRUE)
heart.attack.min

?which.min

#### Finding corresponding hospital

heart.attack.min2 <- which.min(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
best <- outcome[heart.attack.min2,2]
best

#### Testing State Validity

state.valid <- function(state) {
  if (!state %in% outcome$State) {
    print("No such state")
  } else {print("great")}
}
state.valid("NY")
state.valid("Roland")

#### Testing approaches

outcome.state <- subset(outcome, State=="TX")
str(outcome.state)
class(outcome.state)

heart.attack.min <- which.min(outcome.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
heart.attack.min

best <- outcome.state [heart.attack.min, "Hospital.Name"]
best

#### Identifying condition

specific.condition <- function (condition,...) {
  
  condition.spec <- NULL
  condition.spec <- as.numeric(condition.spec)
  
  if (condition=="heart attack") {
    condition.spec <- outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    
  } else if (condition=="heart failure") {
    condition.spec <- outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    
  } else if (condition=="pneumonia") {
    condition.spec <- outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    
  } else {
   stop ('invalid outcome')
    }
}

