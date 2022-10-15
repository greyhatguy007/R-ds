###################### Examples of function uses #############################
#table(dataset$feature) to see how many rows are in each class of $feature

#tapply(flags$animate, flags$landmass, mean) to apply the mean function to the 
#'animate' variable separately for each of the six landmass groups, thus giving 
# the proportion of flags containing an animate image WITHIN each landmass group.

#Similarly, we can look at a summary of population values (in round millions) 
#for countries with and withoutthe color red on their flag with 
#tapply(flags$population, flags$red, summary).

#split(df,df$feature) into groups of classes of feature $feature
###################### Examples of function uses #############################

#Write a function called best that take two arguments: the 2-character abbreviated 
#name of a state and anoutcome name. The function reads the outcome-of-care-measures.csv 
#file and returns a character vectorwith the name of the hospital that has the best 
#(i.e. lowest) 30-day mortality for the specifed outcomein that state. The hospital 
#name is the name provided in the Hospital.Name variable. The outcomes canbe one 
#of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data 
#on a particularoutcome should be excluded from the set of hospitals when deciding the rankings.


#Features used:
# 2 - "Hospital.Name" 
# 11 - "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
# 17 - "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
# 23 - "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"

best <- function(state, outcome){
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    df[, 11] <- as.numeric(df[, 11])
    df[, 17] <- as.numeric(df[, 17])
    df[, 23] <- as.numeric(df[, 23])
    
    valid_states <- unique(df$State)
    valid_outcome <- c('heart attack','heart failure', 'pneumonia')
    
    if (!is.element(state, valid_states)) { #check for invalid state
        stop("invalid state")
    }
    else if (!is.element(outcome, valid_outcome)) {#check for valid outcome
        stop("invalid outcome")
    }
    else{ #At this point only valid cases are going
        cases_by_state = subset(df, State == state) #Filter df by the specific given state
        if (outcome == 'heart attack') {
            minval <- min(cases_by_state[[11]][complete.cases(cases_by_state[[11]])])
            cases <- subset(cases_by_state, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minval)
            hospital <- cases$Hospital.Name
            hospital <- sort(hospital)
            return(hospital)
        }
        else if (outcome == 'heart failure') {
            minval <- min(cases_by_state[[17]][complete.cases(cases_by_state[[17]])])
            cases <- subset(cases_by_state, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == minval)
            hospital <- cases$Hospital.Name
            hospital <- sort(hospital)
            return(hospital)
        }
        else if (outcome == 'pneumonia') {
            minval <- min(cases_by_state[[23]][complete.cases(cases_by_state[[23]])])
            cases <- subset(cases_by_state, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == minval)
            hospital <- cases$Hospital.Name
            hospital <- sort(hospital)
            return(hospital)
        }
    }
}
