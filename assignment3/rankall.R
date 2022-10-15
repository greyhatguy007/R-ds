#Write a function called rankall that takes two arguments: an outcome name (outcome) 
#and a hospital rank-ing (num). The function reads the outcome-of-care-measures.csv 
#file and returns a 2-column data framecontaining the hospital in each state that 
#has the ranking specied in num. For example the function call rankall("heart attack", "best")
#would return a data frame containing the names of the hospitals thatare the best in
#their respective states for 30-day heart attack death rates. The function should 
#return a valuefor every state (some may be NA). The first column in the data frame 
#is named hospital, which contains the hospital name, and the second column is named 
#state, which contains the 2-character abbreviation forthe state name. Hospitals 
#that do not have data on a particular outcome should be excluded from the set of 
#hospitals when deciding the rankings.


#Features used:
# 2 - "Hospital.Name" 
# 11 - "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
# 17 - "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
# 23 - "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"

rankall <- function(outcome, rank = "best"){
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    df[, 11] <- as.numeric(df[, 11])
    df[, 17] <- as.numeric(df[, 17])
    df[, 23] <- as.numeric(df[, 23])
    
    if (is.numeric(rank)) { #rank treatment
        if (rank>length(unique(df$Hospital.Name))) {
            return(NA)
        }
    }
    else{
        if (rank == 'best') {
            rank <- 1 #first position in sorted vector
        }
    }
    
    valid_states <- sort(unique(df$State))
    valid_outcome <- c('heart attack','heart failure', 'pneumonia')
    
    if (!is.element(outcome, valid_outcome)) {#check for valid outcome
        stop("invalid outcome")
    }
    else{ #At this point only valid cases are going
        for (i in valid_states) { #For each state
            cases_by_state = subset(df, State == i) #Filter df by the specific given state
            
            if (outcome == 'heart attack') {
                #Filter the Only two columns we need
                cases <- cases_by_state[,c("Hospital.Name",
                                           "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")] 
                cases <- cases[complete.cases(cases),] #Filter Non-NaN rows
                #Sort cases by DeathRate, than by HospitalName
                cases <- cases[with(cases, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                                 Hospital.Name)), ]
                
                if (rank=='worst') { #take the last element
                    hospital <- tail(cases$Hospital.Name,1)
                }
                else{ #take the rank index element
                    hospital <- cases$Hospital.Name[rank]
                }
            }
            
            else if (outcome == 'heart failure') {
                #Filter the Only two columns we need
                cases <- cases_by_state[,c("Hospital.Name",
                                           "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")] 
                cases <- cases[complete.cases(cases),] #Filter Non-NaN rows
                #Sort cases by DeathRate, than by HospitalName
                cases <- cases[with(cases, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                                 Hospital.Name)), ]
                
                if (rank=='worst') { #take the last element
                    hospital <- tail(cases$Hospital.Name,1)
                }
                else{ #take the rank index element
                    hospital <- cases$Hospital.Name[rank]
                }
            }
            
            else if (outcome == 'pneumonia') {
                #Filter the Only two columns we need
                cases <- cases_by_state[,c("Hospital.Name",
                                           "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")] 
                cases <- cases[complete.cases(cases),] #Filter Non-NaN rows
                #Sort cases by DeathRate, than by HospitalName
                cases <- cases[with(cases, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                                                 Hospital.Name)), ]
                
                if (rank=='worst') { #take the last element
                    hospital <- tail(cases$Hospital.Name,1)
                }
                else{ #take the rank index element
                    hospital <- cases$Hospital.Name[rank]
                }
            }
            
            if (i==valid_states[1]) {
                hospital_stack <- hospital
                states_stack <- i
            }
            else{
                hospital_stack <- c(hospital_stack,hospital)
                states_stack <- c(states_stack,i)
            }
        }#close for
        df_answer <- data.frame("hospital" = hospital_stack, "state" = states_stack)
        return(df_answer)
    }#close outer else
}#close function