library(dplyr)
library(tidyr)
library(magrittr)

## Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num="best"){
        ## Read outcome data
        hosp_dat <- read.csv("outcome-of-care-measures.csv", 
                             na.strings = "Not Available", 
                             stringsAsFactors = FALSE) %>%
                #Select the data that is needed
                select(State, starts_with("Hospital")) %>% 
                select(-contains("Readmission")) %>%
                gather(Outcome, Rate, contains("Rate")) %>%
                mutate(Outcome2 = tolower(gsub("[.]", " ", 
                        gsub("Hospital.30.Day.Death..Mortality..Rates.from.", "", 
                        Outcome)))) %>%
                select(-Outcome) %>%
                #Remove missing values
                filter(!is.na(Rate)) 
        
        ## Check that state and outcome are valid
        if (!(state %in% unique(hosp_dat$State))) stop("Invalid state")
        
        if (!(outcome %in% unique(hosp_dat$Outcome2))) stop("Invalid outcome")
        
        ## Subset the data by state and outcome
        subdat <- hosp_dat %>%
                  filter(State==state & Outcome2==outcome) %>% 
                  arrange(State, Rate, Hospital.Name) %>%
                  mutate(Rank = rank(Rate, ties.method = "first"))
        
        ## Find the hospital of the given rank 
        if (is.character(num)){
                if (num == "best"){
                        rvalue = min(subdat$Rank)
                } else if (num == "worst"){
                        rvalue = max(subdat$Rank)
                } else { 
                        stop(paste(num, "is an invalid rank"))
                }
        } else rvalue = num
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if (rvalue > max(subdat$Rank)){
                return(NA)
        } else { 
                hosp_rank = subdat %>% ungroup() %>% filter(subdat$Rank == rvalue)
                print(hosp_rank)
        }
}

## Testing the function

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", "5000")
rankhospital("TX", "heart attack", "best")
rankhospital("TX", "heart attack", 1)
rankhospital("TX", "heart attack")
rankhospital("CA", "heart failure", 25)
rankhospital("CA", "heart attack", 9)
rankhospital("CA", "heart failure", 90000)

