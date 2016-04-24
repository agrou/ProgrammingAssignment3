library(dplyr)
library(tidyr)
library(magrittr)

## Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        hosp_dat <- read.csv("outcome-of-care-measures.csv", 
                              na.strings = "Not Available", 
                              stringsAsFactors = FALSE) %>%
        # Select only the data that is needed
        select(State, starts_with("Hospital")) %>% 
        select(-contains("Readmission")) %>%
        gather(Outcome, Rate, contains("Rate")) %>%
        mutate(Outcome2 = tolower(gsub("[.]", " ", 
                gsub("Hospital.30.Day.Death..Mortality..Rates.from.", "", 
                                                    Outcome)))) %>%
        select(-Outcome) %>%
        filter(!is.na(Rate))
        
        ## Check that outcome is valid

        if (!(outcome %in% unique(hosp_dat$Outcome2))) stop("Invalid outcome")
                
        ## For each state, find the hospital of the given rank
        ## Subset the data by state
        subdat <<- hosp_dat %>%
                filter(Outcome2==outcome) %>%
                arrange(State, Rate, Hospital.Name) %>%
                group_by(State) %>%
                mutate(Rank = row_number()) %>%
                mutate(maxRank = max(Rank)) 
        
        ## Find the hospital of the given rank 
        if (is.character(num)){
                if (num == "best"){
                        rvalue = min(subdat$Rank)
                } else if (num == "worst"){
                        rvalue = -1
                } else { 
                        stop(paste(num, "is an invalid rank"))
                }
        } else rvalue = num
        
        ## Return a data frame with the hospital names and the 
        ## (abbreviated) state name
        if (rvalue >= 0) {
                hosp_rank <- subdat %>%  
                        filter(Rank == rvalue) %>%
                        select(Hospital.Name, State) %>%
                        arrange(State)
        } else { 
                hosp_rank <<- subdat %>% 
                filter(maxRank == Rank) %>% 
                select(Hospital.Name, State) %>%
                arrange(State)
        }   

        return(hosp_rank)        
}

# Testing the function
head(rankall("heart attack", 20), 10)
tail(rankall("heart attack", "worst"), 10)
tail(rankall("heart failure", "worst"), 10)
tail(rankall("heart failure"), 10)
tail(rankall("pneumonia", "worst"), 3)
rankall("heart failure", "worst")

r <- rankall("heart attack", 4)
as.character(subset(r, State == "HI")$Hospital.Name)

r <- rankall("pneumonia", "worst")
as.character(subset(r, State == "NJ")$Hospital.Name)

r <- rankall("heart failure", 10)
as.character(subset(r, State == "NV")$Hospital.Name)