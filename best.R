library(dplyr)
library(tidyr)
library(magrittr)

## Finding the best hospital in a state

best <- function(state, outcome){
        ## Read outcome data
        hosp_dat <- read.csv("outcome-of-care-measures.csv", 
                             na.strings = "Not Available", 
                             stringsAsFactors = FALSE) %>%
                # Select the data that is needed
                select(State, starts_with("Hospital")) %>% 
                select(-contains("Readmission")) %>%
                gather(Outcome, Rate, contains("Rate")) %>%
                mutate(Outcome2 = tolower(gsub("[.]", " ", 
                                               gsub("Hospital.30.Day.Death..Mortality..Rates.from.", "", 
                                                    Outcome)))) %>%
                select(-Outcome) %>%
                # Remove missing values
                filter(!is.na(Rate)) %>% 
                # Organise data by State, Outcome and Hospital Name
                group_by(State, Outcome2, Hospital.Name) %>% 
                # Get the minimum values for the hospital Rate
                summarise(minRate = min(Rate)) %>%
                # Reorganize the values of Rate by hospital name
                arrange(State, Outcome2, minRate, Hospital.Name)
        
        ## Check if state and outcome are valid
        
        if (!(state %in% unique(hosp_dat$State))) stop("Invalid state")
        
        if (!(outcome %in% unique(hosp_dat$Outcome2))) stop("Invalid outcome")
        
        ## Then select the right hospital 
        hosp_r <- hosp_dat %>% 
                filter(State == state & Outcome2 == outcome) %>% 
                summarise(fmin = first(Hospital.Name)) 
        
        print(hosp_r[[3]]) 
}

# Testing the function
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "pneumonia")
best("MD", "heart attack")
best("BB", "hert attack")
best("NY", "hert attack")

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")


