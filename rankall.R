rankall <- function(outcome, num="best"){
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that state and outcome are valid
    states <- unique(data[,7])
    outcomes = list("heart attack"=11, "heart failure"=17 ,"pneumonia"=23)
    if(length(outcomes[outcome]) == 0){
        stop("invalid outcome")
    }
    ## For each state, find the hospital of the given rank
    col = as.numeric(outcomes[outcome])
    hospitals <- data[((data[col] != "Not Available") 
                                            | (data[col] != "NA")),]
    hospitals[,col] <- as.numeric(hospitals[,col])
    hospitals <- hospitals[with(hospitals, 
                                order(hospitals[,col],hospitals[2])),]
    hospitals <- data.frame(hospitals[!is.na(hospitals[,col]),])
    
    
    
    hospitalRanks <- data.frame(hospital="NA", state = states)
    hospitalRanks[,"state"] <- states
#     message(head(hospitals[,2],1))
    hospitalRanks[,"hospital"] <- sapply(hospitalRanks[,"state"], FUN = findhospital,
                                         hospitals = hospitals, num = num)
    ##Return a data frame with the hospital names and the
    ##(abbreviated) state name
    return(hospitalRanks)

    
}

findhospital <- function(state, hospitals, num){
    hospitals <- hospitals[hospitals[7] == state]
    if(num == "best"){
        return(head(hospitals[2],1))
    }
    else if(num == "worst"){
        return(tail(hospitals[2],1))
    }
    else{
        return(c(head(hospitals[2],num))[num])
    } 
}
