rankhospital <- function(state, outcome, num="best"){

    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character",
                     na.strings=c("Not Available", "NA"))
    
    ## Check that state and outcome are valid
    if(!(toupper(state) %in% states)){
        stop("invalid state")
    }
    outcomes = list("heart attack"=11, "heart failure"=17 ,"pneumonia"=23)
    if(length(outcomes[[outcome]]) == 0){
        stop("invalid outcome")
    }
    ##Return hospital name in that state with lowest 30 day death rate
    

    col = as.numeric(outcomes[outcome])
    hospitals <- data[(data[7] == state) & ((data[col] != "Not Available") 
                                            | (data[col] != "NA")),]
    hospitals[,col] <- as.numeric(hospitals[,col])
    hospitals <- hospitals[with(hospitals, 
                                order(hospitals[,col],hospitals[2])),]
    hospitals <- hospitals[!is.na(hospitals[,col]),]
    if(num == "best"){
        return(head(hospitals[,2],1))
    }
    else if(num == "worst"){

        return(tail(hospitals[,2],1))
    }
    else{
        return(c(head(hospitals[,2],num))[num])
    }    
        
}
