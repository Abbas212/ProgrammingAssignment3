#Subseting important Data as "outl"
outl=outcome[,c(2,7,11,17,23)]

#Viewing new dataset
#View(outl)

#Starting best function
best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        if(!state %in% outl$State)
        {stop("Invalid State")}
        else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) 
        {stop("Invalid outcome")}
        ## Return hospital name in that state with lowest 30-day death
                #Subseting data by state
                ts <- outl[which(outl$State== state ), ]
                if (outcome == "heart attack") {
                        colnum <- 3
                }
                else if (outcome == "heart failure") {
                        colnum <- 4
                }
                else {
                        colnum <- 5
                }
                min_row <- which(as.numeric(ts[ ,colnum]) == 
                                         min(as.numeric(ts[ ,colnum]), na.rm = TRUE))
                hospitals <- ts[min_row,1]
                hospitals <- sort(hospitals)
                return(hospitals[1])
        ## rate
}
