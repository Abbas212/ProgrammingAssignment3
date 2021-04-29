#Subseting important Data as "outl"
outl=outcome[,c(2,7,11,17,23)]



rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        if(!state %in% outl$State)
        {stop("Invalid State")}
        else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) 
        {stop("Invalid outcome")}
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
                ts[ ,colnum] <- as.numeric(ts[ ,colnum])
                a <- ts[order(ts[,colnum],ts[,1]), ]
                a <- a[(!is.na(a[ ,colnum])), ]
                if(num == "best"){
                        num <- 1
                }            
                else if (num == "worst"){
                        num <- nrow(a)
                }
                return(a[num,1])
}
