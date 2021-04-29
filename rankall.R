#Subseting important Data as "outl"
outl=outcome[,c(2,7,11,17,23)]

rankall <- function(outcome, num = "best") {
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) 
        {stop("Invalid outcome")}
        if (outcome == "heart attack") {
                colnum <- 3
        }
        else if (outcome == "heart failure") {
                colnum <- 4
        }
        else {
                colnum <- 5
        }
        outl[ ,colnum] <- as.numeric(outl[ ,colnum])
        outl <- outl[(!is.na(outl[ ,colnum])), ]
        
        splited = split(outl, outl$State)
        ans = lapply(splited, function(x, num) {
                x = x[order(x[,colnum], x$Hospital.Name),]
        
        if(num == "best"){
                return (x$Hospital.Name[1])
        }            
        else if (num == "worst"){
                return (x$Hospital.Name[nrow(x)])
        }
        else{
                return (x$Hospital.Name[num])
        }}, num)
        return (data.frame(hospital=unlist(ans), state=names(ans)))
}