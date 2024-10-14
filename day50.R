#day 50
track_robot <- function(x){
  x.axis <-0
  y.axis <- 0
  
  
  
  for (i in seq_along(x)) {
    main.x<-x[[i]]
    result<-strsplit(main.x," ")[[1]]
    
    if(result[[1]] == "right"){
      x.axis <- x.axis  + as.numeric(result[[2]])
    }
    if(result[[1]] == "up"){
      y.axis <- y.axis  + as.numeric(result[[2]])
    } 
    if(result[[1]] == "left"){
      x.axis <- x.axis  - as.numeric(result[[2]])
    } 
    
    if(result[[1]] == "down"){
      y.axis <- y.axis  - as.numeric(result[[2]])
    } 
    
  }
  return(c(x.axis,y.axis)) 
}

#track_robot(c("right 10", "up 50", "left 30", "down 10")) ➞ [-20, 40]

#track_robot(c()) ➞ [0, 0]
#// If there are no instructions, the robot doesn't move.

#track_robot(c("right 100", "right 100", "up 500", "up 10000")) ➞ [200, 10500]

track_robot(c("left 10", "left 100", "left 1000", "left 10000"))