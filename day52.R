split_n_cases <- function(x, y) {
  
  chunk_size<-nchar(x) / y
  
  startposition <- seq(1,nchar(x), by = chunk_size)
  substring(x,startposition, startposition + chunk_size -1)
}


# split_n_cases("Strengthened", 6) ➞ ["St", "re", "ng", "th", "en", "ed"]
# 
# split_n_cases("Unscrupulous", 2) ➞ ["Unscru", "pulous" ]
# 
# split_n_cases("Flavorless", 1) ➞ ["Flavorless" ]


x<-"Strengthened"
y<-6


chunk_size<-nchar(x) / y

startposition <- seq(1,nchar(x), by = chunk_size)
substring(x,startposition, startposition + chunk_size -1)




same_length <- function(x){
  result <- strsplit(x,"")[[1]]
  result <- as.numeric(result)
  sum(result ==1) ==  sum(result ==0)
}


same_length <- function(x) {
  # Split the input string into a vector of characters
  result <- strsplit(x, "")[[1]]
  
  # Initialize counters
  i <- 1
  n <- length(result)
  
  while (i <= n) {
    # Count the length of the current sequence of '1's
    if (result[i] == "1") {
      count_ones <- 0
      while (i <= n && result[i] == "1") {
        count_ones <- count_ones + 1
        i <- i + 1
      }
      
      # Count the length of the subsequent sequence of '0's
      count_zeroes <- 0
      while (i <= n && result[i] == "0") {
        count_zeroes <- count_zeroes + 1
        i <- i + 1
      }
      
      # If the counts are not equal, return FALSE
      if (count_ones != count_zeroes) {
        return(FALSE)
      }
    } else {
      # Move to the next character if it's not '1'
      i <- i + 1
    }
  }
  
  # If all sequences matched, return TRUE
  return(TRUE)
}

# 
# same_length("110011100010") ➞ True
# 
# same_length("101010110") ➞ False
# 
# same_length("111100001100") ➞ True
# 
# same_length("111") ➞ False

766