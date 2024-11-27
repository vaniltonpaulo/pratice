interview <- function(x,max.inter) {
  if(length(x) != 8) return("disqualified")
  int.time.allowed <- c(5,5,10,10,15,15,20,20)
  result <- logical(0)
  for (i in seq_along(x)) {
    if(x[[i]] > int.time.allowed[[i]]){
      result[[length(result) + 1]] <- TRUE
    }
  }
  result
  if(length(result) > 0) return("disqualified")
  if(max.inter <= 120) return("qualified")
  return("disqualified")
}



# interview(c(5, 5, 10, 10, 15, 15, 20, 20), 120) ➞ "qualified"
# 
# interview(c(2, 3, 8, 6, 5, 12, 10, 18), 64) ➞  "qualified"
# 
# interview(c(5, 5, 10, 10, 25, 15, 20, 20), 120) ➞ "disqualified"
# # Exceeded the time limit for a medium question.
# 
# interview(c(5, 5, 10, 10, 15, 15, 20), 120) ➞ "disqualified"
# # Did not complete all the questions.
# 
# interview(c(5, 5, 10, 10, 15, 15, 20, 20), 130) ➞ "disqualified"
# # Solved all the questions in their respected time limits but exceeded the total time limit of the interview.

interview(c(10, 10, 15, 15, 20, 20), 150)


consecutive_combo <- function(x, y) {
  result <- sort(union(x,y))
  final<-min(result):max(result)
  ifelse(isTRUE(all.equal(result,final)),TRUE,FALSE)
}


# consecutive_combo(c(7, 4, 5, 1), c(2, 3, 6)) ➞ True
# 
# consecutive_combo(c(1, 4, 6, 5), c(2, 7, 8, 9)) ➞ False
# 
# consecutive_combo(c(1, 4, 5, 6), c(2, 3, 7, 8, 10)) ➞ False
# 
# consecutive_combo(c(44, 46), c(45)) ➞ True



tallest_skyscraper <- function(mat){
  max(apply(mat, 2, sum))
}
grid1 <- matrix(c(
  0, 0, 0, 0,
  0, 1, 0, 0,
  0, 1, 1, 0,
  1, 1, 1, 1
), nrow = 4, byrow = T)

# Example 2
grid2 <- matrix(c(0, 1, 0, 0,
                  0, 1, 0, 0,
                  0, 1, 1, 0,
                  1, 1, 1, 1
), nrow = 4, byrow = TRUE)

# Example 3
grid3 <- matrix(c(
  0, 0, 0, 0,
  0, 0, 0, 0,
  1, 1, 1, 0,
  1, 1, 1, 1
), nrow = 4, byrow = TRUE)

tallest_skyscraper(grid1)
tallest_skyscraper(grid2)
tallest_skyscraper(grid3)

remove_letters <- function(x,y) {
  res <- strsplit(y,"")[[1]]
  setdiff(x,res)
}


remove_letters <- function(x,y){
  res <- strsplit(y,"")[[1]]
  
  for (i in seq_len(length(res))) {
    if(isTRUE(any(x[1] %in% res))) {
      x <- x[-1]
    }
  }
  x 
}


# remove_letters(c("s", "t", "r", "i", "n", "g", "w"), "string") ➞ ["w"]
# 
# remove_letters(c("b", "b", "l", "l", "g", "n", "o", "a", "w"), "balloon") ➞ ["b", "g", "w"]
# 
# remove_letters(c("d", "b", "t", "e", "a", "i"), "edabit") ➞ []
remove_letters(c("t", "t", "e", "s", "t", "u"), "testing")

lst <- c("t", "t", "e", "s", "t", "u")
str <- "testing"
str_split <- unlist(strsplit(str, ""))

# Remove each letter in the string from the list
for (char in str_split) {
  match_index <- match(char, lst)
  if (!is.na(match_index)) {
    lst <- lst[-match_index]
  }
}
match(str_split[[1]],lst)
