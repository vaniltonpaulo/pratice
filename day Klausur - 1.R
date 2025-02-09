isMaxReallyLarge <-  function(x) {
  assertNumeric(x,any.missing = FALSE, min.len = 2)
  result <- max(x)
  final <- 2 * x[x != result]
  any( result > final) 
}


isMaxReallyLarge(c(1, 2, 10))
#> [1] TRUE
isMaxReallyLarge(c(1, 2, 3))
#> [1] FALSE
#> 
# -1 is more than 2 times -2 and more than 2 times -3
isMaxReallyLarge(c(-1, -2, -3))
#> [1] TRUE
#> 
# 2 is not *more than* 2x 1
isMaxReallyLarge(c(1, 2))
#> [1] FALSE
#> 


isSorted <-  function(x) {
  assertNumeric(x,any.missing = FALSE)
  decre <- sort(x, decreasing = TRUE)
  incre <- sort(x, decreasing = FALSE)
  ifelse(all(x == decre) || all(x == incre), TRUE, FALSE)
}


isSorted(c(1, 2, 3))
#> [1] TRUE
isSorted(c(3, 2, 1))
#> [1] TRUE
isSorted(c(1, 1, 1))
#> [1] TRUE
isSorted(c(1, 1, 1, 2))
#> [1] TRUE
isSorted(c(1, 1, 1, -2))
#> [1] TRUE
isSorted(c(2, 1, 1, 1, 2))
#> [1] FALSE
isSorted(c(1, 3, 2))
#> [1] FALSE
isSorted(0)
#> [1] TRUE
isSorted(numeric(0))
#> [1] TRUE


matrixMaxes <-  function(x) {
  assertMatrix(x, mode = "numeric", any.missing = FALSE)
  mat <- matrix(0, nrow = nrow(x), ncol = ncol(x))
  
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      mat[i,j] <-  min(max(x[i,]), max(x[,j]))
      
    }
    
  }
  mat
  
}

m1 <- matrix(nrow = 3, byrow = TRUE, c(
  11, 3, 16, 10,
  6, 0, 6, 14,
  10, 5, 19, 14)
)


matrixMaxes(m1)
#> [,1] [,2] [,3] [,4]
#> [1,] 11 5 16 14
#> [2,] 11 5 14 14
#> [3,] 11 5 19 14
## Explanation:
## The column-maxima of 'm1' are 11,5, 19, 14. The row-maxima are
## 16, 14 and 19. Increasing any value more than in the depicted
## solution in the 1st, 2nd or 4th column would give a greater
## column-maximum. The values in the 3rd column are limited by
## the row-wise maxima instead.
m2 <- matrix(nrow = 2, 1:4)
matrixMaxes(m2)
#> [,1] [,2]
#> [1,] 2 3
#> [2,] 2 4
m3 <- matrix(nrow = 2, c(1, 2, 2, 1))
matrixMaxes(m3)
#> [,1] [,2]
#> [1,] 2 2
#> [2,] 2 2
matrixMaxes(matrix(1))
#> [,1]
#> [1,] 1
# empty matrix -> empty matrix
matrixMaxes(matrix(numeric(0)))
#> [,1]
matrixMaxes(matrix(numeric(0), nrow = 0, ncol = 0))
#> <0 x 0 matrix>
#> 
#> 
#> 

m1 <- matrix(nrow = 3, byrow = TRUE, c(
  11, 3, 16, 10,
  6, 0, 6, 14,
  10, 5, 19, 14)
)


matrixMaxes(m1)
#> [,1] [,2] [,3] [,4]
#> [1,] 11 5 16 14
#> [2,] 11 5 14 14
#> [3,] 11 5 19 14
## Explanation:
## The column-maxima of 'm1' are 11,5, 19, 14. The row-maxima are
## 16, 14 and 19. Increasing any value more than in the depicted
## solution in the 1st, 2nd or 4th column would give a greater
## column-maximum. The values in the 3rd column are limited by
## the row-wise maxima instead.

rngManip <-  function(x) {
  assertFunction(x)
  n <-  1
  repeat{
    set.seed(n)
    if(all(replicate(10, x()) == rep("H", 10))) {
      return(set.seed(n))
      break
    }
    
    n <- n + 1
  }
}




f1 <- function() ifelse(runif(1) < 0.5, "H", "T")
set.seed(1)
replicate(10, f1())
#> [1] "H" "H" "T" "T" "H" "T" "T" "T" "T" "H"
rngManip(f1)
replicate(10, f1())
#> [1] "H" "H" "H" "H" "H" "H" "H" "H" "H" "H"
f2 <- function() sample(c("H", "T"), 1)
rngManip(f2)
replicate(10, f2())
#> [1] "H" "H" "H" "H" "H" "H" "H" "H" "H" "H"
f3 <- function() ifelse(diff(runif(2)) > 0, "H", "T")
rngManip(f3)
replicate(10, f3())
#> [1] "H" "H" "H" "H" "H" "H" "H" "H" "H" "H"
#> 
#> 
#> 
#> 






makeErrorsRed <-  function(x) {
  assertFunction(x)
  result = tryCatch({
    x()
  }, error = function(e) {
    stop(paste0("\033[41m\033[97m",conditionMessage(e),"\033[0m",collapse = ""))
  
  })
 result
  
}

#stop("\033[41m\033[97mThis is an errorcool.\033[0m")




f2 <- function() runif()
#f2()
#> Error in runif() : argument "n" is missing, with no default
makeErrorsRed(f2)
#> Error in ***************** :
#> argument "n" is missing, with no default
#### the ***************** part can be anything
#### the 'argument "n" is missing, with no default' should be red with white background.











f1 <- function() runif(1)
set.seed(1)
f1()
#> [1] 0.2655087
set.seed(1)
makeErrorsRed(f1)
#> [1] 0.2655087
f2 <- function() runif()
f2()
#> Error in runif() : argument "n" is missing, with no default
makeErrorsRed(f2)
#> Error in ***************** :
#> argument "n" is missing, with no default
#### the ***************** part can be anything
#### the 'argument "n" is missing, with no default' should be red with white background.
f3 <- function() try(runif(), silent = TRUE)
x <- f3()
x
#> [1] "Error in runif() : argument \"n\" is missing, with no default\n"
#> attr(,"class")
#> [1] "try-error"
#> attr(,"condition")
#> <simpleError in runif(): argument "n" is missing, with no default>
x <- makeErrorsRed(f3)
x
#> [1] "Error in runif() : argument \"n\" is missing, with no default\n"
#> attr(,"class")
#> [1] "try-error"
#> attr(,"condition")


textToMorse  <- function(x) {
  assertString(x)
  x <- toupper(x)
  x <- gsub("\\s+","",x)
  result <- strsplit(x,"")[[1]]
  if(!all(result %in% names(table.morse))) stop("could not convert the text")
  final <- table.morse[match(result,names(table.morse))]
  paste(final,collapse = " ")
}

table.morse <- c(
  A = ".-", B = "-...", C = "-.-.", D = "-..", E = ".", F = "..-.", G = "--.", H = "....",
  I = "..", J = ".---", K = "-.-", L = ".-..", M = "--", N = "-.", O = "---", P = ".--.",
  Q = "--.-", R = ".-.", S = "...", T = "-", U = "..-", V = "...-", W = ".--", X = "-..-",
  Y = "-.--", Z = "--..",
  `0` = "-----", `1` = ".----", `2` = "..---", `3` = "...--", `4` = "....-",
  `5` = ".....", `6` = "-....", `7` = "--...", `8` = "---..", `9` = "----.",
  `.` = ".-.-.-", `,` = "--..--", `?` = "..--.."
)



textToMorse("Morse code") == "-- --- .-. ... . -.-. --- -.. ."
#> [1] "-- --- .-. ... . -.-. --- -.. ."
textToMorse("1 23 4") == ".---- ..--- ...-- ....-"
#> [1] ".---- ..--- ...-- ....-"
# check correct spacing:
textToMorse(" 1? 2. 12 3") == ".---- ..--.. ..--- .-.-.- .---- ..--- ...--"
#> [1] ".---- ..--.. ..--- .-.-.- .---- ..--- ...--"
#> 
identical(textToMorse("MORSE. CODE"), textToMorse("MORSE.CODE"))
#> [1] TRUE
identical(textToMorse("MORSE. CODE"), textToMorse("morse. CoDe"))
#> [1] TRUE
# no code for exclamation mark:
textToMorse("Morse code!")
#> Error in textToMorse("Morse code!") : could not convert the text
#> 
#> 
