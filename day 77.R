get_missing_letters <- function(x) {
  x <- strsplit(x, "")[[1]]
  paste0(setdiff(letters, x), collapse = "")
  
}


# get_missing_letters("abcdefgpqrstuvwxyz") ➞ "hijklmno"
# 
# get_missing_letters("zyxwvutsrq") ➞ "abcdefghijklmnop"
# 
# get_missing_letters("abc") ➞ "defghijklmnopqrstuvwxyz"
# 
# get_missing_letters("abcdefghijklmnopqrstuvwxyz") ➞ ""



censor <- function(x) {
  result <- strsplit(x," ")[[1]]
  final <- character(0)
  for (i in seq_len(length(result))) {
    if(nchar(result[[i]]) > 4){
      final[[length(final) + 1]] <- paste0(rep("*",nchar(result[[i]]) ),collapse = "")
    } else {
      final[[length(final) + 1]] <- result[[i]]
    }
  }
  paste0(final,collapse = " ")
  
}

# censor("The code is fourty") ➞ "The code is ******"
# 
# censor("Two plus three is five") ➞ "Two plus ***** is five"
# 
# censor("aaaa aaaaa 1234 12345") ➞ "aaaa ***** 1234 *****"

digit_occurrences <- function(x,y,z) {
  item <- paste0("[",z,"]",collapse = "")
  result <- seq(x, y,1)
  final<-paste0(result,collapse = "")[[1]]
  length(regmatches(final,gregexpr(item,final,perl = TRUE))[[1]])
  
}

# digit_occurrences(51, 55, 5) ➞ 6
# # [51, 52, 53, 54, 55] : 5 occurs 6 times
# 
# digit_occurrences(1, 8, 9) ➞ 0
# 
# digit_occurrences(-8, -1, 8) ➞ 1
# 
# digit_occurrences(71, 77, 2) ➞ 1


countdown <- function(x, y) {
  x<-x:1
  y <- paste0(y,"!",collapse = "")
  paste(paste0(x,".",collapse = " "),y,sep = " ")
  
}

# countdown(10, "Blast Off") ➞ "10. 9. 8. 7. 6. 5. 4. 3. 2. 1. BLAST OFF!"
# 
# countdown(3, "go") ➞ "3. 2. 1. GO!"
# 
# countdown(5, "FIRE") ➞ "5. 4. 3. 2. 1. FIRE!"

wave <- function(x) {
  result <- strsplit(x,"")[[1]]
  n <- 1
  final <- character(0)
  
  repeat{
    
    if(n <= nchar(x)) {
      result[[n]] <- toupper(result[[n]])
      final[[length(final) + 1]] <- paste0(result,collapse = "")
      result <- tolower(result)
      n <- n + 1
      
    }else {
      break
    }
    
    
  }
  
  final
  
}

# wave("edabit") ➞ ["Edabit", "eDabit", "edAbit", "edaBit", "edabIt", "edabiT"]
# 
# wave("just do it") ➞ ["Just do it", "jUst do it", "juSt do it", "jusT do it", "just Do it", "just dO it", "just do It", "just do iT"]
# 
# wave(" ") ➞ []

