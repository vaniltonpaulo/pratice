#Words With Duplicate Letters

no_duplicate_letters <- function(x){
  x.splited <- strsplit(x," ")[[1]]
 
  
  check.duplicated <- function(k){
    return(any(duplicated(strsplit(k,"")[[1]]) == TRUE))
  }
  
  result <-vapply(x.splited,check.duplicated,logical(1))
  
  if(any(result == TRUE)) return(FALSE)
  
  return(TRUE)
}



# no_duplicate_letters("Fortune favours the bold.") ➞ True
# 
# no_duplicate_letters("You can lead a horse to water, but you can't make him drink.") ➞ True
# 
# no_duplicate_letters("Look before you leap.") ➞ False
# # Duplicate letters in "Look" and "before".
# 
# no_duplicate_letters("An apple a day keeps the doctor away.") ➞ False
# # Duplicate letters in "apple", "keeps", "doctor", and "away".
no_duplicate_letters("Time flies when you're having fun.")


#Vowel to Vowel Links

vowel_links <- function(x){
  vowels <- c("a","e","i","o","u")
  x.splited <-strsplit(x," ")[[1]]
  result <- logical(0)
  for (i in seq_len(length(x.splited) -1)) {
    if( substring(x.splited[[i]],nchar(x.splited[[i]]),nchar(x.splited[[i]])) %in% vowels &&   substring(x.splited[[i+ 1]],1,1) %in% vowels){
      result[[length(result) + 1]] <-TRUE
    } else {
      result[[length(result) + 1]] <-FALSE
    }
  }
  
  any(result == TRUE)
}


# vowel_links("a very large appliance") ➞ True
# 
# vowel_links("go to edabit") ➞ True
# 
# vowel_links("an open fire") ➞ False
# 
# vowel_links("a sudden applause") ➞ False




#Flip the Array

flip_list <- function(l){
  if(any(unlist(lapply(l,is.list)) == FALSE)){
   return(lapply(l,as.list))
  }else{
    return(lapply(l,unlist))
  }
}



# flip_list(list(1, 2, 3, 4)) ➞ [[1], [2], [3], [4]]
# # Take a horizontal list and flip it vertical.
# 
# flip_list(list(list(5), list(6), list(9))) ➞ [5, 6, 9]
# # Take a vertical list and flip it horizontal.
# 
# flip_list(list()) ➞ []
flip_list(list(2))


#Next Prime
next_prime <- function(n){
  
  check.prime <- function(n){
    result <- logical(0)
    for (i in seq_len(n)[-1]) {
      if(n %% i ==0){
        result[[length(result)+1]] <- TRUE
      } else{
        result[[length(result)+1]] <- FALSE
      }
      
    }
    sum(result) == 1
  }
  
  repeat{
    if(check.prime(n) == TRUE) break
    n <- n+ 1
    
  }
  n
}

# next_prime(12) ➞ 13
# 
# next_prime(24) ➞ 29
# 
# next_prime(11) ➞ 11
# # 11 is a prime, so we return the number itself.




#Tall People
