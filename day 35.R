extend_vowels <- function(x,num){
  if(num == 0) return(x)
  if( num<0 || num != floor(num)) return("invalid")
  
  k<-regmatches(x,gregexpr("([aeiouAEIOU])",x,perl = TRUE))[[1]] 
  if(length(k) == 0) return(x)
  vowels<-(lapply(k,rep,num))
  final <-lapply(vowels,function(j){
    paste0(j,collapse = "")
  })
  
  regmatches(x,gregexpr("[aeiouAEIOU]",x,perl = TRUE))[[1]]  <- unlist(final)
  return(x)
}


# extend_vowels("Hello", 5) ➞ "Heeeeeelloooooo"
# 
# extend_vowels("Edabit", 3) ➞ "EEEEdaaaabiiiit"
# 
# extend_vowels("Extend", 0) ➞ "Extend"
extend_vowels("Nice", -8)
extend_vowels("A", 10)
extend_vowels("Z", 93)
extend_vowels("Vowel", 0.5)





letters_only <- function(x){
  if(x== "") return(FALSE)
  codebook <- c(letters," ")
  y<- strsplit(x,"")[[1]]
  final <-y %in% codebook
  
  return(all(final))
}

# letters_only("PYTHON") ➞ False
# 
# letters_only("python") ➞ True
# 
# letters_only("12321313") ➞ False
# 
# letters_only("i have spaces") ➞ True
# 
# letters_only("i have numbers(1-10)") ➞ False
# 
# letters_only("") ➞ False

