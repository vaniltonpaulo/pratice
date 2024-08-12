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



first_n_vowels <- function(x,n){
  y<-strsplit(x,"")[[1]]
  k<- regmatches(x,gregexpr('[aeiou]',x,perl = TRUE))[[1]]
  if(length(k) < n) return("invalid")
  paste0(k[1:n],collapse = "")
}



# first_n_vowels("sharpening skills", 3) ➞ "aei"
# 
# first_n_vowels("major league", 5) ➞ "aoeau"
# 
# first_n_vowels("hostess", 5) ➞ "invalid"
first_n_vowels("shrimpy", 2)
first_n_vowels("crabby patty", 2)




lst = c("tall height", "tall height", "short height", "medium height", "tall height")
pattern = "yourregularexpressionhere"

#len(re.findall(pattern, ", ".join(lst))) ➞ 3


sum(grepl("(?<=tall)",lst,perl = TRUE))


txt = "(214) 987-6482"
txt = "2020-04-18"

regmatches(txt,gregexpr("(?<area>[0-9]{3})",txt,perl = TRUE))[[1]][[1]]

k<- regmatches(txt,gregexec("(?<year>[0-9]{4})-(?<month>[0-9]{2})-(?<day>[0-9]{2})",txt,perl = TRUE))[[1]]

k["year",][[1]]
k["month",][[1]]
k["day",][[1]]
