is_icecream_sandwich <- function(x){
  first <- regmatches(x,gregexpr("^[A-z]",x,perl = TRUE))[[1]]
  last <- regmatches(x,gregexpr("[A-z]$",x,perl = TRUE))[[1]]
  
  firstgroup <- regmatches(x,gregexpr(first,x,perl = TRUE))[[1]]
  secondgroup <- regmatches(x,gregexpr(paste0("[^",unique(firstgroup),"]",collapse = ""),x,perl = TRUE))[[1]]
  
  isTRUE(length(firstgroup) %% 2 ==0 & nchar(secondgroup) !=0 )
}


# is_icecream_sandwich("CDC") ➞ True
# 
# is_icecream_sandwich("AAABB") ➞ False
# 
# is_icecream_sandwich("AA") ➞ False

is_icecream_sandwich("BBBBB")
is_icecream_sandwich("AAACCCAA")


tweak_letters <- function(x,y){
  result<-strsplit(x,"")[[1]]
  final<- character(0)

  for (i in seq_along(result)) {
    
    if(result[[i]] %in% letters){
      final[[length(final) + 1]] <- letters[[which(result[[i]] == letters) +y[[i]] ]]
    }
  }
  
  paste(final,collapse = "")
}

# 
# tweak_letters("apple", c(0, 1, -1, 0, -1)) ➞ "aqold"
# # "p" + 1 => "q"; "p" - 1 => "o"; "e" - 1 => "d"
# 
# tweak_letters("many", c(0, 0, 0, -1)) ➞ "manx"
# 
# tweak_letters("rhino", c(1, 1, 1, 1, 1)) ➞ "sijop"


atbash <- function(x){
  result <-strsplit(x,"")[[1]]
  final <- character(0)
  for (i in seq_along(result)) {
    if(result[[i]] %in% letters){
      final[[length(final) + 1]] <-   letters[length(letters) - which(result[[i]] == letters) +1]
      
    }else if(result[[i]] %in% LETTERS){
      final[[length(final) + 1]] <-   LETTERS[length(LETTERS) - which(result[[i]] == LETTERS) +1]
      
    }else{
      final[[length(final) + 1]] <- result[[i]]
    } 
    
  }
  paste(final,collapse = "")
  
  
}
# 
# atbash("apple") ➞ "zkkov"
# 
# atbash("Hello world!") ➞ "Svool dliow!"
# 
# atbash("Christmas is the 25th of December") ➞ "Xsirhgnzh rh gsv 25gs lu Wvxvnyvi"
atbash("Vmxibkgrlm zmw wvxibkgrlm ziv rwvmgrxzo uli gsv Zgyzhs xrksvi.")








shift_sentence <- function(x){
  result <- strsplit(x," ")[[1]]
  
  final<-unlist(lapply(result,function(k){
    substr(k,1,1)
  }))
  
  final<-append(final,final[[length(final)]],after = 0)
  final<-final[-length(final)]
  
  regmatches(result,gregexpr("^[A-z]",result,perl = TRUE)) <- final
  paste(result,collapse = " ")
}



# shift_sentence("create a function") ➞ "freate c aunction"
# 
# shift_sentence("it should shift the sentence") ➞ "st ihould shift she tentence"
# 
# shift_sentence("the output is not very legible") ➞ "lhe tutput os iot nery vegible"
# 
# shift_sentence("edabit") ➞ "edabit"

