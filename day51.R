hangman <- function(x,y){
  pattern <- paste0("[^",paste0(y,toupper(y),collapse = ""),"\\s\\W]")
  regmatches(x,gregexpr(pattern,x,perl = TRUE))[[1]] <- "-"
  x
}



# hangman("helicopter", c("o", "e", "s")) ➞ "-e---o--e-"
# 
# hangman("tree", c("r", "t", "e")) ➞ "tree"
# 
# hangman("Python rules", c("a", "n", "p", "r", "z")) ➞ "P----n r----"
# 
# hangman("He's a very naughty boy!", c("e", "a", "y")) ➞ "-e"- a -e-y -a----y --y!"

hangman("Show me the money", c("a", "f", "u", "r", "q"))




is_correct_aliases <- function(x,y){
  final <-logical(0)
  
  for (i in seq_along(x)) {
    main.x<-substr(x[[i]],1,1)
    result<-strsplit(y[[i]]," ")[[1]]
    if(main.x == substr(result[[1]],1,1) && main.x == substr(result[[2]],1,1)){
      final[[length(final)+ 1]] <- TRUE
    }
  }
  sum(final) ==length(x)
}





# is_correct_aliases(c("Adrian M.", "Harriet S.", "Mandy T."), c("Amazing Artichoke", "Hopeful Hedgehog", "Marvelous Mouse")) ➞ True
# 
# is_correct_aliases(c("Rachel F.", "Pam G.", "Fred Z.", "Nancy K."), c("Reassuring Rat", "Peaceful Panda", "Fantastic Frog", "Notable Nickel")) ➞ True
# 
# is_correct_aliases(c("Beth T."), c("Brandishing Mimosa")) ➞ False
# # Both words in "Brandishing Mimosa" should begin with a "B" - "Brandishing Beaver" would do the trick.

is_correct_aliases(c('Bella T.', 'Tom H.', 'Ben C.'), c('Beautiful Barn', 'Talented Tapestry', 'Cool Bamboo'))


chunkify <- function(x,y){
  split(x, ceiling(seq_along(x) / y))
  
}

# chunkify(c(2, 3, 4, 5), 2) ➞ list([2, 3], [4, 5])
# 
# chunkify([2, 3, 4, 5, 6], 2) ➞ [[2, 3], [4, 5], [6]]
# 
# chunkify(c(2, 3, 4, 5, 6, 7), 3) ➞ [[2, 3, 4], [5, 6, 7]]
# 
# chunkify([2, 3, 4, 5, 6, 7], 1) ➞ [[2], [3], [4], [5], [6], [7]]
# 
# chunkify([2, 3, 4, 5, 6, 7], 7) ➞ [[2, 3, 4, 5, 6, 7]]



possible_palindrome <- function(x){
  result<-table(strsplit(x,"")[[1]])
  if(all(result == 1))return(FALSE)
  if(all(result %% 2 ==1)) return(TRUE)
  if(sum(result %% 2 ==1) == 1) return(TRUE)
  return(FALSE)
  
}

# possible_palindrome("acabbaa") ➞ True
# # Can make the following palindrome: "aabcbaa"
# 
# possible_palindrome("aacbdbc") ➞ True
# # Can make the following palindrome: "abcdcba"
# 
# possible_palindrome("aacbdb") ➞ False
# 
# possible_palindrome("abacbb") ➞ False
possible_palindrome("ab")
possible_palindrome("abc")
possible_palindrome("bbaacd")



dial <- function(x){
  
  my_vector <- c("abc" = 2, "def" = 3, "ghi" = 4, "jkl" = 5, "mno" = 6, "pqrs" = 7, "tuv" = 8, "wxyz" = 9)
  
  for (i in seq_along(my_vector)) {
    y<-names(my_vector)[[i]]
    pattern <- paste0("[",paste0(y,toupper(y)),"]")
    regmatches(x,gregexpr(pattern,x,perl = TRUE)) <- my_vector[[i]]
    
    
  }
  
  x
}

# abc  = 2
# def  = 3
# ghi  = 4
# jkl  = 5
# mno  = 6
# pqrs = 7
# tuv  = 8
# wxyz = 9



# dial("1-800-HOTLINEBLING") ➞ "1-800-468546325464"
# 
# dial("abc-def-ghi-jkl!!") ➞ "222-333-444-555!!"
# 
# dial("adgjmptw :)") ➞ "23456789 :)"

mat.multiplier <- function(x,y){
  if(ncol(x) != nrow(y)) stop("error")
  if(nrow(x) != ncol(y)) stop("error")
  
  x %*%y
}


matrix1 <- matrix(c(1, 2, 3, 
                    4, 5, 6, 
                    7, 8, 9), nrow = 3, byrow = TRUE)

matrix2 <- matrix(c(1, 2, 3, 
                    4, 5, 6, 
                    7, 8, 9), nrow = 3, byrow = TRUE)


mat.multiplier(matrix1,matrix2)








make_box <- function(x){
  mt <- matrix(numeric(0),ncol = 1,nrow = x)
  
  for (i in seq_len(x)) {
    if(i == 1 || i == x){
      mt[i,1]<-paste0(rep("#",x),collapse = "")
    }else{
      mt[i,1]<-paste0("#",paste0(rep(" ",x-2),collapse = ""),"#",collapse = "")
      
    }  
  }
  mt
}
# make_box(5) ➞ [
#   "#####",
#   "#   #",
#   "#   #",
#   "#   #",
#   "#####"
# ]
# 
# make_box(3) ➞ [
#   "###",
#   "# #",
#   "###"
# ]
# 
# make_box(2) ➞ [
#   "##",
#   "##"
# ]
# 
# make_box(1) ➞ [
#   "#"
# ]

letts <- function(x,y){
  x.main<-strsplit(x,"")[[1]]
  
  y.main<-strsplit(y,"")[[1]]
  
  final <-intersect(x.main,y.main)
  final <-paste0(sort(final),collapse = "")
  
  
  pattern.x<-paste0("[^",y,"]",collapse = "")
  m.x<-regmatches(x,gregexpr(pattern.x,x,perl = TRUE))[[1]]
  
  final.1<-paste0(sort(unique(m.x)),collapse = "")
  
  
  
  pattern.y<-paste0("[^",x,"]",collapse = "")
  m.y<-regmatches(y,gregexpr(pattern.y,y,perl = TRUE))[[1]]
  final.2<-paste0(sort(unique(m.y)),collapse = "")
  
  list(final,final.1,final.2)
  
  
}


# letts("sharp", "soap") ➞ ["aps", "hr", "o"]
# 
# letts("board", "bored") ➞ ["bdor", "a", "e"]
# 
# letts("happiness", "envelope") ➞ ["enp", "ahis", "lov"]
# 
# letts("kerfuffle", "fluffy") ➞ ["flu", "ekr", "y"]
# # Even with multiple matching letters (e.g. 3 f's), there should 
# # only exist a single "f" in your first element.
# 
# letts("match", "ham") ➞ ["ahm", "ct", ""]
# # "ham" does not contain any letters that are not found already 
# # in "match".





ones_threes_nines <- function(x){
  
  
  result.1<-logical(0)
  result.2<-logical(0)
  result.3<-logical(0)
  
  
  while (x > 0) {
    
    if(x > 9){
      x<-x- 9
      result.1[[length(result.1) + 1]] <- TRUE
    }else{
      if(x >= 3){
        x<-x- 3
        result.2[[length(result.2) + 1]] <- TRUE
      }else{
        x<-x- 1
        result.3[[length(result.3) + 1]] <- TRUE
        
      }
    }
    
    
  }
  paste(paste0("nines:",sum(result.1),collapse = ""),paste0("threes:",sum(result.2),collapse = ""),paste0("ones:",sum(result.3),collapse = "")
        ,collapse = ",",sep = ", ")
  
  
}


# ones_threes_nines(10) ➞ "nines:1, threes:0, ones:1"
# 
# ones_threes_nines(15) ➞ "nines:1, threes:2, ones:0"
# 
# ones_threes_nines(22) ➞ "nines:2, threes:1, ones:1"

ones_threes_nines(25)


#This function I already did before

# generate_hashtag("    Hello     World   " ) ➞ "#HelloWorld"
# 
# generate_hashtag("") ➞ false, "Expected an empty string to return false"
# 
# generate_hashtag("Edabit Is Great") ➞ "#EdabitIsGreat", "Should remove spaces."