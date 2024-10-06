############# Medium start Algos

mt <- matrix(c(0, 1, 0, 0,
                        1, 0, 1, 1,
                        0, 1, 0, 1,
                        0, 1, 1, 0), 
                      nrow = 4, byrow = TRUE)

mt[1,2]

is_adjacent <- function(mt,x,y){
  if(mt[x,y] == 1) return(TRUE)
  FALSE
}


is_adjacent(mt, 3, 2)
is_adjacent(mt, 1, 3)


solutions <- function(a,b,c){
  if((b^2 - 4 * a * c) == 0) return(1)
  if((b^2 - 4 * a * c) >0) return(2)
  if((b^2 - 4 * a * c) <0) return(0)
  
}

# solutions(1, 0, -1) ➞ 2
# // x² - 1 = 0 has two solutions (x = 1 and x = -1).
# 
# solutions(1, 0, 0) ➞ 1
# // x² = 0 has one solution (x = 0).
# 
# solutions(1, 0, 1) ➞ 0
# // x² + 1 = 0 has no real solutions.
solutions(1000, 1000, 0)
solutions(10000, 400, 4)



sum_fractions <-  function(x){
  result<-lapply(x,function(k){
    k[[1]]/k[[2]]
  })
  round(sum(unlist(result)),0)
}



# sum_fractions(list(list(18, 13), list(4, 5))) ➞ 2
# 
# sum_fractions([[36, 4], [22, 60]]) ➞ 9
# 
# sum_fractions(list(list(11, 2), list(3, 4), list(5, 4), list(21, 11), list(12, 6))) ➞ 11



age_difference <- function(x,y){
  

  x <- c(x, 2* y)

max(x) - min(x)

}
# age_difference(36, 7) ➞ 22
# # 22 years from now, the father will be 58 years old and his son will be 29 years old.
# 
# age_difference(55, 30) ➞ 5
# # 5 years ago, the father was 50 years old and his son was 25 years old.
# 
# age_difference(42, 21) ➞ 0



dna_to_rna <- function(x){
  chartr("ATGC","UACG",x)
}

# dna_to_rna("ATTAGCGCGATATACGCGTAC") ➞ "UAAUCGCGCUAUAUGCGCAUG"
# 
# dna_to_rna("CGATATA") ➞ "GCUAUAU"
# 
# dna_to_rna("GTCATACGACGTA") ➞ "CAGUAUGCUGCAU"




is_sastry <- function(x){
  result<-x+1
  final<-paste0(x,result,collapse = "")
  sqrt(as.numeric(final)) %% 2 == 0
}



# is_sastry(183) ➞ True
# # Concatenation of n and its successor = 183184
# # 183184 is a perfect square (428 ^ 2)
# 
# is_sastry(184) ➞ False
# # Concatenation of n and its successor = 184185
# # 184185 is not a perfect square
# 
# is_sastry(106755) ➞ True
# # Concatenation of n and its successor = 106755106756
# # 106755106756 is a perfect square (326734 ^ 2)



to_camel_case <- function(x){
  result <- strsplit(x,"_")[[1]]
  main <- result[2: length(result)]
  final<-vapply(main, function(k){
    vowel.k<-substr(k,1,1)
    sub(vowel.k,toupper(vowel.k),k)
  },character(1))
  final <- c(result[[1]],final)
  return(paste0(final,collapse = ''))
}


to_snake_case <- function(x){
  gsub("([A-Z])","_\\1",x,perl = TRUE)
}



# to_camel_case("hello_edabit") ➞ "helloEdabit"
# 
# to_snake_case("helloEdabit") ➞ "hello_edabit"
# 
# to_camel_case("is_modal_open") ➞ "isModalOpen"
# 
# to_snake_case("getColor") ➞ "get_color"
to_snake_case("getBackgroundColor")
to_camel_case("get_background_color")




ABA <- function(x){
  y<-"A"
  n <-1
  result <- character(0)
  repeat{
    result<-paste0(result,y,result,collapse = "")
    if(n == (which(x == LETTERS))){
      break
    }
    y <- LETTERS[which(y == LETTERS) +1]
    n <- n+1
  }
  result
}



# A ➞ **A**
#   B ➞ A**B**A
# C ➞ ABA**C**ABA
# D ➞ ABACABA**D**ABACABA
# E ➞ ABACABADABACABA**E**ABACABADABACABA
# F ➞ ABACABADABACABAEABACABADABACABA**F**ABACABADABACABAEABACABADABACABA

# And so on ...



# ABA("A") ➞ "A"
# 
# ABA("B") ➞ "ABA"
# 
# ABA("E") ➞ "ABACABADABACABAEABACABADABACABA"



snakefill <- function(x){
 
  
  board <- x * x
  result <- numeric(0)
  
  for (i in seq_len(board)) {
    if((2^i) >= board){
      result <- i -1
      break
    }
  }
  result
}

# snakefill(3) ➞ 3
# 
# snakefill(6) ➞ 5
# 
# snakefill(24) ➞ 9


encrypt <- function(x){
  paste0(chartr("aeiou","01223",x),"aca",collapse = "")
}
# 
# encrypt("banana") ➞ "0n0n0baca"
# 
# encrypt("karaca") ➞ "0c0r0kaca"
# 
# encrypt("burak") ➞ "k0r3baca"
# 
# encrypt("alpaca") ➞ "0c0pl0aca"

uncensor <- function(x,y){
  if(y == "") return(x)
  main <- strsplit(y,"")[[1]]
  n <- 1
  repeat{
    x<-sub("\\*",main[[n]],x,perl = TRUE)
    if(n == nchar(y)){
      break
    }
    n<- n+ 1
  }
  x
}

# uncensor("Wh*r* d*d my v*w*ls g*?", "eeioeo") ➞ "Where did my vowels go?"
# 
# uncensor("abcd", "") ➞ "abcd"
# 
# uncensor("*PP*RC*S*", "UEAE") ➞ "UPPERCASE"

uncensor('Ch**s*, Gr*mm*t -- ch**s*', 'eeeoieee')



sock_merchant <- function(x){
  if(0 ==length(x)) return(0)
  num<-x[duplicated(x)]
  main<-unique(num)
  
  result <- numeric(0)
  
  for (i in seq_len(length(main))) {
    if(sum(main[i] == x) %% 2 ==0){
      result[[length(result)+ 1]] <- sum(main[i] == x)
      
    } else{
      result[[length(result)+ 1]] <- sum(main[i] == x) -1
    }
  }
  
  sum(result /2)
}


# sock_merchant(c(10, 20, 20, 10, 10, 30, 50, 10, 20)) ➞ 3
# 
# sock_merchant(c(50, 20, 30, 90, 30, 20, 50, 20, 90)) ➞ 4
# 
# sock_merchant(c()) ➞ 0
sock_merchant(c(50, 40, 30, 100, 60, 65, 90, 80, 10))


