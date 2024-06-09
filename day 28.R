#The Bottom of the Matrix
lower_triang <-function(mt){
  new.mt <-matrix(0,ncol = ncol(mt),nrow = nrow(mt))
  
  for (i in seq_len(nrow(mt))) {
    for (j in seq_len(ncol(mt))) {
      if(i >= j){
        new.mt[i,j]<- mt[i,j]
      } else{
        new.mt[i,j]<- 0
      }
    }
  }
  new.mt
  
}

# lower_triang([
#   [1, 2, 3],
#   [4, 5, 6],
#   [7, 8, 9]
# ]) ➞ [
#   [1, 0, 0],
#   [4, 5, 0],
#   [7, 8, 9]
# ]
# 
# lower_triang([
#   [5, 7],
#   [7, 9]
# ]) ➞ [
#   [5, 0],
#   [7, 9]
# ]
# 
# lower_triang([
#   [1, 8, 8, 1],
#   [2, 7, 7, 2],
#   [3, 6, 6, 3],
#   [4, 5, 5, 4]
# ]) ➞ [
#   [1, 0, 0, 0],
#   [2, 7, 0, 0],
#   [3, 6, 6, 0],
#   [4, 5, 5, 4]
# ]
mt<-matrix(c(5,7,7,9),ncol = 2,nrow = 2,byrow = TRUE)


lower_triang(mt)

####### ####### ####### ####### ####### ####### ########### ####### HARD

###The Snake — Area Filling
snakefill <- function(n){
  snake.length <- 1
  squares.amount<- n * n 
  count <- 0
  
  while(snake.length<=squares.amount){
    count <- count+ 1 
    snake.length <- snake.length *2
  }
  count -1
}

# snakefill(3) ➞ 3
# 
# snakefill(6) ➞ 5
# 
# snakefill(24) ➞ 9
snakefill(900)
snakefill(555)
snakefill(8)
snakefill(2)
snakefill(1)



#Encode Morse
encode_morse <- function(x){
  x <- toupper(x)
  x.splited<-strsplit(x,"")[[1]]
  y<- char_to_dots[x.splited]
  paste0(y,collapse = " ") 
}

char_to_dots = c(
  'A'= '.-', 'B'= '-...', 'C'= '-.-.', 'D'= '-..', 'E'= '.', 'F'= '..-.',
  'G'= '--.', 'H'= '....', 'I'= '..', 'J'= '.---', 'K'= '-.-', 'L'= '.-..',
  'M'= '--', 'N'= '-.', 'O'= '---', 'P'= '.--.', 'Q'= '--.-', 'R'= '.-.',
  'S'= '...', 'T'= '-', 'U'= '..-', 'V'= '...-', 'W'= '.--', 'X'= '-..-',
  'Y'= '-.--', 'Z'= '--..', ' '= ' ', '0'= '-----',
  '1'= '.----', '2'= '..---', '3'= '...--', '4'= '....-', '5'= '.....',
  '6'= '-....', '7'= '--...', '8'= '---..', '9'= '----.',
  '&'= '.-...', "'"= '.----.', '@'= '.--.-.', ')'= '-.--.-', '('= '-.--.',
  ':'= '---...', ','= '--..--', '='= '-...-', '!'= '-.-.--', '.'= '.-.-.-',
  '-'= '-....-', '+'= '.-.-.', '"'= '.-..-.', '?'= '..--..', '/'= '-..-.'
)


#encode_morse("EDABBIT CHALLENGE") ➞ ". -.. .- -... -... .. -   -.-. .... .- .-.. .-.. . -. --. ."

#encode_morse("HELP ME !") ➞ ".... . .-.. .--.   -- .   -.-.--"




#Disarium Number

is_disarium <- function(n){
  n.splited <- strsplit(as.character(n),"")[[1]]
  f.n <-as.numeric(n.splited)
  result <- numeric(0)
  
  for (i in seq_len(length(n.splited))) {
    result[[length(result) + 1]] <- f.n[[i]]^i
    
  }
  sum(result) == n
}
# is_disarium(75) ➞ False
# # 7^1 + 5^2 = 7 + 25 = 32
# 
# is_disarium(135) ➞ True
# # 1^1 + 3^2 + 5^3 = 1 + 9 + 125 = 135
# 
# is_disarium(544) ➞ False
# 
# is_disarium(518) ➞ True
# 
# is_disarium(466) ➞ False
# 
# is_disarium(8) ➞ True

