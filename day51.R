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


