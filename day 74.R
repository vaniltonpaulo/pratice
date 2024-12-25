make_happy <- function(x) {
  regmatches(x,gregexpr("(?<=\\:|\\;)\\(",x,perl = TRUE))[[1]] <- ")"
  x
}



# make_happy("My current mood: :(") ➞ "My current mood: :)"
# 
# make_happy("I was hungry 8(") ➞ "I was hungry 8)"
# 
# make_happy("print('x(')") ➞ "print('x)')"
make_happy("I'm thirsty ;(")


count_smileys <- function(x) {
  sum(grepl("(?<=\\:|\\;).*(\\)|D)",x,perl = TRUE))
}

# count_smileys(c(":)", ";(", ";}", ":-D")) ➞ 2
# 
# count_smileys(c(";D", ":-(", ":-)", ";~)")) ➞ 3
# 
# count_smileys(c(";]", ":[", ";*", ":$", ";-D")) ➞ 1

to_camel_case <- function(x) {
  result <- strsplit(x,"(\\-|\\_)")[[1]]
  
  final <- lapply(result, function(k){
    paste0(toupper(substr(k,1,1)),substr(k,2,nchar(k)), collapse = "")
    
  })
  
  paste0(unlist(final),collapse = "")  
}

# to_camel_case("A-B-C") ➞ "ABC"
# 
# to_camel_case("the-stealth-warrior") ➞ "theStealthWarrior"
# 
# to_camel_case("The_Stealth_Warrior") ➞ "TheStealthWarrior"



tweet <- function(x) {
  result <- regmatches(x,gregexpr("(\\@|\\#)[A-z]+",x,perl = TRUE))[[1]]
  paste(result,collapse = " ")
}

# tweet("Visit us at @edabit") ➞ "@edabit"
# 
# tweet("Follow @JavaScript") ➞ "@JavaScript"
# 
# tweet("#Honesty is the best @policy!!") ➞ "#Honesty @policy"

tweet('@RonaldRoss was awarded the Nobel Prize for his work on the transmission of #malaria.')


split_and_delimit <- function(x,z,y){
  split_string_equal_parts <- function(text, part_length) {
    # Initialize an empty vector to store the parts
    parts <- character(0)
    
    # Calculate the total number of parts
    n <- ceiling(nchar(text) / part_length)
    
    # Loop through each part and extract the substring
    for (i in 1:n) {
      start <- (i - 1) * part_length + 1
      end <- min(i * part_length, nchar(text))
      parts <- c(parts, substring(text, start, end))
    }
    
    return(parts)
  }
  
  
  result <- split_string_equal_parts(x,z)
  
  final<-paste0(result,collapse = " ")
  gsub(" ",paste0("\\",y,collapse = ""),final)
  
}

# split_and_delimit("bellow", 2, "&") ➞ "be&ll&ow"
# 
# split_and_delimit("magnify", 3, ":") ➞ "mag:nif:y"
# 
# split_and_delimit("poisonous", 2, "~") ➞ "po~is~on~ou~s"

num_in_str <- function(x) {
  result <- grep("[0-9]+",x,value = TRUE,perl = TRUE)
  grep("[A-z]+",result,value = TRUE,perl = TRUE)  
}

# num_in_str(c("1a", "a", "2b", "b")) ➞ ["1a", "2b"]
# 
# num_in_str(c("abc", "abc10")) ➞ ["abc10"]
# 
# num_in_str(c("abc", "ab10c", "a10bc", "bcd")) ➞ ["ab10c", "a10bc"]
# 
# num_in_str(c("this is a test", "test1")) ➞ ["test1"]


