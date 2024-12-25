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



# to_camel_case("A-B-C") ➞ "ABC"
# 
# to_camel_case("the-stealth-warrior") ➞ "theStealthWarrior"
# 
# to_camel_case("The_Stealth_Warrior") ➞ "TheStealthWarrior"