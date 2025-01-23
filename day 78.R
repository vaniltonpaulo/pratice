calculate_fuel <- function(x) {
  ifelse(x* 10 > 100, x*10, 100)
}

# calculate_fuel(15) ➞ 150
# 
# calculate_fuel(23.5) ➞ 235
# 
# calculate_fuel(3) ➞ 100

flip <-  function(x) {
  ifelse(x== 1, 0,1)
}

# flip(1) ➞ 0
# 
# flip(0) ➞ 1


# say_hello_bye("alon", 1) ➞ "Hello Alon"
# 
# say_hello_bye("Tomi", 0) ➞ "Bye Tomi"
# 
# say_hello_bye("jose", 0) ➞ "Bye Jose"