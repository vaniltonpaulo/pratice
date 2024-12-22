letter_check <- function(x){
  x <- tolower(x)
  
  result <-strsplit(x[[2]], "")[[1]]
  final <- strsplit(x[[1]],"")[[1]]
  
  isTRUE(all(result %in% final))
}
# letter_check(c("trances", "nectar")) ➞ True
# 
# letter_check(c("compadres", "DRAPES")) ➞ True
# 
# letter_check(c("parses", "parsecs")) ➞ False

reverse.123 <- function(x){
  result <-strsplit(x,"\\s+")[[1]]
  soo<-lapply(result, function(k){
    if(nchar(k) >= 5){
      final<-strsplit(k,"")[[1]]
      almost<- rev(final)
      paste0(almost,collapse = "")
    }else{
      k
    }
  })
  paste(soo, collapse = " ")
}


get_middle <- function(x) {
  y <- strsplit(x,"")[[1]]
  result <- nchar(x)
  imp <- result/2
  ifelse(result %% 2 == 0,paste0(y[[imp]],y[[imp + 1]],collapse = "") ,y[[imp + 1]])
}

# get_middle("test") ➞ "es"
# 
# get_middle("testing") ➞ "t"
# 
# get_middle("middle") ➞ "dd"
# 
# get_middle("A") ➞ "A"
get_middle("cabinet")



retrieve <- function(x){
  x <- tolower(x)
  regmatches(x,gregexpr("(?<![bcdfghjklmnpqrstvwxyzaeiou])[aeiou][a-z]*",x,perl = TRUE))[[1]]
}


# retrieve("A simple life is a happy life for me.") ➞ ["a", "is", "a"]
# 
# retrieve("Exercising is a healthy way to burn off energy.")
# ➞ ["exercising", "is", "a", "off", "energy"]
# 
# retrieve("The poor ostrich was ostracized.")
# ➞ ["ostrich", "ostracized"]
# 
# retrieve("")
# ➞ []


remove_special_characters <- function(x) {
  result <- regmatches(x,gregexpr("[^\\.\\!@#\\$%\\^\\&\\*\\(\\)]",x,perl = TRUE))[[1]]
  paste0(result,collapse = "")
}
# remove_special_characters("The quick brown fox!") ➞ "The quick brown fox"
# 
# remove_special_characters("%fd76$fd(-)6GvKlO.") ➞ "fd76fd-6GvKlO"
# 
# remove_special_characters("D0n$c sed 0di0 du1") ➞ "D0nc sed 0di0 du1"


grab_city <- function(x) {
  result <-regmatches(x,gregexpr("(?<=\\s)\\[[A-z]+\\]",x,perl = TRUE))[[1]]
  regmatches(result,gregexpr("(?<=\\[)[A-z]+(?=\\])",result,perl = TRUE))[[1]] 
}

# grab_city("[Last Day!] Beer Festival [Munich]") ➞ "Munich"
# 
# grab_city("Cheese Factory Tour [Portland]") ➞ "Portland"
# 
# grab_city("[50% Off!][Group Tours Included] 5-Day Trip to Onsen [Kyoto]") ➞ "Kyoto"

grab_city("[Duration: 7 hours] Tour of the Maritimes [Charlottetown]")


remove_abc <- function(x){
  paste0(regmatches(x,gregexpr("[^abc]",x,perl = TRUE))[[1]],collapse = "")
}

# remove_abc("This might be a bit hard") ➞ "This might e  it hrd"
# 
# remove_abc("hello world!") ➞ None
# 
# remove_abc("") ➞ None

make_title <- function(x) {
  regmatches(x,gregexpr("^[A-z]",x,perl = TRUE))[[1]] <- toupper(regmatches(x,gregexpr("^[A-z]",x,perl = TRUE))[[1]])
  regmatches(x,gregexpr(" [A-z]",x,perl = TRUE))[[1]] <- toupper(regmatches(x,gregexpr(" [A-z]",x,perl = TRUE))[[1]])
  
  x
}

# make_title("This is a title") ➞ "This Is A Title"
# 
# make_title("capitalize every word") ➞ "Capitalize Every Word"
# 
# make_title("I Like Pizza") ➞ "I Like Pizza"
# 
# make_title("PIZZA PIZZA PIZZA") ➞ "PIZZA PIZZA PIZZA"
make_title("1f you c4n r34d 7h15, you r34lly n33d 2 g37 l41d")

