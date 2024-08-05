is_last_character_n <- function(x){
  y<-strsplit(x,"")[[1]]
  y[[length(y)]] == 'n'
}


# is_last_character_n("Aiden") ➞ True
# 
# is_last_character_n("Piet") ➞ False
# 
# is_last_character_n("Bert") ➞ False
# 
# is_last_character_n("Dean") ➞ True
is_last_character_n("Daniel")


count_d <- function(x){
  y <- strsplit(x,"")[[1]]
  
  sum(y == 'd') + sum(y == "D")
}



# count_d("My friend Dylan got distracted in school.") ➞ 4
# 
# count_d("Debris was scattered all over the yard.") ➞ 3
# 
# count_d("The rodents hibernated in their den.") ➞ 3


potatoes <- function(x){
  
  y<-regmatches(x, gregexpr("potato", x ))[[1]]
  
  length(y)
}

# 
# potatoes("potato") ➞ 1
# 
# potatoes("potatopotato") ➞ 2
# 
# potatoes("potatoapple") ➞ 1



count_claps <- function(x){
  
  # y <- strsplit(x,"C")[[1]]
  # length(y) -1
  
  #Alternative
  sum(strsplit(x,"")[[1]] == "C")
}

# count_claps("ClaClaClaClap!") ➞ 4
# count_claps("ClClClaClaClaClap!") ➞ 6
# count_claps("CCClaClClap!Clap!ClClClap!") ➞ 9
count_claps("Clap!CClaClap!ClClap!ClClClap!ClaCClap!ClaCClClap!CClap!CClClaClap!ClaClaClCCClap!Clap!CClClaClCClaClaClClap!Clap!CClaCClaCClap!ClaCClClCClap!CClap!CClap!ClaClaClaClaClap!ClClap!CClaClaClaClClaClClCClClCClClaClaCClClap!")






amazing_edabit <- function(x){
  if(grepl("edabit",x) == TRUE){
    return(x)
  } else{
    sub("amazing","not amazing", x)
  }  
}

# amazing_edabit("edabit is amazing.") ➞ "edabit is amazing."
# 
# amazing_edabit("Mubashir is amazing.") ➞ "Mubashir is not amazing."
# 
# amazing_edabit("Infinity is amazing.") ➞ "Infinity is not amazing."




bomb <- function(x){
  if(grepl("bomb",x) == TRUE){
    return("Duck!!!")
  } else{
    return("There is no bomb, relax.")
  }  
}



# bomb("There is a bomb.") ➞ "Duck!!!"
# 
# bomb("Hey, did you think there is a bomb?") ➞ "Duck!!!"
# 
# bomb("This goes boom!!!") ➞ "There is no bomb, relax."


get_filename <- function(x){
  sub(".*[/\\]","",x)
  #ALternative
  #basename(x)
  
}



# get_filename("C:/Projects/pil_tests/ascii/edabit.txt") ➞ "edabit.txt"
# 
# get_filename("C:/Users/johnsmith/Music/Beethoven_5.mp3") ➞ "Beethoven_5.mp3"
# 
# get_filename("ffprobe.exe") ➞ "ffprobe.exe"


count_syllables <- function(x){
  length(strsplit(x,"[aeiouAEIOU]")[[1]])
}

# 
# count_syllables("Hehehehehehe") ➞ 6
# 
# count_syllables("bobobobobobobobo") ➞ 8
# 
# count_syllables("NANANA") ➞ 3





variable_valid <- function(x){
  
  if(  grepl("\\s",x) == TRUE){
    return(FALSE)
  }
  if(grepl("^[[:alpha:]]",x) == TRUE){
    return(TRUE)
  }else{
    return(FALSE)
  }
  TRUE
  
  #Alternative
  #grepl("^[a-zA-Z][a-zA-Z0-9_]*$", x)
}


# 
# variable_valid("result") ➞ True
# 
# variable_valid("odd_nums") ➞ True
# 
# variable_valid("2TimesN") ➞ False
variable_valid("rather_long_variable_name")
variable_valid("count spaces")





owofied <- function(x){
  y<-gsub("i","wi",x)
  y <-gsub("e","we",y)
  paste(y,"owo",collapse = "")
  
}



# 
# owofied("I'm gonna ride 'til I can't no more")
# ➞ "I'm gonna rwidwe 'twil I can't no morwe owo"
# 
# owofied("Do you ever feel like a plastic bag")
# ➞ "Do you wevwer fwewel lwikwe a plastwic bag owo"
# 
# owofied("Cause baby you're a firework")
# ➞ "Causwe baby you'rwe a fwirwework owo"

owofied("We've known each other for so long")



get_decimal_places <- function(x){
  if(grepl("\\.",x) == TRUE){
    y<-regmatches(x,regexpr("(?<=\\.)[0-9]*",x,perl = TRUE))
    return(length(strsplit(y,"")[[1]]))
    
  }else{
    return(0)
  }
}

# 
# get_decimal_places("43.20") ➞ 2
# 
# get_decimal_places("400") ➞ 0
# 
# get_decimal_places("3.1") ➞ 1
get_decimal_places("00010.00010")


filter_string <- function(x){
  y <- strsplit(x,"")[[1]]
  big<-sum(grepl("[[:upper:]]",y,perl = TRUE))
  small <- sum(grepl("[[:lower:]]",y,perl = TRUE))
  digits <- sum(grepl("[[:digit:]]",y,perl = TRUE))
  weird <- sum(grepl("[[:punct:]]",y,perl = TRUE))
  list(big,small,digits,weird)
}


# 
# filter_string("*$(#Mu12bas43hiR%@*!") ➞ [2, 6, 4, 8]
# # 2 uppercase letters
# # 6 lowercase letters
# # 4 numbers
# # 8 special characters
# 
# filter_string("^^Edabit^^%$#12379") ➞ [1, 5, 5, 7]
# 
# filter_string("**Airforce1**") ➞ [1, 7, 1, 4]
filter_string("RYT'>s&gO-.CM9AKeH?,5317tWGpS<*x2ukXZD")




sub_reddit <- function(x){
  basename(x)
}




# sub_reddit("https://www.reddit.com/r/funny/") ➞ "funny"
# 
# sub_reddit("https://www.reddit.com/r/relationships/") ➞ "relationships"
# 
# sub_reddit("https://www.reddit.com/r/mildlyinteresting/") ➞ "mildlyinteresting"





is_prefix <- function(x,y){
  grepl("^auto",x,perl = TRUE)
}


is_suffix <- function(word,suffix){
  
  # Remove the leading "-" from the suffix
  suffix <- sub("^-", "", suffix)
  
  # Check if the end of the word matches the suffix
  
  substr(x,nchar(x) - nchar(suffix)+ 1,nchar(x)) == suffix
}



is_prefix <- function(word,prefix){
  # Remove the leading "-" from the suffix
  prefix <- sub("-$", "", prefix)
  # Check if the end of the word matches the suffix
  substr(word,1,nchar(prefix)) == prefix
  
}
# 
# is_prefix("automation", "auto-") ➞ True
# 
# is_suffix("arachnophobia", "-phobia") ➞ True
# 
# is_prefix("retrospect", "sub-") ➞ False
# 
# is_suffix("vocation", "-logy") ➞ False
is_prefix("retrospect", "sub-")
is_prefix("superfluous", "super-")
#
