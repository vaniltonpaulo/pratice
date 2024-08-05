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

