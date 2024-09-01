
sort_by_letter <- function(x){
  if(length(x) == 0) return("")
  k <- regmatches(x,gregexpr('[a-z]',x,perl = TRUE))
  k.vowels<-unlist(k)
  x[order(k.vowels)]
  
}

# sort_by_letter(c("932c", "832u32", "2344b"))
# ➞ ["2344b", "932c", "832u32"]
# 
# sort_by_letter(c("99a", "78b", "c2345", "11d"))
# ➞ ["99a", "78b", "c2345", "11d"]
# 
# sort_by_letter(c("572z", "5y5", "304q2"))
# ➞ ["304q2", "5y5", "572z"]
# 
# sort_by_letter(c())
# ➞ []


last_name_lensort <- function(x){
  k <- lapply(x,function(k){
    strsplit(k," ")[[1]][[2]]
  })
  
  surnames<-unlist(k)
  x[order(nchar(surnames))]
}



last_name_lensort(c(
   "Jennifer Figueroa","Heather Mcgee",
   "Amanda Schwartz","Nicole Yoder","Melissa Hoffman"))

#➞ ["Heather Mcgee", "Nicole Yoder", "Melissa Hoffman", "Jennifer Figueroa", "Amanda Schwartz"]

last_name_lensort(c("Hitagi Senjougahara","Edward Elric","Light Yagami","Rintaro Okabe","Kurisu Makise"))




who_passed<-function(x){
  
  check.grade <- function(student,grades){
    k <- lapply(grades,function(k){
      strsplit(k,"/")[[1]]
    })
    result <- logical(0)
    for (i in seq_len(length(k))) {
      if(k[[i]][[1]] == k[[i]][[2]]){
        result <- c(result,TRUE)
      }else{
        result <- c(result,FALSE)
      }
      
    }
    
    final <- character(0)
    
    if(all(result) == TRUE){
      final<-c(final,student)
    } 
    
    final 
    
  }

  
  
  final.vect <-character(0)
  
  for (i in seq_along(names(x))) {
    final.vect <-c(final.vect,check.grade(names(x)[[i]],x[[names(x[i])]]))
  }
  return(sort(final.vect))
  
  
}


# who_passed({
#   "Zara" : ["10/10"],
#   "Kris" : ["30/30"],
#   "Charlie" : ["100/100"],
#   "Alex" : ["1/1"]
# }) ➞ ["Alex", "Charlie", "Kris", "Zara"]
# 
# who_passed({
#   "Zach" : ["10/10", "2/4"],
#   "Fred" : ["7/9", "2/3"]
# }) ➞ []


 who_passed( list(
  John = c("5/5", "50/50", "10/10", "10/10"),
  Sarah = c("4/8", "50/57", "7/10", "10/18"),
  Adam = c("8/10", "22/25", "3/5", "5/5"),
  Barry = c("3/3", "20/20")
))
 
 
 
 who_passed( list(
   Zara = c("10/10"),
   Kris = c("30/30"),
   Charlie = c("100/100"),
   Alex = c("1/1")
 ))

 
 who_passed(list(
   Zach = c("10/10", "2/4"),
   Fred = c("7/9", "2/3")
 ))
 