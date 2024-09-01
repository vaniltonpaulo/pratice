
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
