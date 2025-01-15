get_missing_letters <- function(x) {
  x <- strsplit(x, "")[[1]]
  paste0(setdiff(letters, x), collapse = "")
  
}


# get_missing_letters("abcdefgpqrstuvwxyz") ➞ "hijklmno"
# 
# get_missing_letters("zyxwvutsrq") ➞ "abcdefghijklmnop"
# 
# get_missing_letters("abc") ➞ "defghijklmnopqrstuvwxyz"
# 
# get_missing_letters("abcdefghijklmnopqrstuvwxyz") ➞ ""



censor <- function(x) {
  result <- strsplit(x," ")[[1]]
  final <- character(0)
  for (i in seq_len(length(result))) {
    if(nchar(result[[i]]) > 4){
      final[[length(final) + 1]] <- paste0(rep("*",nchar(result[[i]]) ),collapse = "")
    } else {
      final[[length(final) + 1]] <- result[[i]]
    }
  }
  paste0(final,collapse = " ")
  
}

# censor("The code is fourty") ➞ "The code is ******"
# 
# censor("Two plus three is five") ➞ "Two plus ***** is five"
# 
# censor("aaaa aaaaa 1234 12345") ➞ "aaaa ***** 1234 *****"

