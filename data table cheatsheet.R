#How to order things
flights.data[][order(number_of_fligth, decreasing = TRUE)]

#how to match multiple values in a row

flights.data[carrier %in%  carrier.code, , ]


#keep the collumns and rows exact the same in the left table  and making the table on the rigth match according to that

merge(first, second, by = c(z ="month",x ="origin", y ="dest"), all.x = TRUE)


#Selecting the row that....
#( this is maily used when there multiple of the same thing expample: the same rout has multiple arrival delays)

flights[flights[,.I[which.max(arr_delay)], by =.(origin,dest) ]$V1]
flights[flights[,.I[which.min(dep_delay)], by =.(origin,dest) ]$V1]

#Handling date in data table


cutoff<-ISOdate(2008,12,8)
bank.ledger[ISOdate(yr,mth,day) < cutoff ]


#How to select the highest paid in two company

#First order
salaries_data<-salaries_data[order(-Salary)]
#then
salaries_data[order(-Salary),.SD[1],by = Company]
#Alternative
salaries_data[salaries_data[,.I[1], by = Company]$V1]




#How to select the second highest paid in a company
#First order
salaries_data<-salaries_data[order(-Salary)]
salaries_data[order(-Salary),.SD[2],by = Company]
#Alternative
salaries_data[salaries_data[,.I[2], by = Company]$V1]


#You can also use .I for indexing


# Find the row index of the first person with Age > 30 in each Gender group
salaries_data[salaries_data[, .I[Age > 30 & Company =="CompanyA" ], by = Gender]$V1]


#Find out how many times a number goes into another number

x %/% y




########################################################################

#HERE WE ARE DOING A DEEP DIVE OF THE EXERCISE 


#EXERCISES 1

#HOW TO ASSERT IN DATA TABLE

# ------  1-ASSERT THAT IT IS A DATA TABLE
#        -->  assertDataTable(prices)
#----------2 - ASSERT THAT A SPECIFIC COLUMN IS THERE
#         ---> assertDataTable(a, types = "atomic")
#              assertNames(colnames(a), must.include = "id")


# ------- 2 ASSERT FOR LIST
#  -------  assertList(k, 
#             names = "unique" #asserts that that the elements inside have unique name
#              ie list(a=2,b=8)
 #                ,any.missing = TRUE #IF YOU ALLOW FOR missing values    
#)
#diff between by and keyby

#In the first case (by), the result is not sorted, while in the second case (keyby),
#the result is sorted by group.


# How to remove a column from a data tabe

#minuend[!subtrahend, on = "index"] 
#OR USE NULL ( BETTER)


# HOW TO USE Rbindlist

#rbindlist(lst,
#          fill = TRUE, # fill the columns with elements inside the list
   #use.names = TRUE)









#HOW TO DEAL WITH COMPETION

ex04CarrierDelay <- function(flights, year) {
  # your code
  assertDataTable(flights)
  assertInt(year)
  combs <- CJ(month = 1:12, carrier = flights$carrier, unique = TRUE)
  yr <- year
  allflights <- flights[year == yr][combs, on = c("month", "carrier")][, {
    dt <- .SD
    .SD[, .(
      mean.delay = mean(arr_delay),
      mean.delay.competition = dt[!carrier, mean(arr_delay, na.rm = TRUE), on = "carrier"]
    ), by = "carrier"]
  }, by = "month"]
  res <- allflights[combs, on = c("month", "carrier")]
  setnafill(res, fill = 0, cols = "mean.delay")
  res
}



# 
# 
# 
# 3. .SD and dt <- .SD:
#   .SD is a special symbol in data.table that refers to the Subset of Data corresponding to the current group within by operations.
# dt <- .SD assigns the current subset of the data (i.e., the data for a specific month and carrier) to a local variable dt.
# dt is used to store the data that contains the flights for the current group (month and carrier).
# 4. Inside .SD[, .()]:
#   Now, within the .SD (Subset of Data) for each carrier and month:
#   
#   mean.delay = mean(arr_delay):
#   
#   This calculates the mean delay (mean.delay) for the current carrier in the current month.
# It computes the average arr_delay (arrival delay) for flights of that carrier in the month.
# mean.delay.competition = dt[!carrier, mean(arr_delay, na.rm = TRUE), on = "carrier"]:
#   
#   This calculates the mean delay of the competition (i.e., every other airline except the current carrier).
# dt[!carrier, ...] means "exclude the current carrier."
# For the remaining rows (the competitors), the function computes the mean of arr_delay to get the delay for the competition.
# on = "carrier" ensures that it works with the carrier column to exclude the current airline while calculating the competition's delays.
# 5. by = "carrier" and by = "month":
# The calculation is performed by each carrier within each month.
# The outer by = "month" groups the data by month, so that for each month, the mean delay for each carrier and their competitors is calculated.
# The inner by = "carrier" within the .SD[, ...] block ensures that the calculations are performed for each carrier in that specific month.