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





#COLLECT ALL DATA INTO A SINGLE LIST COLUMN


# Write a function that accepts one `data.table` argument `data` and collects all the sensor data into
# a single list column. This means, the resulting `data.table` should have columns `machine`, `quality`,
# `sensor`, and the `sensor` column should be a `list` column containing a named `numeric` vector
# containing all the non-`NA` sensor values with appropriate names. The order of the table should not be changed.
# The output for the example data would therefore be:
widget.corp.data.list <- rbindlist(list(
  list(machine = NULL, quality = NULL, sensor = NULL),
  list("Machine01",    78,             list(c(sensor01 = 23, sensor02 = 28.6, sensor03 = -23))),
  list("Machine02",    28,             list(c(sensor01 = 41, sensor02 = 77.8, sensor04 = 27))),
  list("Machine03",    32,             list(c(sensor01 = 57, sensor02 = 91.6, sensor03 = -29, sensor04 = 10))),
  list("Machine03",    80,             list(c(sensor02 = 32.3))),
  list("Machine03",    58,             list(c(sensor01 = 10, sensor02 = 77.8, sensor03 = 3))),
  list("Machine02",    74,             list(c(sensor02 = 24.5, sensor03 = -18, sensor04 = 3))),
  list("Machine01",    46,             list(c(sensor01 = 81))),
  list("Machine01",    24,             list(c(sensor01 = 43, sensor02 = 13.3, sensor03 = -22))),
  list("Machine02",    7,              list(c(sensor01 = 96, sensor02 = 96.0, sensor03 = 0))),
  list("Machine01",    22,             list(c(sensor01 = 107, sensor02 = 23.5, sensor03 = 7, sensor04 = 8))),
  list("Machine03",    98,             list(c(sensor03 = 11)))
))
  
  
  # Be aware that some rows may not have any non-missing sensor data, in which case the `sensor` value for the
  # corresponding result row should be an empty `numeric`.
  ex02ListTable <- function(data) {
    assertDataTable(data)
    sensorcols <- grep("^sensor[0-9]+", colnames(data), value = TRUE)
    data[, sensor := list(list(c(na.omit(unlist(.SD))))), .SDcols = sensorcols, by = seq_len(nrow(data))]
    data[, c("machine", "quality", "sensor")]
  }
  
  
  
#   3. data[, sensor := list(list(c(na.omit(unlist(.SD))))), .SDcols = sensorcols, by = seq_len(nrow(data))]
#   This is the key part of the function, and it's performing a transformation on the data.table.
# Let’s break it down:
# 
# .SDcols = sensorcols: This specifies that only the columns stored in sensorcols (i.e., columns starting with "sensor") should be considered in the operation.
# by = seq_len(nrow(data)): This ensures that the operation is performed row-by-row. The sequence seq_len(nrow(data)) simply generates a sequence from 1 to the number of rows in the data, effectively applying the operation to each row individually.
# .SD: .SD is a special symbol in data.table that represents the subset of data being worked on (in this case, the sensorcols columns for each row).
# unlist(.SD): This takes all the sensorcols columns for the current row and "unlists" them, i.e., it combines all sensor values from that row into a single vector.
# na.omit(unlist(.SD)): This removes any NA values from the unlisted vector of sensor data.
# list(c(...)): The cleaned (non-NA) vector is wrapped in a list to create a list column. The double list() is used to ensure that the new column sensor is a list of lists (since each row will store a list of sensor values).
# sensor :=: This assigns the result to a new column called sensor in the data table.
# In summary, this line creates a new column sensor in data where each row contains a list of the non-NA sensor values from that row.
  
  
  
  
  
  
  
################  
#                   PRIME
#                       NUMBERS
  ############################  
  ksiprime<-function(n){
    if(n == 1) return(FALSE)
    if(n == 2) return(TRUE)
    if(n %% 2 == 0) return(FALSE)
    
    for (i in 3:sqrt(n)) {
      if(n %% i == 0 ) return(FALSE)
    }
    return(TRUE)    
    
  }
  ksiprime(47)
  
  
  
  
  
  ############### COOL CONCEPT ALMOST WORKS Problem is the mean is a bit off in two cases
  x<-2
  ex04CarrierDelay <- function(flights, year) {
    # your code
    
    
    
    assertDataTable(flights)
    assertCount(year, tol = 0)
    yr <- year
    
    flights <-flights[order(month, day,hour)]
    result<-flights[yr == year,lapply(.SD,mean),.SDcols = "arr_delay" , by = .(month, carrier)]
    result <-result[order(month,carrier)]
    
    
    
    
    
    carrier.res<-unique(flights$carrier)
    soka_list <- list()
    for (i in seq_along(carrier.res)) {
      carrier.names <-carrier.res[[i]]
      for (j in seq_len(12)) {
        
        soka <-flights[yr == year & carrier != carrier.names & month == j,.(carrier = carrier.names,mean.delay.competition =mean(arr_delay, na.rm = TRUE)), by = .(month)]
        soka_list[[paste0("carrier_", carrier.names, "_month_", j)]] <- soka 
      }
    }
    #soka_list
    
    nice<-rbindlist(soka_list,fill = TRUE)
    nice<- nice[order(month,carrier)]
    
    
    sooo<- merge(result,nice,all.x = TRUE, by = c(month ="month",carrier ="carrier"))
    sooo<-sooo[,.(month, carrier ,mean.delay=arr_delay,mean.delay.competition)]
    combs <- CJ(month = 1:12, carrier = flights$carrier, unique = TRUE)
    res <- sooo[combs, on = c("month", "carrier")]
    setnafill(res,fill = 0,cols = c("mean.delay",  "mean.delay.competition"))
  }