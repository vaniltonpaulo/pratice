# data tables

library(data.table)

# Create a sample data.table
dt <- data.table(
  name = c("John", "Alice", "Bob", "John", "Alice"),
  income = c(45000, 55000, 30000, 48000, 56000),
  age = c(30, 25, 28, 31, 26)
)


dt[, .(mean_income = mean(income)), by = name]
dt[, lapply(.SD, mean), by = name, .SDcols = c("income", "age")]

# Using .SDcols to limit which columns are in .SD
dt[, lapply(.SD, mean), by = name, .SDcols = "income"]


# Compute both mean and range of income
dt[, .(mean_income = mean(income), income_range = max(income) - min(income)), by = name]


# Exercises

#dt[i,j,by]

# Sample data.table
dt <- data.table(
  name = c("John", "Alice", "Bob", "John", "Alice", "Bob", "John", "Alice"),
  age = c(30, 25, 28, 31, 26, 29, 32, 27),
  income = c(45000, 55000, 30000, 48000, 56000, 32000, 50000, 57000),
  city = c("NY", "LA", "NY", "LA", "NY", "LA", "NY", "LA")
)


#Income greater than 50k
dt[income > 50000]

dt[,.(name, income)]

dt[,.(mean_income = mean(income)),by =.(city)]

dt[age > 30, .(total_income = sum(income)), by = .(name)]

dt[,.N,by = .(city)]

dt[,.(range_of_income = max(income) - min(income)) ,by = .(name)]


###SD


dt <- data.table(
  name = c("John", "Alice", "Bob", "John", "Alice", "Bob", "John", "Alice"),
  age = c(30, 25, 28, 31, 26, 29, 32, 27),
  income = c(45000, 55000, 30000, 48000, 56000, 32000, 50000, 57000),
  city = c("NY", "LA", "NY", "LA", "NY", "LA", "NY", "LA")
)


dt[,lapply(.SD,mean),by = city,.SDcols = c("age","income")]

dt[,.(mean_income = mean(income), sum_income = sum(income)),by = .(name),.SDcols = c("income")]

dt[, lapply(.SD, function(x) list(mean = mean(x), sum = sum(x))), by = name, .SDcols = "income"]


dt[,lapply(.SD, function(x) list(sum_age = sum(age),sum_income. = sum(income))),by = .(city),.SDcols = c("age","income")]

dt[, lapply(.SD, sum), by = city, .SDcols = c("age", "income")]

dt[,lapply(.SD,function(x) (x -mean(x))/sd(x)),by = .(name), .SDcols = c("age","income")]


dt[,lapply(.SD,function(x) list(mins = min(x),maxs = max(x))),by = .(city),.SDcols = c("age","income")]


dt[,lapply(.SD,sum),by =.(name),.SDcols = is.numeric]

dt[,lapply(.SD,uniqueN),by = .(name),.SDcols = c('city')]

dt[,lapply(.SD,mean),by =.(city),.SDcols = grepl("income",names(dt))]

dt[income > 50000,lapply(.SD,mean),by= .(name),.SDcols = c('age')]

dt[,.SD[income > 40000],by= .(name)]



#BINDER CLASS 

#####Mine
dt <- as.data.table(cars) #nice
dt[,c("speed.2") := .(speed^2)] #nice
dt[speed %between% c(10,20),] #nice 
dt[speed.2 >= 4* dist,] # forgot to add =
dt[,c("speed.3","speed.4") := .(speed^4,speed^4)]# nice
dt[,speed := NULL] #nice



######################
# 1. Get a data.table out of the cars dataset
#    (Note that setDT() won't work on built-in datasets directly)

# (a)
carsdt <- as.data.table(cars)

# (b)
carsdt <- cars
setDT(carsdt)

######################
# 2. Create a column speed.2, containing the squared "speed"-values

carsdt[, speed.2 := speed^2]

# possible, but not recommended:
cbind(carsdt, speed.2 = carsdt$speed^2)
carsdt$speed.2 <- carsdt[, speed^2]
carsdt$speed.2 <- carsdt$speed^2

carsdt <- carsdt[, .(speed, dist, speed.2 = speed^2)]

# result
carsdt

######################
# 3. Remove all rows that have speed either below 10 or above 20
# Try this using %between%
carsdt[!(speed < 10 | speed > 20)]
carsdt[speed >= 10 & speed <= 20]
carsdt[speed %between% c(10, 20)]

carsdt <- carsdt[speed %between% c(10, 20)]

######################
# 4. Remove all rows where speed.2 is smaller than 4 times the dist
carsdt <- carsdt[speed.2 >= dist * 4]

carsdt

######################
# 5. Add the columns speed.3 and speed.4 (as speed^3 and speed^4) with a single command
carsdt[, `:=`(speed.3 = speed^3, speed.4 = speed^4)]

carsdt[, c("speed.3", "speed.4") := .(speed^3, speed^4)]

carsdt[, (paste0("speed.", 3:4)) := lapply(3:4, function(x) speed ^ x)]

carsdt <- carsdt[, .(speed, dist, speed.2, speed.3 = speed ^ 3, speed.4 = speed ^ 4)]


######################
# 6. Remove the speed column
carsdt[, speed := NULL]
carsdt$speed <- NULL