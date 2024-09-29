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


