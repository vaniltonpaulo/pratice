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


##################################################################

##################################################################

input <- if (file.exists("flights14.csv")) {
  "flights14.csv"
} else {
  "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"
}
flights <- fread(input)
flights

ans <- flights[order(origin,-dest)]
flights[,sum((arr_delay+ dep_delay)<0)]

flights[origin == "JFK" & month == 6,.N]
flights["XYZ", nomatch = NULL]

flights[,.(origin)]



# keyby just order insinde the column in ascending order
flights[carrier == "AA",
        .(mean(arr_delay), mean(dep_delay)),
        keyby = .(origin, dest, month)]


####################### DATA TABLE PART 2



itemshop.prices <- rbindlist(list(
  list(item = NULL,             price.onsite = NULL, price.online = NULL, price.b2b = NULL),
  list("Healing Potion",        9.99,                12.99,               8),
  list("Staff of Illusion",     18.95,               20.00,               15),
  list("Lesser Stone of Mana",  2.60,                4.00,                2),
  list("Greater Stone of Mana", 7.50,                9.99,                6),
  list("Sword of Clarity +2",   21.50,               22.99,               20)
))

itemshop.sales <- rbindlist(list(
  list(item = NULL,             channel = NULL, quantity = NULL),
  list("Healing Potion",        "online",       1),
  list("Sword of Clarity +2",   "onsite",       1),
  list("Sword of Clarity +2",   "online",       2),
  list("Sword of Clarity +2",   "onsite",       2),
  list("Sword of Clarity +2",   "b2b",          200),
  list("Greater Stone of Mana", "onsite",       3),
  list("Staff of Illusion",     "b2b",          100)
))


# how many sales did we have online, how many on-site, etc?

itemshop.sales[channel == "online", sum(quantity)]

itemshop.sales[channel == "onsite", sum(quantity)]

itemshop.sales[channel == "b2b", sum(quantity)]

# Instead: aggregate / "group by"

# base-R:
aggregate(itemshop.sales$quantity, by = itemshop.sales[, "channel"], FUN = sum)

# data.table:

itemshop.sales[, .(quantity.total = sum(quantity)), by = "channel"]

# notice: we are using 'i' as we did before!
# compare:
itemshop.sales[, .(quantity.total = sum(quantity))]

itemshop.sales[, sum(quantity)]

# the 'by' version has the 'channel' column included!


# even an atomic is turned into a data.table here:
itemshop.sales[, sum(quantity), by = "channel"]
# (doesn't know the name, therefore uses 'V1')

# what is happening?
# 'j' expression is run N times!

# remember, expressions can be a lot!
itemshop.sales[, {
  cat("hello!\n")  # run 3 times
  .(quantity.total = sum(quantity))
}, by = "channel"]


# group by multiple
itemshop.sales[, .N, by = c("channel", "item")]

# special variables

# .N: size of the group
itemshop.sales[, .N, by = "channel"]

itemshop.sales[, .(.N, total = sum(quantity)), by = "channel"]

# .I: row index of original dataset
itemshop.sales[, .(message = paste("Using rows", paste(.I, collapse = ", "))), by = "channel"]


# .GRP: "index" of the group
itemshop.sales[, .GRP, by = "channel"]

# .NGRP: total number of groups
itemshop.sales[, .(message = paste("Group", .GRP, "out of", .NGRP)), by = "channel"]


# .BY: the "by" column(s)
#  named list with one element!
itemshop.sales[, .(by.expr = deparse(.BY)), by = "channel"]

xx <- "channel"  # user given, what if idk what this is
itemshop.sales[, .(dotby = .BY[[1]]), by = xx]

itemshop.sales[, .(by.expr = deparse(.BY)), by = c("channel", "item")]

# .SD: the sub-data.table
itemshop.sales[, str(.SD)]

itemshop.sales[, str(.SD), by = "channel"]

# e.g.: get "best" row within sub-data.table
itemshop.sales[, .SD[which.max(quantity)], by = "channel"]

# e.g.: get first / last row within sub-data.table
itemshop.sales[, .SD[1], by = "channel"]
itemshop.sales[, .SD[.N], by = "channel"]

# e.g.: use lapply
itemshop.sales[, lapply(.SD, function(x) length(unique(x))), by = "channel"]


itemshop.sales[, ncol(.SD), by = "channel"]
itemshop.sales[, ncol(.SD), by = "channel", .SDcols = c("quantity")]

itemshop.sales[, lapply(.SD, function(x) length(unique(x))),
               by = "channel", .SDcols = "quantity"]

irisdt <- as.data.table(iris)

numcols <- 1:4  # colnames(irisdt)[1:4]
# irisdt[, lapply(.SD, round)]
irisdt[, (numcols) := lapply(.SD, round) , .SDcols = numcols]


coef(lm(Sepal.Length ~ ., data = irisdt))

irisdt[, as.list(coef(lm(Sepal.Length ~ ., data = .SD))),
       by = "Species"]

irisdt[, as.list(coef(lm(Sepal.Length ~ ., data = .SD))),
       by = "Species", .SDcols = c("Sepal.Width", "Petal.Width")]

# can use `:=` with `by`:

is2 <- copy(itemshop.sales)

is2[, sum.total := sum(quantity)]

is2[, quantity.total := sum(quantity), by = "channel"]

is2[, index.within.group := seq_len(.N), by = "channel"]

# `i` is evaluated before `by`
itemshop.sales[1:3, .N, by = "channel"]


###############
# exercise

# (made up) list of ski-resorts with temperatures
ski.temps <- rbindlist(list(
  list(name = NULL,   country = NULL, temp = NULL, unit = NULL),
  list("Zugspitze",   "Germany",      23,          "Fahrenheit"),
  list("Mt Buller",   "Australia",    -3,          "Celsius"),
  list("Lake Louise", "Canada",       -5,          "Celsius"),
  list("Mt Buller",   "Australia",    41,          "Fahrenheit"),
  list("Zugspitze",   "Germany",      -8,          "Celsius"),
  list("Wurmberg",    "Germany",      -1,          "Celsius")
))


# 1. convert all temperatures to celsius
#   (formula F -> C is  C = (F - 32) * 5/9
ski.temps[,
          temp := ifelse(unit == "Fahrenheit", (temp - 32) * 5 / 9, temp)
]

ski.temps[unit == "Fahrenheit", temp := (temp - 32) * 5 / 9]
ski.temps[, unit := "Celsius"]

# 2. create a table that lists the resort with the highest max temp. in each country

ski.temps[, .(name = name[which.max(temp)]), by = "country"]
ski.temps[, .SD[which.max(temp)], by = "country"]

# what about lowest max temp

ski.temps[, .(temp = max(temp)), by = c("country", "name")][,
                                                            .SD[which.min(temp)], by = "country"]


ski.temps[unit == "Fahrenheit",
          temp := ifelse(unit == "Fahrenheit", (temp - 32) * 5 / 9, temp)
]

ski.temps[unit == "Fahrenheit", temp := (temp - 32) * 5 / 9]

ski.temps[, .(name = name[which.max(temp)]), by = "country"]

ski.temps[, .SD[which.max(temp)], by = "country"]
