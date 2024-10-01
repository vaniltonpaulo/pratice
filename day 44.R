library(data.table)

# Example price list for a bookstore
bookstore.prices <- rbindlist(list(
  list(book = NULL,             price.onsite = NULL, price.online = NULL),
  list("Magic Guide",           15.00,               18.00),
  list("Data Science Handbook", 25.00,               27.50),
  list("Art of War",            12.50,               14.00),
  list("Philosophy 101",        10.00,               12.00),
  list("Science Fiction Vol. 1", 8.50,               10.00)
))

# Example sales records for a bookstore
bookstore.sales <- rbindlist(list(
  list(book = NULL,             channel = NULL),
  list("Magic Guide",           "online"),
  list("Data Science Handbook", "onsite"),
  list("Data Science Handbook", "online"),
  list("Art of War",            "onsite"),
  list("Philosophy 101",        "onsite"),
  list("Science Fiction Vol. 1","online"),
  list("Magic Guide",           "onsite")
))

sales <- copy(bookstore.sales)
prices <- copy(bookstore.prices)
#1 Exercise

sales[,.(count = .N), by=.(book)][prices,on="book"][,.(book,count = nafill(count,fill = 0))]


#2 exercise

sales[,c("channel") := (paste0("count.",channel))][]
result <- if(!nrow(sales)) price[book,] else dcast(sales, book~channel,fun.aggregate = length)
#result <- rbind(data.table(book = character(0),count.online = numeric(0),count.onsite = numeric(0)),result,fill = TRUE)


#3 exercise

sum(prices[sales, on = c("book")][,ifelse(channel == "online",price.online,price.onsite)])

# exercise 4

sales[,.SD[which.max(.N)],by = .(channel),.SDcols = c("book")]


#exercise 5

prices[sales,on=c("book")][,.(revenue = sum(ifelse(channel == "online", price.online,price.onsite))), by =.(book)]


#exercise 6

sales[, .N, by = .(book,channel)][, .N,by = book][N == 2,.(book)]


#Exercise 7 

sales[,.N,by = .(channel,book)][channel == "online",.(book)]


#Exercise 8
prices[sales,on = "book"][,.(avg_mean =mean(ifelse(channel == "online", price.online, price.onsite))), by =.(channel)]



################################## baby step

customers <- data.table(
  name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  age = c(25, 40, 35, 28, 50),
  city = c("New York", "Los Angeles", "Chicago", "Miami", "Boston")
)

#Exercise 1.1
customers[age>30,]



customers[,age_squared:= age ^2 ][]


sales <- data.table(
  product = c("Laptop", "Mouse", "Laptop", "Mouse", "Keyboard"),
  quantity = c(2, 1, 1, 3, 2)
)

sales[ , .(quantity = sum(quantity)),by = product]



products <- data.table(
  product = c("Laptop", "Mouse", "Keyboard"),
  price = c(1000, 50, 150)
)

sales <- data.table(
  product = c("Laptop", "Mouse", "Keyboard", "Laptop", "Mouse"),
  quantity = c(2, 1, 3, 1, 2)
)

sales[products,on  ="product"][,.(rev = sum(quantity * price)),by=product]


transactions <- data.table(
  date = as.Date(c("2024-01-15", "2024-01-20", "2024-02-10", "2024-02-20")),
  amount = c(200, 150, 300, 250)
)

transactions[,sum(amount),by = month(date)]



sales <- data.table(
  product = c("Laptop", "Mouse", "Keyboard", "Laptop", "Mouse"),
  quantity = c(12, 9, 15, 4, 8)
)

sales[,product_rating := ifelse(quantity > 10,"Top Seller","Regular Seller")][]

sales <- data.table(
  product = c("Laptop", "Mouse", "Keyboard", "Laptop", "Mouse"),
  onsite_quantity = c(2, 1, 0, 1, 3),
  online_quantity = c(3, 2, 1, 2, 0)
)

sales[,.(quantity = sum(onsite_quantity+online_quantity)),by = product]



customers <- data.table(
  name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  age = c(25, NA, 35, 28, NA)
)
mean_age <- customers[, mean(age, na.rm = TRUE)]

# Step 2: Replace missing age values (NA) with the calculated mean
customers[is.na(age), age := mean_age]
