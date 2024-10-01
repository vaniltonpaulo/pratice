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
