prices <- data.table(
  item = c("Potion", "Sword", "Shield", "Armor"),
  price_onsite = c(10, 50, 30, 100),
  price_online = c(12, 55, 33, 110)
)

sales <- data.table(
  item = c("Potion", "Sword", "Potion", "Armor", "Shield", "Sword", "Sword"),
  channel = c("onsite", "online", "online", "onsite", "onsite", "onsite", "online")
)

#exercise 1.1
sales[,.(count = .N),by=.(item)]

#exercise 1.3

x<-dcast(sales,item~channel,fun.aggregate = length)
setnames(x,c("online","onsite"),c("count_online","count_onsite"))
x


#exercise 1.3

sales[prices,on = "item"][,sum(ifelse(channel == "online",price_online * 0.95,price_onsite * 0.9),na.rm = TRUE)][]



#exercise 2.1

prices <- data.table(
  item = c("Potion", "Sword", "Shield", "Armor"),
  price_onsite = c(10, NA, 30, 100),
  price_online = c(12, 55, NA, 110)
)

sales <- data.table(
  item = c("Potion", "Sword", "Potion", "Armor", "Shield", "Sword", "Sword"),
  channel = c("onsite", "online", "online", "onsite", "onsite", "onsite", "online")
)

sales[prices,on = "item"][!is.na(price_onsite),][!is.na(price_online),][,.(number_of_items_sold = .N),by = item]


#exercise 2.2


sales <- data.table(
  item = c("Potion", "Sword", "Potion", "Armor", NA, "Sword", "Sword"),
  channel = c("onsite", "online", "online", "onsite", "onsite", "onsite", NA)
)


prices <- data.table(
  item = c("Potion", "Sword", "Shield", "Armor"),
  price_onsite = c(10, NA, 30, 100),
  price_online = c(12, 55, NA, 110)
)

sales[prices, on ="item"][!is.na(item) & !is.na(channel),sum(ifelse(channel == "online",price_online,price_onsite),na.rm = TRUE)]


#exercise 3.1

prices <- data.table(
  item = c("Potion", "Sword", "Shield", "Armor"),
  category = c("potion", "weapon", "armor", "armor"),
  price_onsite = c(10, 50, 30, 100),
  price_online = c(12, 55, 33, 110)
)

sales <- data.table(
  item = c("Potion", "Sword", "Potion", "Armor", "Shield", "Sword", "Sword"),
  channel = c("onsite", "online", "online", "onsite", "onsite", "onsite", "online")
)


sales[,.(count = .N), by = .(item)]


#Exercise 3.2: Revenue by Sales Channel and Category

sales[prices, on = "item"][,revenue := ifelse(channel == "online",price_online,price_onsite)][,.(revenue = sum(revenue)), by = .(category,channel)][]


#Exercise 3.3: Calculating the Average Revenue per Sale


sales[prices, on = "item"][,revenue := ifelse(channel == "online",price_online,price_onsite)][,.(avg_revenue = mean(revenue)),by = .(item,channel)][]



#Variation 4: Optimizing and Working with Large Data

sales[,sales_count := .N,by = .(item,channel)][1:5,order(sales_count, decreasing = TRUE),by = .(item) ][]



#Variation 5: Handling Complex Sales Scenarios



prices <- data.table(
  item = c("Potion", "Sword", "Shield", "Armor"),
  price_onsite = c(10, 50, 30, 100),
  price_online = c(12, 55, 33, 110),
  discount_day = c("Monday", "Wednesday", NA, "Friday")
)

sales <- data.table(
  item = c("Potion", "Sword", "Potion", "Armor", "Shield", "Sword", "Sword"),
  channel = c("onsite", "online", "online", "onsite", "onsite", "onsite", "online"),
  sale_day = c("Monday", "Wednesday", "Tuesday", "Friday", "Thursday", "Wednesday", "Monday")
)


sales[prices, on = "item"][,revenue := ifelse(channel == "online",price_online,price_onsite)][,.(
  revenue = ifelse(discount_day == sale_day,revenue *0.9,revenue))][,sum(revenue,na.rm = TRUE)]


#Exercise 5.2: Stock Management and Backorders

stock <- data.table(
  item = c("Potion", "Sword", "Shield", "Armor"),
  stock_level = c(10, 5, 3, 2)
)

sales <- data.table(
  item = c("Potion", "Sword", "Potion", "Armor", "Shield", "Sword", "Sword"),
  quantity = c(2, 1, 3, 1, 1, 2, 1)
)



  stock_copy <- copy(stock)
  
  for (i  in seq_len(nrow(sales))) {
    stock_copy[item == sales$item[i], stock_level := stock_level - sales$quantity[i]]
    
  }
  stock_copy[]