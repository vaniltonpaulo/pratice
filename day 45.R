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

sales[prices,on = "item"][,sum(ifelse(channel == "online",price_onsite * 0.9,price_online * 0.95))][]