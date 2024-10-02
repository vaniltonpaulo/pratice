prices <- data.table(
  item = c("Potion", "Sword", "Shield", "Armor"),
  price_onsite = c(10, 50, 30, 100),
  price_online = c(12, 55, 33, 110)
)

sales <- data.table(
  item = c("Potion", "Sword", "Potion", "Armor", "Shield", "Sword", "Sword"),
  channel = c("onsite", "online", "online", "onsite", "onsite", "onsite", "online")
)