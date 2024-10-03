#How to order things
flights.data[][order(number_of_fligth, decreasing = TRUE)]

#how to match multiple values in a row

flights.data[carrier %in%  carrier.code, , ]