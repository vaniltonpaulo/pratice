#How to order things
flights.data[][order(number_of_fligth, decreasing = TRUE)]

#how to match multiple values in a row

flights.data[carrier %in%  carrier.code, , ]


#keep the collumns and rows exact the same in the left table  and making the table on the rigth match according to that

merge(first, second, by = c(z ="month",x ="origin", y ="dest"), all.x = TRUE)