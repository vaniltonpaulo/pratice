#How to order things
flights.data[][order(number_of_fligth, decreasing = TRUE)]

#how to match multiple values in a row

flights.data[carrier %in%  carrier.code, , ]


#keep the collumns and rows exact the same in the left table  and making the table on the rigth match according to that

merge(first, second, by = c(z ="month",x ="origin", y ="dest"), all.x = TRUE)


#Selecting the row that....
#( this is maily used when there multiple of the same thing expample: the same rout has multiple arrival delays)

flights[flights[,.I[which.max(arr_delay)], by =.(origin,dest) ]$V1]
flights[flights[,.I[which.min(dep_delay)], by =.(origin,dest) ]$V1]

#Handling date in data table


cutoff<-ISOdate(2008,12,8)
bank.ledger[ISOdate(yr,mth,day) < cutoff ]


#How to select the highest paid in two company

#First order
salaries_data<-salaries_data[order(-Salary)]
#then
salaries_data[order(-Salary),.SD[1],by = Company]
#Alternative
salaries_data[salaries_data[,.I[1], by = Company]$V1]




#How to select the second highest paid in a company
#First order
salaries_data<-salaries_data[order(-Salary)]
salaries_data[order(-Salary),.SD[2],by = Company]
#Alternative
salaries_data[salaries_data[,.I[2], by = Company]$V1]


#You can also use .I for indexing


# Find the row index of the first person with Age > 30 in each Gender group
salaries_data[salaries_data[, .I[Age > 30 & Company =="CompanyA" ], by = Gender]$V1]


#Find out how many times a number goes into another number

x %/% y




########################################################################

#HERE WE ARE DOING A DEEP DIVE OF THE EXERCISE 


#EXERCISES 1