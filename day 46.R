flights <- data.table(
  flight_id = 1:6,
  year = c(2013, 2014, 2014, 2014, 2013, 2014),
  month = c(1, 2, 3, 3, 1, 2),
  origin = c("JFK", "JFK", "LAX", "LAX", "SFO", "JFK"),
  dest = c("LAX", "LAX", "JFK", "SFO", "LAX", "SFO")
)

specify_year <- function(flights,yr){
  flights[yr ==year,.(number_of_fligths = .N), by = .(month)]
}

specify_year(flights,2013)
specify_year(flights,2014)

route <- function(flights){
  flights[,.(number_of_fligths = .N), by = .(month,origin,dest)]
}

route(flights)
route(flights)



#Exercise 3
flights <- data.table(
  flight_id = 1:6,
  year = c(2014, 2014, 2014, 2014, 2014, 2014),
  month = c(1, 2, 3, 3, 1, 2),
  origin = c("JFK", "JFK", "LAX", "LAX", "SFO", "JFK"),
  dest = c("LAX", "LAX", "JFK", "SFO", "LAX", "SFO"),
  arr_delay = c(30, 90, 120, 20, 40, 15)
)

# Function to filter flights with arrival delays greater than 60 minutes
filter_flights_with_delays <- function(flights) {
  flights[arr_delay>60]
  
}

filter_flights_with_delays(flights)




# Function to count flights until a delay greater than 60 minutes occurs
count_flights_until_delay <- function(flights) {
  flights[, .(flights_to_delay = which(arr_delay > 60)[1] - 1), by = .(month, origin, dest)]
  
}
count_flights_until_delay(flights)



#new exercise with .SD

#Exercise 1
# Sample data
flights <- data.table(
  flight_id = 1:10,
  carrier = rep(c("AA", "DL", "UA"), length.out = 10),
  arr_delay = c(30, 60, 10, 120, 15, 45, 5, 90, 20, 35),
  dep_delay = c(20, 45, 5, 130, 10, 35, 0, 85, 15, 25),
  distance = c(1000, 2000, 1500, 1800, 900, 1200, 1700, 1100, 1600, 900)
)

flights[,lapply(.SD,mean),by = carrier, .SDcols = is.numeric]


#Exercise 1.2: Filter Rows Within .SD

flights[,.SD[arr_delay > 60],by = carrier]


#Exercise 2: Intermediate .SD Operations

#Exercise 2.1: Calculate the Range of Delays by Group

flights[,.SD[,.(arr_range = max(arr_delay) - min(arr_delay))],by = carrier]

#Exercise 2.2: Rank Flights Within Groups

flights[,.(arr_delay),by = carrier][order(arr_delay,decreasing = TRUE)]




# Create a data table with sample salary data
salary_data <- data.table(
  EmployeeID = 1:10,
  Name = c("John", "Jane", "Alice", "Bob", "Charlie", "David", "Eve", "Frank", "Grace", "Henry"),
  Age = c(34, 28, 29, 41, 36, 45, 32, 38, 25, 30),
  Gender = c("M", "F", "F", "M", "M", "M", "F", "M", "F", "M"),
  Department = c("IT", "HR", "Finance", "IT", "HR", "Finance", "IT", "HR", "Finance", "IT"),
  Salary = c(60000, 50000, 52000, 75000, 48000, 65000, 55000, 49000, 53000, 62000),
  YearsAtCompany = c(5, 3, 4, 10, 2, 8, 6, 3, 1, 7)
)

#What is the average salary of employees across the company?
salary_data[,lapply(.SD,mean),.SDcols = c("Salary")]

#Which department has the highest average salary?
salary_data[,lapply(.SD,mean),.SDcols = c("Salary"), by = Department][1,1:2]

#How does the average salary differ between males and females?
salary_data[,lapply(.SD,mean),.SDcols = c("Salary"), by = Gender]

#Who has the highest salary in the company?
salary_data[,lapply(.SD,mean),.SDcols = c("Salary"), by = Name][1,1:2]

#What is the average age of employees across different departments?
salary_data[,lapply(.SD,mean),.SDcols = c("Age"), by = Department]

#Is there any correlation between the number of years at the company and salary?

salary_data[,.(YearsAtCompany,Salary),by = Name][order(YearsAtCompany,Salary,decreasing = TRUE)]
#there is a weak corrolation

#How many employees are in each department?

salary_data[,.(number.of.employees = .N),by = Department]


#Who has been working the longest at the company?
salary_data[max(YearsAtCompany)]

#What is the average salary for employees who have been with the company for more than 5 years?

salary_data[YearsAtCompany > 5, .(avg.salary = mean(Salary))]


#What is the salary range (min, max) in each department?
salary_data[,.(min.salary = min(Salary),max.salary = max(Salary)),by =Department ]



#Advanced Questions

#What is the median salary for employees who are under 35 and have been with the company for more than 3 years?

salary_data[YearsAtCompany > 3 & Age <35,.(median.salary = as.numeric(median(Salary)))]
  


#Calculate the ratio of male to female employees in each department. Which department has the most gender imbalance?

gender_count_by_department <- salary_data[,.(Count = .N),by=.(Department,Gender)]

gender_count_wide <- dcast(gender_count_by_department, Department ~ Gender, value.var = "Count", fill = 0)

gender_count_wide[,.(ratio = M/F)]


#Which employee(s) have a salary above the department’s average for their respective department?
salary_data[,avg.salary:=mean(Salary),by = Department][Salary >avg.salary][,Name]



#For employees who have been with the company for 5 years or less, what is the average salary by 
#gender, and how does it compare to employees who have been with the company for more than 5 years?


result<-salary_data[YearsAtCompany < 5,.(avg.five.less = mean(Salary)),by = Gender]
result.2 <-salary_data[YearsAtCompany > 5,.(avg.five.more = mean(Salary)),by = Gender]
merge(result,result.2,by = "Gender",all.x = TRUE)

