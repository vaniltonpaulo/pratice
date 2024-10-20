# data table exercises

#THIS EXERCISES ARE RELATED TO DATA TABLE UB 1 AUFGABE 2) - 3

employees <- data.table(
  employee_id = 1:10,
  department = c("HR", "HR", "IT", "IT", "Finance", "Finance", "Sales", "Sales", "IT", "Sales"),
  salary = c(50000, 55000, 60000, 62000, 57000, 53000, 48000, 52000, 61000, 49000)
)

employees[,{
  dt <-.SD
  .SD[,.(
      employe.mean = salary,
      others.mean = dt[employee_id != .BY$employee_id, mean(salary,na.rm = TRUE)]
      
    ), by = employee_id]
},by = department]




city_temps <- data.table(
  city = c("New York", "New York", "LA", "LA", "Chicago", "Chicago"),
  date = as.IDate(c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02", "2024-01-01", "2024-01-02")),
  temperature = c(32, 30, 75, 74, 28, 27)
)

city_temps[,{
  dt <-.SD
  .SD[,.(
    city.mean = mean(temperature, na.rm = TRUE),
    other.city = dt[city != .BY$city, mean(temperature, na.rm = TRUE)]
  ), by = city ]
}, by = date]