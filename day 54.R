DT <- as.data.table(iris)
iris2snake_case <- function(name) {
  name <- tolower(name)
  gsub("\\.", "_", name)
}

setnames(DT, iris2snake_case(names(DT)))

set.seed(99)

DT[sample(.N, 75),
   .(mean_sepal_length = mean(sepal_length)),
   by = species]


DT[, lapply(.SD, uniqueN)]
DT[, lapply(.SD, mean), by = species]
DT[sample(.N, 10), lapply(.SD, max), by = species]

DT[,.SD[3],by= species]

DT[order(sepal_length,decreasing = TRUE),head(.SD,2), by = species]
DT[order(sepal_length,decreasing = TRUE),.SD[1:2], by = species]



DT[order(sepal_length - sepal_width),
   head(.SD, 2),
   by = species]


DT[,.SD[which(sepal_length > mean(sepal_length))], by = species]

DT[,.SD[.N > 10], by = species]

DT[,.SD[which.max(petal_length)], by = species]

DT[, .(exact.row.indices =.I[petal_length == max(petal_length)]), by = species]