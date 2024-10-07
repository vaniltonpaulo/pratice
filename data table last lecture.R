library("data.table")

options(
  datatable.print.class = TRUE,
  datatable.print.keys = TRUE
)

# wide & long format

itemshop.prices <- rbindlist(list(
  list(item = NULL,             price.onsite = NULL, shipping.onsite = NULL, price.online = NULL, shipping.online = NULL, price.b2b = NULL, shipping.b2b = NULL),
  list("Healing Potion",        9.99,                0,                      12.99,               2.99,                   8,                2),
  list("Staff of Illusion",     18.95,               0,                      20.00,               6,                      15,               5),
  list("Lesser Stone of Mana",  2.60,                0,                      4.00,                3.5,                    2,                2),
  list("Greater Stone of Mana", 7.50,                0,                      9.99,                3.5,                    6,                2),
  list("Sword of Clarity +2",   21.50,               0,                      22.99,               6,                      20,               5)
))

itemshop.sales <- rbindlist(list(
  list(item = NULL,             channel = NULL, quantity = NULL),
  list("Healing Potion",        "online",       1),
  list("Sword of Clarity +2",   "onsite",       1),
  list("Sword of Clarity +2",   "online",       2),
  list("Sword of Clarity +2",   "onsite",       2),
  list("Sword of Clarity +2",   "b2b",          200),
  list("Greater Stone of Mana", "onsite",       3),
  list("Staff of Illusion",     "b2b",          100)
))







###########################
# Reshaping
#
# wide --> long: "melt"
#   imagine an ice cream table melting in the sun


#  ----->melt() is used to reshape wide data into long format, making it easier for analysis, grouping, and visualization.
#         -----> Use it when you have multiple columns that should instead be represented as rows for clearer,
#                        more flexible data handling.


#THIS IS JUST TO SHOW YOU HOW MELT WORKS
melt(itemshop.prices,
     id.vars = "item",  # could be more than one
     # default: everything that's not numeric/integer/logical ---> only character
     # measurement vars: default: everything else ------> This are the columns that you can specify to use
     measure.vars = c("price.onsite", "price.online", "price.b2b"),
     variable.name = "channel",  # how to name the 'variable' column; default "variable"
     value.name = "price"  # how to name the 'value' column; default 'value'
)

# otherwise: setnames(result, "value", "price")

# also: variable.factor = FALSE would leave 'channel' as `character`


# better: "patterns". ---> If you want to melt columns that have different names like price.  shipping.

#THIS IS JUST A ILUSTRATION OF THE BETTER WAY OF CODING
melt(itemshop.prices,
     id.vars = "item",
     measure.vars = patterns("^price\\.", "^shipping\\."),  # !!
     variable.name = "channel",  # ---> this will be a FACTOR in order to differentiate
     value.name = c("price", "shipping") # ---> the names of the column where the values will go
)


# is the same as above

#REAL DEAL
itemshop.prices.long <- melt(itemshop.prices,
                             id.vars = "item",
                             # named arguments to 'patterns' instead of 'value.name'
                             measure.vars = patterns(price = "^price\\.", shipping = "^shipping\\."),  # !!
                             variable.name = "channel"
)
# instead of having the collumn channel as 1,2,3 --> we rename the factor

#HOW TO RENAME FACTORS IN CHANNEL COLUMN
levels(itemshop.prices.long$channel) <- c("onsite", "online", "b2b")












# quiz: for each sold item, I want to have:
#  - gross revenue (price times units sold)
#  - total shipping (shipping times units sold)
#  - total revenue (gross revenue + shipping)


#HERE THEY JUCT USED THE MELT VERSION OF PRICE AND HAD TO COMBINE THE TWO TABLES ON TWO COLUMNS they share
itemshop.prices.long[itemshop.sales, on = c("item", "channel")][,
                                                                sum(price * quantity)]
itemshop.prices.long[itemshop.sales, on = c("item", "channel")][,
                                                                sum(shipping * quantity)]
itemshop.prices.long[itemshop.sales, on = c("item", "channel")][,
                                                                sum((price + shipping) * quantity)]

itemshop.prices.long[itemshop.sales, on = c("item", "channel")][channel == "b2b",
                                                                sum((price + shipping) * quantity)]

itemshop.prices.long[itemshop.sales, on = c("item", "channel")][,
                                                                .(total.revenue = sum((price + shipping) * quantity)), by = "channel"]

itemshop.prices.long[itemshop.sales, on = c("item", "channel")][,
                                                                .(total.revenue = sum((price + shipping) * quantity)), by = "item"]

# btw: patterns() can also be used for .SDcols

itemshop.prices[, .SD, .SDcols = patterns("^price\\.")]


###########################
# Reshaping
#
# long --> wide: "dcast"
#   "casting" is the process of putting molten metal / plastic
#   into an empty mold to give it a new shape

dcast(itemshop.prices.long,
      item ~ channel,
      value.var = "price"
)

# multiple value.var
dcast(itemshop.prices.long,
      item ~ channel,
      value.var = c("price", "shipping"),
      sep = "."
)

# suppose we had missing values:
dcast(itemshop.prices.long[1:12],
      item ~ channel,
      value.var = c("price", "shipping"),
      sep = ".",
      fill = 999  # default: NA
)


## aggregation / different levels of categorization
valuecounts <- data.table(
  chapter = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3),
  subchapter = c(0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 2),
  info = c("chars", "words", "chars", "words", "chars", "words", "chars", "words", "chars", "chars", "chars"),
  value = c(1000, 100, 2000, 200, 3000, 300, 1500, 150, 123, 234, 345)
)

valuecounts

# get information for each chapter + subchapter
dcast(valuecounts,
      chapter + subchapter ~ info
)

# aggregate??
dcast(valuecounts,
      chapter + subchapter ~ info
)[, .(words = sum(words)), by = "chapter"]

dcast(valuecounts,
      chapter ~ info,
      value.var = "value",
      fun.aggregate = sum
)

dcast(valuecounts,
      chapter ~ info,
      value.var = "value" #default fun.aggregate: length
)


dcast(valuecounts,
      chapter ~ info + subchapter,
      value.var = "value"
)

# lm(outcome ~ col1 + col2 + col3, data = ...)

# lm(outcome ~ ., data = ...)

###########################
# List columns
#

# have non-scalar information for an item in a table
dt <- data.table(
  x = c(1, 2, 3),
  y = c("a", "b", "c"),
  z = list(0, c(1, 10), c(2, 20, 200))
)

dt$z
dt$x

# data.table extracts the value as a list
# ! one of the few cases where this works !
dt[, z]
dt[, x]

# usually not so nice!
dt[, c(z, list(100))]  # :-/

# dt[, list(0, c(1, 10), c(2, 20, 200), 100)]

# compare with:
dt[, c(x, 1000)]

# otherwise behaves mostly as normal:
dt[, .(x = x[-1], y = y[-2], z = z[-3])]

dt[z == 0]  # z is a list!
vapply(dt$z, function(zval) identical(zval, 0), logical(1))
dt[vapply(z, function(zval) identical(zval, 0), logical(1))]

dt[x == 2, z := list(c(5, 4, 3, 2))]

# dt$z[dt$x == 2] <- list(c(5, 4, 3, 2))

dt

dt[x %in% c(2, 3), z := list(c(-1, -2), c(-3, -4))]

dt

dt[x %in% c(1, 3), z := list(NULL, 0)]

dt
dt$z

###########################
# common patterns
#

# common pattern: build up data.table
#  rbindlist


# do.call(rbind, <list>)
#
# rbind(l[[1]], l[[2]], l[[3]])

result <- data.table()
for (n in c(10, 100, 1000)) {
  simulation <- rnorm(n)
  newrow <- data.table(n, mean = mean(simulation), sd = sd(simulation))
  result <- rbind(result, newrow)
}

# better:
resultrows <- lapply(c(10, 100, 1000), function(n) {
  simulation <- rnorm(n)
  data.table(n, mean = mean(simulation), sd = sd(simulation))
})

resultrows

rbindlist(resultrows)

# "fill" if data has different cols:
rbindlist(list(
  data.table(x = 1),
  data.table(y = 2, x = 2)
), fill = TRUE)

# ignore names (data *must* have same number of cols)
rbindlist(list(
  data.table(x = 1),
  data.table(y = 2)
), use.names = FALSE)

# compare with:
rbindlist(list(
  data.table(x = 1),
  data.table(y = 2)
), fill = TRUE)

# common pattern: do something for subgroups


irisdt <- as.data.table(iris)

# also works, but difficult
dt <- irisdt[,
             .(result = list(
               lm(Sepal.Length ~ Sepal.Width, data = .SD)
             )),
             by = "Species"
]

dt
dt$result

# instead, use "split", + lapply (or similar)
subtbls <- split(irisdt, by = "Species")

lapply(subtbls, function(x) lm(Sepal.Length ~ Sepal.Width, data = x))

###########################
# 'env' argument
#
# variable clashes
#   ..<varname>

irisdt[, mean(Sepal.Length)]

irisdt[, mean(Sepal.Width)]

var <- "Petal.Width"

irisdt[, var, with = FALSE]

# irisdt[, mean(x),
#   env = list(x = "Petal.Width")
#  ]

x <- 1:3

x


get("x")

which.var.to.get <- "x"

get(which.var.to.get)

irisdt[, get(var)]

for (col in colnames(irisdt)[1:4]) {
  print(irisdt[, mean(get(col))])
}

###########################
# further
# setequal
# fread (fwrite)
#  - difference read.csv
# interesting functions
# - first, last
# - shift
# use the cheat sheet


