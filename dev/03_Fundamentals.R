#----------------------------------------------------------------------------
# Fundamentals code
#----------------------------------------------------------------------------


# Data types --------------------------------------------------------------

# Numeric

dow.jones <- 35490.69

dow.jones

class(dow.jones)

is.numeric(dow.jones)

is.double(dow.jones)

# Integer

as.integer(dow.jones)

as.integer(round(dow.jones))

# In-place 

dow.jones

dow.jones <- as.integer(round(dow.jones))

dow.jones

# Character

index.1 <- "Dow Jones"

index.1

index.2 <- 'S&P 500'

index.2

class(index.1)

is.character(index.2)

# Factor

regions <- factor(c("NE", "SE", "NE", "SW", "SE", "NW", "NW", "SW"))

class(regions)

str(regions)

levels(regions)

summary(regions)

# Boolean / logical 

logicals <- c(TRUE, FALSE, TRUE)

sum(logicals)

mean(logicals)

# Date

date.1 <- as.Date("1991-01-03")

class(date.1)

date.2 <- as.Date("11/2/1958", format = "%m/%d/%Y")

date.2

library(lubridate)

date.3 <- mdy("6/2/1987")

date.3


# Data structures ---------------------------------------------------------

# Vectors

## Created using the concatenate function c()
(a <- c(1, 3, 5, 7, 9))

## Created using a sequence
(b <- 1:5)

## A character vector
(c <- c("a", "b", "c", "d", "e"))

## A logical vector
(d <- c(TRUE, FALSE, FALSE, TRUE, TRUE))

## Vector arithmetic
(e <- a + b)

# Matrices

(mat <- cbind(a, b, e))

class(mat)

# Access the observation in row 3, column 2
mat[3,2]

# Access all observations in rows 1-3 and columns 2-3
mat[1:3,2:3]

# Access all rows in columns 1 and 3; in other words, access columns  1 and 3
mat[,c(1,3)]

(mat <- cbind(mat, c))

# Data frame

df <- data.frame(
  a = a, 
  b = b, 
  c = c, 
  d = d, 
  e = e
)

df

summary(df)

df[1:3,2:3]

df[, c("a", "c")]

df$b

# List

my.list <- list(
  index.1, 
  regions, 
  a, 
  df
)

my.list

my.list[[4]]


# Functions ---------------------------------------------------------------

# Create vector
f <- c(2.23, 5.76, 9.12, 4.54, 3.33)

# Round to zero decimals
round(f)

# Round to one decimal
round(f, digits = 1)

# Use argument names
round(x = f, digits = 1)

# Drop argument names
round(f, 1)

# Use the detail argument name
round(f, digits = 1)

# Help with functions
help(round)

?round()

# Create function
calculate_ctax <- function(x, rate = .21){
  
  x * rate
  
}

calculate_ctax(100)

calculate_ctax(100, rate = .125)

firms.income <- c(100, 95, 70, 150, 50)

calculate_ctax(firms.income)


# Comparisons -------------------------------------------------------------

# Greater than 
10 > 5

# Less than 
10 < 5

# Greater than or equal to
10 >= 5

# Less than or equal to
10 <= 5

# Equal to 
10 == 5

# Not equal to
10 != 5


# Conditional logic -------------------------------------------------------

# Store the individual's income value
income <- 100

# Implement conditional logic
if(income >= 100){
  
  income * .3
  
} else(
  
  income * .1
)

# Conditional logic on data frame

df

df$f <- ifelse(df$e > 10, 1, 0)

df

df$g <- ifelse(df$e > 10 & df$a == 9, 1, 0)

df$h <- ifelse(df$e > 10 | df$a == 9, 1, 0)

df

# DF exercise

firms_df <- data.frame(
  firm_name = LETTERS[1:10],
  revenues = round(runif(10, 90, 110), 2),
  expenses = round(runif(10, 50, 120), 2)
)

firms_df

firms_df$ptni <- firms_df$revenues - firms_df$expenses

firms_df$tax <- calculate_ctax(firms_df$ptni)

firms_df$atni <- firms_df$ptni - firms_df$tax

summary(firms_df)

# Column mean
colMeans(firms_df[-1])

median(firms_df$revenue)

median(firms_df$expenses)


# Iteration ---------------------------------------------------------------

# Create a pre-allocation vector
firms.med <- vector(mode = "numeric", length = ncol(firms_df[-1]))

# Execute median loop
for (i in seq_along(firms_df[-1])) {
  firms.med[i] <- median(firms_df[-1][[i]])
}

# View the output
firms.med

# Assign names of variables to median vector
names(firms.med) <- colnames(firms_df[-1])

# Round and display the medians
round(firms.med, 2)

# Apply as alternative
apply(firms_df[-1], 2, median)

# Column bind loops
round(
  cbind(mean = colMeans(firms_df[-1]), 
        median = apply(firms_df[-1], 2, median),
        sd = apply(firms_df[-1], 2, sd)))


# Errors ------------------------------------------------------------------

dow.jones + index.1