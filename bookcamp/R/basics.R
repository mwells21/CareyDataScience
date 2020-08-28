a = 6

# We can combine some values into a vector using the c() function
x = c(1, 3, 2, 5, 4)
x

# we can even add some values to a vector
y = c(0, x, 5)
y

# Getting the first value of x 
x[1]

# fourth item
x[4]

# x has only 5 values
x[6]

# getting more than one item 
x[1,4,2]

# Try this instead
x[c(1,4,2)]

k=2:4

x[k]

# multiply 2 and y
y
2*y 

# multiply z and y
z=3:9
z
y
z*y


# test whether z >= 4, create a logical vector
z>=4

# and more conditions
z<6

# & is an and parameter
z>=4 & z<6

# | is an or parameter
z<4 | z>7

# == checks whether two things are exactly the same or not
# = is different than ==
z==3

# != is the oposite of ==
z!=5

cond = z!=5

z[cond]


# load the tidyverse library to be able to 
# use some visualization and preprocessing functions
library(tidyverse)

# read the time_series_covid19_deaths_US.csv file and save on the raw_data variable
filename = "time_series_covid19_deaths_US.csv"
raw_data = read_csv(filename)

# Dimension of the data
dim(raw_data)
nrow(raw_data)
ncol(raw_data)

# Let's look at a few rows of the data
head(raw_data)

# Let's look at a all rows of the data
View(raw_data)

# brings back the value in row 2 and column 6
raw_data[2,6]

# brings back all the values of the sixth column
raw_data[,6]

# brings back all the values of the Province_State column
raw_data$Province_State

# brings back the second row
raw_data[2,]

# brings back the second row
raw_data[2,c(5,6)]

# brings back the second row except for the fourth column
raw_data[2,-4]



