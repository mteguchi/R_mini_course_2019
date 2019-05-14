#ExampleReadFile.R

# Shows an example of reading data file into R

# Tomo Eguchi
# 27 June 2017

# clean the workspace before do anything
rm(list=ls())

# read the cleaned up version of our data file
growth_data_1 <- read.csv(file = 'data/Growth data Nov 2008 cleaned.csv',
                          na.strings = "")

growth_data_2 <- read.csv(file = 'data/Growth data Nov 2008.csv',
                          na.strings = c('172.4*', '200+'))

# we are trying to remove rows with NAs in SCL and Weight
# columns

# find levels in Weight variable
# levels(growth_data_2$Weight)
#
# # offensive ones are "172.4*' and '200+'
# growth_data_2[grep('[*]', growth_data_2$Weight),
#               'Weight'] <- NA
# growth_data_2[grep('[+]', growth_data_2$Weight),
#               'Weight'] <- NA
#
# growth_data_2$Weight2 <- as.numeric(growth_data_2$Weight)

# extract the relevant columns from the original "dirty"
# data frame
growth_data_2a <- growth_data_2[, c('SCL', 'Weight')]

# find where NAs are:
growth_data_2NA <- is.na(growth_data_2a)

# sum each row:
growth_data_2sum <- rowSums(growth_data_2NA)

growth_data_2b <- growth_data_2[growth_data_2sum != 2, ]

big.turtles <- subset(growth_data_2b, SCL > 90.0)

middle.turtle <- subset(growth_data_2b,
                        SCL > 60.0 & SCL < 100)

little.big.turtles <- subset(growth_data_2b,
                             SCL < 60.0 | SCL > 100)

growth_data_2b$Date <- as.Date(growth_data_2b$Date.Caught,
                               format = '%m/%d/%Y')

turtle.2116 <- subset(growth_data_2b,
                      Turtle.ID.Tag == '2116')

plot(turtle.2116$Date, turtle.2116$SCL, type = 'p')
plot(turtle.2116$Date, turtle.2116$SCL, type = 'l')
plot(turtle.2116$Date, turtle.2116$SCL, type = 'b')

growth_data_2b$Size.Class <- 1
growth_data_2b$SCL[is.na(growth_data_2b$SCL)] <- -1
growth_data_2b[growth_data_2b$SCL>60.0 &
                 growth_data_2b$SCL <90.0,
               'Size.Class'] <- 2

growth_data_2b[growth_data_2b$SCL > 90.0,
               'Size.Class'] <- 3

growth_data_2b[growth_data_2b$SCL <0, 'SCL'] <- NA

growth_data_2b[is.na(growth_data_2b$SCL), 'Size.Class'] <- NA

growth_data_2b$f.size.class <- as.factor(growth_data_2b$Size.Class)

# To Do:
# 1. better way to remove rows with two NAs (!is.na)
growth_data_3 <- growth_data_2[!is.na(growth_data_2$SCL) |
                                 !is.na(growth_data_2$Weight),]

growth_data_4 <- growth_data_2[!is.na(growth_data_2$SCL) &
                                 !is.na(growth_data_2$Weight),]
# the following two lines do the same thing as Line 79 using
# dplyr package's filter function
library(dplyr)
growth_data_5 <- filter(growth_data_2,
                        !is.na(SCL) & !is.na(Weight))

# 2. better way to create size classes using rbind
# if using indexing, NAs remain
growth_data_2[growth_data_2$SCL > 90, ]

# whereas subset gets rid of NAs and that's what we want here
big.turtles <- subset(growth_data_2, SCL > 90.0)
big.turtles$Size.Class <- 3

middle.turtles <- subset(growth_data_2,
                        SCL >= 60.0 & SCL <= 90.0)
middle.turtles$Size.Class <- 2

little.turtles <- subset(growth_data_2, SCL < 60.0)
little.turtles$Size.Class <- 1

# what if we had a different column name:
little.turtles.2 <- little.turtles
colnames(little.turtles.2) <- c('a', 'b', 'c', 'd', 'e')

all.turtles <- rbind(little.turtles,
                     middle.turtles,
                     big.turtles)
summary(all.turtles)

# the following does not work becausee column names are
# different.
all.turtles <- rbind(little.turtles.2,
                     middle.turtles,
                     big.turtles)

summary(all.turtles)

# use the factor variable to make pretty plots:
# ggplot package Date vs. SCL
# remove legend
# background grids -remove
# color by individuals
# x and y axis labels
# font size, angles
#
# another plot between SCL and weight
# remove outliers - dplyr::filter
# or create include column with TRUE/FALSE
# maybe do a linear regression ... random effects?
#
# Intake/Discharge data - monthly boxplot
# creating functions F2C
# creating date variable
# extracting months
#













