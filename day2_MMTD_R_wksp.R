#day2_MMTD_R_wksp.R

# Day 2 of MMTD R workshop

# Tomo Eguchi
# 29 June 2017

# clean the workspace before do anything
rm(list=ls())

growth_data_2 <- read.csv(file = 'data/Growth data Nov 2008.csv',
                          na.strings = c('172.4*', '200+'))

growth_data_2$Date <- as.Date(growth_data_2$Date.Caught,
                               format = '%m/%d/%Y')

# one way to assign size classes
# many ways to do these tasks...
growth_data_2$Size.Class[growth_data_2$SCL > 90] <- 3
growth_data_2$Size.Class[growth_data_2$SCL <= 90 &
                           growth_data_2$SCL >= 60] <- 2
growth_data_2$Size.Class[growth_data_2$SCL < 60] <- 1

# ...

# whereas subset gets rid of NAs and that's what we want here
big.turtles <- subset(growth_data_2, SCL > 90.0)
# create a new variable called Size.Class and assign a value
big.turtles$Size.Class <- 3

middle.turtles <- subset(growth_data_2,
                        SCL >= 60.0 & SCL <= 90.0)
middle.turtles$Size.Class <- 2

little.turtles <- subset(growth_data_2, SCL < 60.0)
little.turtles$Size.Class <- 1

all.turtles <- rbind(little.turtles,
                     middle.turtles,
                     big.turtles)
summary(all.turtles)

# use the factor variable to make pretty plots:
library(ggplot2)
p1 <- ggplot(data = all.turtles) +
  geom_point(aes(x=Date, y=SCL,
                 color = Turtle.ID.Tag)) +
  geom_line(aes(x=Date, y=SCL,
                color = Turtle.ID.Tag)) +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        axis.line = element_line(color = 'red',
                                 size = 2),
        axis.line.y = element_line(size = 5,
                                   color = 'gray'),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(color = 'green'),
        axis.ticks = element_line(size = 2),
        axis.ticks.length=unit(0.8,"cm")) +
  ylab(expression(SCL^{2})) + # superscript
  xlab(expression(Date[2]))   # subscript

p1
#
# another plot between SCL and weight
all.turtles <- na.omit(all.turtles)
p2 <- ggplot(data = all.turtles,
             aes(x = SCL, y = Weight)) +
  geom_point()
p2

# p2.2 is equivalent of p2.
p2.2 <- ggplot() +
  geom_point(data = all.turtles,
             aes(x = SCL, y = Weight))

p3 <- ggplot()+
  geom_point(data = big.turtles,
             aes(x = Date, y = SCL),
             color = 'red') +
  geom_point(data = little.turtles,
             aes(x = Date, y = SCL),
             color = 'black')

p3
# remove outliers - dplyr::filter
# or create include column with TRUE/FALSE
# maybe do a linear regression ... random effects?

all.turtles.2 <- all.turtles
library(dplyr)
# find out if we can select the outliers
filter(all.turtles.2, SCL < 50 & Weight > 50)
filter(all.turtles.2, SCL > 65 & SCL < 75 & Weight > 95)

# seems like we can
# create a T/F variable - this will be used as an index
# variable later:
all.turtles.2$include <- TRUE
all.turtles.2$include[all.turtles.2$SCL < 50 &
                         all.turtles.2$Weight > 50] <- FALSE

all.turtles.2$include[all.turtles.2$SCL > 65 &
                         all.turtles.2$SCL < 75 &
                         all.turtles.2$Weight > 95] <- FALSE

all.turtles.2 <- all.turtles.2[all.turtles.2$include == TRUE,]

p4 <- p2 +
  geom_point(data = all.turtles.2,
             aes(x = SCL, y = Weight),
             color = 'red')
p4


# Intake/Discharge data - monthly boxplot
rm(list=ls())
water.temp <- read.csv('data/IntakeAndDischargeTemp.csv')
plot(water.temp$Intake, water.temp$Discharge)
water.temp <- na.omit(water.temp)
water.temp <- water.temp[water.temp$Discharge > 32,]

water.temp <- water.temp[water.temp$Discharge < 110,]
plot(water.temp$Intake, water.temp$Discharge)

# creating functions:
F2C <- function(F){
  C <- (F - 32)*(5/9)
  return(C)
}

water.temp$Date2 <- as.Date(water.temp$Date,
                            format = '%d-%b-%y %H:%M:%S')

# the following two lines do the same thing; as.Date in the first line
# is redundant because water.temp$Date2 is already in the date format
water.temp$month <- as.numeric(format(as.Date(water.temp$Date2), '%m'))
water.temp$month <- as.numeric(format(water.temp$Date2,'%m'))

water.temp$fmonth <- as.factor(water.temp$month)

p5 <- ggplot(data = water.temp) +
  geom_boxplot(aes(x = fmonth,
                   y = F2C(Intake)),
               color = 'blue',
               size = 1.5,
               alpha = 0.6) +
  geom_boxplot(aes(x = fmonth,
                   y = F2C(Discharge)),
               color = 'red',
               size = 1.5,
               alpha = 0.6) +
  xlab('Month') +
  ylab('Temperature (C)') +
  theme(axis.text = element_text(size = 12))

p5

ggsave(file = 'prettyBoxPlot.png',
       dpi = 600,
       plot = p5,
       width = 8,
       height = 10)




## making maps and overlaying data:
rm(list=ls())

infile <- 'data/CcStrandingQuery_16March2017.csv'
dat0 <- read.table(infile,
                   sep = ",",
                   header = TRUE)

# look at the data:
head(dat0)
str(dat0)
summary(dat0)
dat0.state <- dat0[dat0$State != '', ]


library(ggmap)
# get maps from the web:
West.coast <- get_map(location = c(lon = -138.0,
                                   lat = 43.0),
                      zoom = 4,
                      maptype = "satellite",
                      color = 'bw',
                      source = 'google')

So.Cal <- get_map(location = c(lon = -119.0,
                               lat = 33),
                  zoom = 7,
                  maptype = "satellite",
                  color = 'bw',
                  source = 'google')

map.west.coast <- ggmap(West.coast)
map.So.Cal <- ggmap(So.Cal)

dat0.state$Year <- as.factor(dat0.state$Year_Initially_Observed)

library(viridis)
p2 <-map.west.coast +
  geom_point(data = dat0.state,
             aes(x = Longitude, y = Latitude,
                 color = Year),
             size = 4) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.5, end = 1.0) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0),
        legend.position = c(0.15, 0.4))

dat.locs.So.Cal <- subset(dat0.state,
                          Latitude < 34.45 & Longitude > -122)
p3 <-   map.So.Cal +
  geom_point(data = dat.locs.So.Cal,
             aes(x = Longitude,
                 y = Latitude,
                 color = Year),
             size = 3) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.5,
                      end = 1.0) +
  xlab("Longitude") +
  ylab("Latitude") +
  #ggtitle("Loggerhead turtles") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10,
                                    hjust = 0.5),
        legend.text = element_text(size = 8,
                                   vjust = 0),
        legend.position = c(0.90, 0.6))
p3











