#R_Workshop_June2019

# what we did in the workshop in June 2019

# Tomo Eguchi
# June 2019

# clean the workspace before do anything
rm(list=ls())

# read the cleaned up version of our data file
growth_data_1 <- read.csv(file = 'data/Growth data Nov 2008 cleaned.csv',
                          na.strings = "")

# remove the couple weight entries - assign NAs
growth_data_2 <- read.csv(file = 'data/Growth data Nov 2008.csv',
                          na.strings = c('172.4*', '200+'))

# remove the rows with NAs in SCL and weight:
# extract the relevant columns from the original "dirty"
# data frame
growth_data_2a <- growth_data_2[, c('SCL', 'Weight')]

# find where NAs are:
growth_data_2NA <- is.na(growth_data_2a)

# sum each row:
growth_data_2sum <- rowSums(growth_data_2NA)

# extract those rows that didn't have 2 in sums:
growth_data_2b <- growth_data_2[growth_data_2sum != 2, ]

# alternatively, use !is.na? -> much simpler!!!
growth_data_2c <- growth_data_2[!is.na(growth_data_2$SCL) |
                                  !is.na(growth_data_2$Weight),]

# see if they are the same:
summary(growth_data_2b)
summary(growth_data_2c)
dim(growth_data_2b)
dim(growth_data_2c)
# seems like they are the same!

# examples of subsetting:
big.turtles <- subset(growth_data_2b, SCL > 90.0)

middle.turtle <- subset(growth_data_2b,
                        SCL > 60.0 & SCL < 100)

little.big.turtles <- subset(growth_data_2b,
                             SCL < 60.0 | SCL > 100)

# create a real data columns
growth_data_2b$Date <- as.Date(growth_data_2b$Date.Caught,
                               format = '%m/%d/%Y')

# extract one turtle
turtle.2116 <- subset(growth_data_2b,
                      Turtle.ID.Tag == '2116')

plot(turtle.2116$Date, turtle.2116$SCL, type = 'p')
plot(turtle.2116$Date, turtle.2116$SCL, type = 'l')
plot(turtle.2116$Date, turtle.2116$SCL, type = 'b')

# creating size class factor:
# an awkward way of doing it...
growth_data_2b$Size.Class <- 1
growth_data_2b$SCL[is.na(growth_data_2b$SCL)] <- -1
growth_data_2b[growth_data_2b$SCL>60.0 &
                 growth_data_2b$SCL <90.0,
               'Size.Class'] <- 2

growth_data_2b[growth_data_2b$SCL > 90.0,
               'Size.Class'] <- 3

growth_data_2b[growth_data_2b$SCL < 0, 'SCL'] <- NA

growth_data_2b[is.na(growth_data_2b$SCL), 'Size.Class'] <- NA

growth_data_2b$f.size.class <- as.factor(growth_data_2b$Size.Class)

# or use subset data frames to make it more elegant:
# create another dataframe for little ones:
little.turtles <- subset(growth_data_2c,
                         SCL < 60.0)
# also re-subset big and medium ones with factor size classes:
big.turtles <- subset(growth_data_2c, SCL > 90.0)

middle.turtles <- subset(growth_data_2c,
                        SCL > 60.0 & SCL < 100)

# create size class column:
little.turtles$Size.Class <- 1
middle.turtle$Size.Class <- 2
big.turtles$Size.Class <- 3

# then rbind all:
growth_data_2d <- rbind(little.turtles,
                        middle.turtle,
                        big.turtles)
growth_data_2d$Date <- as.Date(growth_data_2d$Date.Caught,
                               format = '%m/%d/%Y')

growth_data_2d$f.size.class <- as.factor(growth_data_2d$Size.Class)

# use the factor variable to make pretty plots:
library(ggplot2)
p1 <- ggplot(data = growth_data_2d) +
  geom_point(aes(x = Date, y = SCL))

p2 <- ggplot(data = growth_data_2d) +
  geom_point(aes(x = Date, y = SCL,
                 color = Turtle.ID.Tag))

p3 <- p2 + geom_line(aes(x = Date, y = SCL,
                color =Turtle.ID.Tag))

p4 <- p3 + theme(legend.position = 'none')

p5 <- p4 + theme(panel.background = element_blank())

p6 <- p5 + theme(axis.line = element_line(color = 'red'))

p7 <- p6 + theme(axis.line = element_line(size = 2))

p8 <- p7 + ylab("SCL (cm)") +
  xlab("Date")

p9 <- p8 + ggtitle('Change in SCL')

p10 <- p9 +theme(plot.title = element_text(hjust = 0.5))

p11 <- p10 +  theme(axis.text.x = element_text(size = 12,
                                               angle = 90),
                 axis.text.y = element_text(size = 12))

p12 <- p11 + theme(axis.title.x = element_text(size = 15))

# Let's run a linear regression analysis on SCL and mass
# if possible, always look at relationships so that
# models are appropriate.
p13 <- ggplot() +
  geom_point(data = growth_data_2d,
             aes(x = SCL, y = Weight,
                 color = Turtle.ID.Tag)) +
  theme(legend.position = 'none')

# ggplot doesn't like NAs
growth_data_2d <- na.omit(growth_data_2d)
p13 <- ggplot() +
  geom_point(data = growth_data_2d,
             aes(x = SCL, y = Weight,
                 color = Turtle.ID.Tag)) +
  theme(legend.position = 'none')

# not really linear - as expected and seems like there
# are some outliers!! Let's find them and remove:
# Learn dplyr
library(dplyr)
# first one:
filter(growth_data_2d, SCL < 50 & Weight > 50)
# second and third:
filter(growth_data_2d, SCL > 65 & SCL < 75 & Weight > 75)

# can go through by eye and remove one at a time...
growth_data_2d[growth_data_2d$Turtle.ID.Tag == 'X-129/X-130',]
# looks like just one so remove
growth_data_2d <- growth_data_2d[growth_data_2d$Turtle.ID.Tag != 'X-129/X-130',]

# not very smooth/elegant

# or create another column with index, True and False
growth_data_2d$include <- TRUE
growth_data_2d$include[growth_data_2d$SCL < 50 &
                         growth_data_2d$Weight > 50] <- FALSE

growth_data_2d$include[growth_data_2d$SCL > 65 &
                         growth_data_2d$SCL < 75 &
                         growth_data_2d$Weight > 75] <- FALSE

growth_data_2e <- growth_data_2d[growth_data_2d$include,]

p14 <- ggplot() +
  geom_point(data = growth_data_2e,
             aes(x = SCL, y = Weight,
                 color = Turtle.ID.Tag)) +
  theme(legend.position = 'none')

# take care of the non-linear part:
p15 <- ggplot() +
  geom_point(data = growth_data_2e,
             aes(x = SCL, y = log(Weight),
                 color = Turtle.ID.Tag)) +
  theme(legend.position = 'none')

# about right or ...
p16 <- ggplot() +
  geom_point(data = growth_data_2e,
             aes(x = SCL^3, y = Weight,
                 color = Turtle.ID.Tag)) +
  theme(legend.position = 'none')

# another approach is to just embrace the non-linearity
# and use GAM... a bit too much stats for this workshop
# another issue we see is the chagne in variance (residuals)
# with increasing SCL... this needs to be dealt with with
# different models... more stats!

# ignoring some offensive issues... run a basic model:
lm_lin <- lm(Weight ~ SCL, data = growth_data_2e)
summary(lm_lin)

growth_data_2e$SCL3 <- growth_data_2e$SCL^3
lm_cubed <- lm(Weight ~ SCL3, data = growth_data_2e)
summary(lm_cubed)

# but wait... repeated measures?
# mixed effects model:
library(lme4)
# individual as a random effect:
lmm_cubed <- lmer(Weight ~ SCL3 + (1|Turtle.ID.Tag),
                  data = growth_data_2e)

# centering and scaling:
growth_data_2e$scaled_SCL3 <- scale(growth_data_2e$SCL3)
growth_data_2e$scaled_Weight <- scale(growth_data_2e$Weight)
lmm_scaled_cubed <- lmer(scaled_Weight ~ scaled_SCL3 + (1|Turtle.ID.Tag),
                         data = growth_data_2e)
summary(lmm_scaled_cubed)
# enough stats for now...

###################################################3
# another dataset and practice creating functions:
rm(list=ls())
library(ggplot2)
save.fig <- F
# read the data file and clean up some ridiculous data points
data.0 <- read.csv('data/IntakeAndDischargeTemp.csv',
                   header = T)
head(data.0)
summary(data.0)

# NA needs to be removed before making the comparison below:
data.0 <- na.omit(data.0)

plot(data.0$Intake, data.0$Discharge)
data.0[data.0$Discharge < 50 | data.0$Discharge > 104,
       'Discharge'] <- NA
data.0 <- na.omit(data.0)

data.0$Date <- as.Date(data.0$Date,
                       format = '%d-%b-%y %H:%M:%S')

data.0$Month <- as.numeric(format(as.Date(data.0$Date), '%m'))
data.0$fMonth <- as.factor(data.0$Month)

# we use Celsius not Fahrenheit
# create a function that converts from F to C
F2C <- function(F){
  C <- (F - 32)*5/9
  return(C)
}

p1 <- ggplot() +
  geom_boxplot(data = data.0,
               aes(x = fMonth, y = F2C(Intake)),
               color = 'blue',
               size = 1.5,
               alpha = 0.6) +
  geom_boxplot(data = data.0,
               aes(x = fMonth, y = F2C(Discharge)),
               color = 'red',
               size = 1.1,
               alpha = 0.4) +
  #geom_hline(yintercept = 15) +
  ylab("") +
  xlab("")  +
  #ggtitle() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10, hjust = 0.5),
        legend.text = element_text(size = 8, vjust = 0),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

if (save.fig){
  ggsave(plot = p1,
         dpi = 1200,
         device = 'png',
         filename = 'images/intake_discharge.png')

}

dif.temp <- F2C(data.0$Discharge) - F2C(data.0$Intake)

mean.dif.temp <- mean(dif.temp)
#se.dif.temp <- SE(dif.temp)

#####################################################
# Another example:

rm(list = ls())

library(ggplot2)
#library(dplyr)
#library(viridis)
library(reshape)
#library(tidyr)

save.fig <- T
dat.raw <- read.delim('data/PDO_Feb2017_data.txt',
                      sep = "", header = T)
PDO.values <- melt(dat.raw, id.vars = 'YEAR')
colnames(PDO.values) <- c('Year', 'Month', 'PDO')

dt <- seq(from = 0, to = 1.0 - 1/12, by = 1/12)
uniq.period <- c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
                 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC')

PDO.values$dt <- NA
for (k in 1:length(uniq.period)){
  PDO.values[PDO.values$Month == uniq.period[k], 'dt'] <- dt[k]
}

PDO.values$time <- PDO.values$Year + PDO.values$dt
PDO.values$Pos <- ifelse(PDO.values$PDO > 0, 'TRUE', 'FALSE')

min.yr <- 1990
max.yr <- max(PDO.values$Year)
PDO.values.1990 <- subset(PDO.values,
                          Year < max.yr &
                            Year > min.yr)
DGN_bycatch_year <- c(2001, 1998, 1997, 1993, 1992)

p1 <- ggplot(data = PDO.values.1990) +
  geom_bar(stat = 'identity',
           aes(x = time, y = PDO, fill = Pos)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr,
                                  to = max.yr,
                                  by = 1)) +
  scale_fill_manual(values = c('blue', 'red'),
                    guide = FALSE) +
  ggtitle('PDO index') +
  annotate('rect', xmin = 2001, xmax = 2002,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1997, xmax = 1999,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1992, xmax = 1994,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12,
                                   angle = 90),
        axis.text.y = element_text(size = 12))

min.yr <- 1980
max.yr <- max(PDO.values$Year)
PDO.values.1980 <- subset(PDO.values,
                          Year <= max.yr & Year >= min.yr)
PDO.values.1980$Year <- as.factor(PDO.values.1980$Year)

p2 <- ggplot(data = PDO.values.1980) +
  geom_bar(stat = 'identity',
           aes(x = time, y = PDO, fill = Pos)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr,
                                  to = max.yr, by = 1)) +
  scale_fill_manual(values = c('blue', 'red'),
                    guide = FALSE) +
  ggtitle('Pacific Decadal Oscillation Index') +
  annotate('rect', xmin = 2001, xmax = 2002,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1997, xmax = 1999,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1992, xmax = 1994,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 2006, xmax = 2007,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 2014, xmax = 2016,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 11, angle = 90),
        axis.text.y = element_text(size = 12))

min.yr <- min(PDO.values$Year)
max.yr <- max(PDO.values$Year)
PDO.values$Year <- as.factor(PDO.values$Year)

p3 <- ggplot(data = PDO.values) +
  geom_bar(stat = 'identity',
           aes(x = time, y = PDO, fill = Pos)) +
  scale_x_continuous(name = '',
                     breaks = seq(from = min.yr,
                                  to = max.yr,
                                  by = 1)) +
  scale_fill_manual(values = c('blue', 'red'),
                    guide = FALSE) +
  ggtitle('Pacific Decadal Oscillation Index') +
  annotate('rect', xmin = 2001, xmax = 2002,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1997, xmax = 1999,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  annotate('rect', xmin = 1992, xmax = 1994,
           ymin = -Inf, ymax = Inf, alpha = 0.3) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 11,
                                   angle = 90),
        axis.text.y = element_text(size = 12))

if (save.fig){
  ggsave(plot = p1,
         dpi = 1200,
         file = paste0('images/PDO1990_',
                       Sys.Date(), '.png'))
  ggsave(plot = p3,
         dpi = 1200,
         file = paste0('images/PDOall_',
                       Sys.Date(), '.png'))
  ggsave(plot = p2,
         dpi = 1200,
         file = paste0('images/PDO1980_',
                       Sys.Date(), '.png'))


}

######################################################
rm(list=ls())

library(ggplot2)
library(ggmap)
library(viridis)
#library(cowplot)

save.fig <- T
internet <- T

#infile <- 'data/Stranding_Query_Loggerheads_March2017.txt'
infile <- 'data/CcStrandingQuery_16March2017.csv'
dat0 <- read.table(infile, sep = ",", header = TRUE)
# look at the data:
head(dat0)
str(dat0)
dat0.state <- dat0[dat0$State != '', ]
dat0.state$Year <- as.factor(dat0.state$Year_Initially_Observed)

p1 <- ggplot(data = dat0.state) +
  geom_bar(aes(x = Year, fill = State)) +
  #qplot(yr.fac, data = dat1.fishery, geom = "bar", fill = STATE) +
  scale_y_continuous(breaks = seq(0, 17, 1)) +
  ylab('Counts') + xlab('Year') +
  ggtitle('Stranded loggerhead turtles') +
  theme(axis.text.x = element_text(angle = 90,
                                   size = 15, vjust = 0.5))

dat1 <- subset(dat0, Alive_Released == 'FALSE' &
                 !is.na(Latitude))

dat1$yr.fac <- as.factor(dat1$Year_Initially_Observed)

dat1.size <- dat1[, c('State', 'yr.fac', 'Species_Code',
                      'Latitude', 'Longitude',
                      'Weight',
                      'Curved_Carapace_Length',
                      'Straight_Carapace_Length')]

colnames(dat1.size) <- c('State', 'Year', 'Species_Code',
                         'Latitude', 'Longitude',
                         'Weight',
                         'Curved_Carapace_Length',
                         'Straight_Carapace_Length')

# if (internet){
#   West.coast <- get_map(location = c(lon = -138.0,
#                                      lat = 43.0),
#                         zoom = 4,
#                         maptype = "satellite",
#                         color = 'bw',
#                         source = 'google')
#   saveRDS(West.coast,
#           file = 'RData/CC_stranding_westcoast.rds')
# 
#   So.Cal <- get_map(location = c(lon = -119.0,
#                                  lat = 33),
#                     zoom = 7,
#                     maptype = "satellite",
#                     color = 'bw',
#                     source = 'google')
#   saveRDS(So.Cal,
#           file = 'RData/CC_stranding_SoCal.rds')
# } else {
  # West.coast <- readRDS(file = 'RData/CC_stranding_westcoast.rds')
  # SoCal <- readRDS(file = 'RData/CC_stranding_SoCal.rds')
#   print('read from rds files')
# }

# map.west.coast <- ggmap(West.coast)
# map.So.Cal <- ggmap(So.Cal)
# 
# p2 <-map.west.coast +
#   geom_point(data = dat1.size,
#              aes(x = Longitude, y = Latitude,
#                  color = Year),
#              size = 4) +
#   scale_color_viridis(discrete = TRUE,
#                       begin = 0.5, end = 1.0) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title = element_text(size = 10, hjust = 0.5),
#         legend.text = element_text(size = 8, vjust = 0),
#         legend.position = c(0.15, 0.4))
# 
# dat.locs.So.Cal <- subset(dat1.size,
#                           Latitude < 34.45 & Longitude > -122)
# p3 <-   map.So.Cal +
#   geom_point(data = dat.locs.So.Cal,
#              aes(x = Longitude,
#                  y = Latitude,
#                  color = Year),
#              size = 3) +
#   scale_color_viridis(discrete = TRUE,
#                       begin = 0.5,
#                       end = 1.0) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   #ggtitle("Loggerhead turtles") +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.title = element_text(size = 10,
#                                     hjust = 0.5),
#         legend.text = element_text(size = 8,
#                                    vjust = 0),
#         legend.position = c(0.90, 0.6))
# 
# dat.size <- na.omit(data.frame(Year = dat1.size$Year,
#                                CCL = dat1.size$Curved_Carapace_Length,
#                                state = dat1.size$State))
# p4 <- ggplot() +
#   geom_histogram(data = dat.size,
#                  aes(x = CCL),
#                  binwidth = 5,
#                  color = 'black',
#                  fill = 'white') +
#   xlab(expression(CCL[cm])) +
#   ylab('Frequency') +
#   ggtitle('USA') +
#   xlim(10, 100) +
#   # scale_x_discrete()
#   theme(axis.title.y = element_text(size = 12),
#         axis.text.y = element_text(size = 12))
# 
# if (save.fig){
#   ggsave(filename = paste0('images/Cc_strandings_',
#                            Sys.Date(), '.png'),
#          plot = p1,
#          width = 8,
#          height = 7,
#          dpi = 1200)
# 
#   ggsave(filename = paste0('images/Cc_strandings_westcoast_',
#                            Sys.Date(), '.png'),
#          plot = p2,
#          width = 9.4,
#          height = 8.4,
#          dpi = 1200)
# 
#   ggsave(filename = paste0('images/Cc_strandings_SCB_',
#                            Sys.Date(), '.png'),
#          plot = p3,
#          width = 9.4,
#          height = 8.4,
#          dpi = 1200)
# 
# }
