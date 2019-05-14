# Example script for R workshop for the turtle groups

library(ggplot2)

max <- 120
x <- runif(n = 100, min = 44, max = 115)

y <- 0.96 + 0.04 * x + rnorm(n=length(x), 0, 0.1)

mass <- exp(y)
df1 <- data.frame(length = x, mass = mass)
summary(df1)

lm1 <- lm(log(mass) ~ length, data = df1)
summary(lm1)
df1$predict <- predict(lm1)


plot(x, y)
p1 <- ggplot(data = df1, aes(x = length)) +
  geom_point(aes(y = log(mass)),
             colour = "red",
             size = 3) +
  geom_line(aes(y = predict), colour = "black") +
  theme(axis.text = element_text(size = 12)) +
  xlab("Length (cm)") +
  ylab("ln(Mass) (kg)")

p1
