#Ecostats 25 Nov 2026




install.packages("agridat")
library(agridat)
library(tidyverse)
library(DHARMa)

library(emmeans)

# Using a sample dataset from the agridat package

apples <- agridat::archbold.apple


hist(apples$yield)


lpi_data <- read.csv("CC-8-Modelling-master/shagLPI.csv")

hist(lpi_data$pop)


hist(mtcars$wt)


hist(mtcars$carb)


apples_2 <- apples %>%
  mutate(spacing = as.factor(spacing))

str(apples_2)


apple_mod <- lm(yield ~ spacing, data = apples_2)
plot(apple_mod)


plot(simulateResiduals(apple_mod))


summary(apple_mod)


pairs(emmeans(apple_mod, ~spacing))



ggplot(apples_2, aes(x = spacing, y = yield, fill = spacing)) + 
  geom_boxplot() +
  theme_classic()



View(ilri.sheep)
str(ilri.sheep)


hist(ilri.sheep$weanwt)


sheep_mod <- lm(weanwt ~ weanage*sex, data = ilri.sheep)
plot(simulateResiduals(sheep_mod))


summary(sheep_mod)


