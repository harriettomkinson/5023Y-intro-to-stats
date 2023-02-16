library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
#library commands loaded in

lsmodel0 <- lm(formula = height ~ 1, data = darwin)
#least squares model plot of the heights of the plants

summary(lsmodel0)
#outputs a table of coefficients of the darwin data

mean(darwin$height)
#outputs the mean height of the plants

lsmodel1 <- lm(height ~ type, data=darwin)
#fits the data as a linear model
broom::tidy(lsmodel1)

