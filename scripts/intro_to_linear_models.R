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
#outputs a table of a range of values in addition to an intercept

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height))
#confirms the fact that selfed plants are an average of 2.6 inches shorter

summary(lsmodel1)
#fuller summary of the model

darwin %>% 
  ggplot(aes(x=type, 
             y=height,
             colour=type))+
  geom_jitter(alpha=0.5,
              width=0.1)+
  stat_summary(fun=mean,
               size=1.2)+
  theme_bw()
#models superimposed the calculated means onto a plot

confint(lsmodel1)
#provides confidence intervals based on the ls model

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)
#tests Darwin's null hypothesis of self-pollination would reduce fitness
#by using height as a proxy
#tests whether predicted value lies within 95% confidence level for the difference of the mean

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)
#outputs a summary table

darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()
#calculating the "other" mean and outputting a table

means <- emmeans::emmeans(lsmodel1, specs = ~ type)
means
#provides mean, s.e and 95% confidence level estimates of all levels of model at once

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))
#outputs a summary plot using the data from the emmeans model

performance::check_model(lsmodel1)
#provides assumption checking for:
#1. that residual variance in our data is approx. normally distributed
#2. that resdidual variance is approx. equal between groups

performance::check_model(lsmodel1, check=c("normality","qq"))
plot(lsmodel1, which=c(2,2))
#outputs a plot of the Darwin data to check the data's normality

performance::check_model(lsmodel1, check="homogeneity")
plot(lsmodel1, which=c(1,3))
#checks for equal variances of the data

performance::check_model(lsmodel1, check="outliers")
plot(lsmodel1, which=c(4,4))
#checks for outliers

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)
#outputs a plot which summarises the Darwin data