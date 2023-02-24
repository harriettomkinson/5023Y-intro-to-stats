#Library ----
library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)

# Data Import ----
janka <- read_csv ("data/Janka.csv")
#reading the csv file into R

head(janka)
#checking data has loaded and first 10 rows printed

#Data Check ----
colnames(janka)
#prints the column names

glimpse(janka)
#gives number of observations, both numerical and character text

janka %>%
  duplicated() %>%
  sum()
#checks for duplicate rows in the data - should be zero!

janka %>%
  summarise(min=min(dens, na.rm=TRUE),
            max=max(dens, na.rm=TRUE))
#gives the minimum and maximum density of the timber

janka %>%
  summarise(min=min(hardness, na.rm=TRUE),
            max=max(hardness, na.rm=TRUE))
#gives the minimum and maximum hardness of the timber

janka %>%
  is.na %>%
  sum()
#checks for n/a's in the data frame

summary(janka)
#produces a summary of the janka data

#Exploratory Analysis ----
janka %>%
  ggplot(aes(x=dens, y=hardness))+
  geom_point()
#plotting a simple linear plot to look for a visual linear association between
#wood density and timber hardness

with(janka, cor(dens, hardness))
#generating Pearson's R using the rstatix package

janka_ls1 <- lm(hardness ~ dens, data = janka) 
#this linear model will estimate a 'line of best fit'

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")
# specify linear model method for line fitting
#with a regression line added too

summary(janka_ls1)
#provides summary statistics for the intercept and the differrence in the mean
#from the intercept

#Mean Centered Regression ----
dens_mean <- janka %>% 
  summarise(mean_dens=mean(dens))
# 45.73333 - the centered mean calculated

janka %>% 
  mutate(centered_dens = dens-pull(dens_mean)) %>% 
  lm(hardness ~ centered_dens, data = .) %>% 
  broom::tidy()
#creates a tibble with the summary statistics 

confint(janka_ls1)
#prints confidence intervals for the intercept and density

summary(janka_ls1)
#prints summary statistics, including R^2 values

#Assumptions ----
predict(janka_ls1)
#this gives the predicted values from the model
resid(janka_ls1)
#this gives the residual values (the difference between the observed values and 
#fitted values by the model)

augmented_ls1 <- janka_ls1 %>% 
  broom::augment()
augmented_ls1 %>% 
  ggplot(aes(x=dens, 
             y=.fitted))+
  geom_line()+ 
  geom_point(aes(x=dens, 
                 y=hardness))+
  geom_segment(aes(x=dens, 
                   xend=dens, 
                   y=.fitted, 
                   yend=hardness), 
               linetype="dashed", colour="red")
#plots a black regression line and the residual values are represented by red
#dashed lines

# A line connecting all the data points in order 
p1 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_line()+
  ggtitle("Full Data")

# Plotting the fitted values against the independent e.g. our regression line
p2 <- augmented_ls1 %>% 
  ggplot(aes(x=dens, y=.fitted))+
  geom_line()+
  ggtitle("Linear trend")

# Plotting the residuals against the fitted values e.g. remaining variance
p3 <- augmented_ls1 %>% 
  ggplot(aes(x=.fitted, y=.resid))+
  geom_hline(yintercept=0, colour="white", size=5)+
  geom_line()+
  ggtitle("Remaining \npattern")

p1+p2+p3

#below, this code does the same as the code above, but in a more succinct way

model_plot <- function(data=augmented_ls1, 
                       x="dens", 
                       y="hardness", 
                       title="Full data"){
  ggplot(aes(x=.data[[x]], 
             y=.data[[y]]), 
         data=data)+
    geom_line()+
    theme_bw()+
    ggtitle(title)
}

p1 <- model_plot()
p2 <- model_plot(y=".fitted", title="Linear prediction")
p3 <- model_plot(y=".resid", title="Remaining pattern")

#Normal Distribution ---- 
plot(janka_ls1, which=c(2,2))
#plots a model diagnostic plot using the confidence intervals

#Equal Variance ----
plot(janka_ls1, which=c(1,3))
#produces standardised residuals, raw residual/standard deviation

#Outliers ----
plot(janka_ls1, which=c(4,5))
#shows any outliers in the data

#Prediction ----
coef(janka_ls1)[1] + coef(janka_ls1)[2] * 65
#using coefficients to work out the model

predict(janka_ls1, newdata=list(dens=c(22,35,65)))
#gives predicted values automatically rather than manually

#Adding Confidence Intervals ----
broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)
#for standard error

broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")
#for 95% confidence levels

emmeans::emmeans(janka_ls1, 
                 specs = "dens", 
                 at = list(dens = c(22, 35, 65)))
#emmeans package good for predicting catagorical data, can also be used for continuous data

#Prediction ----
pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65)))
janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, aes(y=.fitted, x=dens), colour="red")+
  geom_label(data=pred_newdata, (aes(y=(.fitted+10), x=(dens+3), label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,80), expand=expansion(add=c(0,5)))
#plots the 3 new predicted values onto a figure previously plotted