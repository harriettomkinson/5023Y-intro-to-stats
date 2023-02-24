#Library ----
library(tidyverse)
library(rstatix)
library(performance)

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


