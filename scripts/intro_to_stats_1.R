library(tidyverse)
library(here)
library(kableExtra)

darwin <- read_csv(here("data", "Darwin.csv"))
#loading in the data from the data file

# check the structure of the data
glimpse(darwin)

# check data is in a tidy format
head(darwin)

# check variable names
colnames(darwin)


# clean up column names

darwin <- janitor::clean_names(darwin)

# check for duplication
darwin %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
darwin %>% 
  summarise(min=min(height, na.rm=TRUE), 
            max=max(height, na.rm=TRUE))

# check for typos by looking at distinct characters/values

darwin %>% 
  distinct(pair)

darwin %>% 
  distinct(type)

# missing values
darwin %>% 
  is.na() %>% 
  sum()

# quick summary

summary(darwin)

darwin %>% 
  ggplot(aes(x=type,
             y=height))+
  geom_point()
#creates a simple plot of data points of height by type of plant

darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))
#gives the mean and standard deviation by type of plant

darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))
#making a new object

darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()
#creating a summary plot of the means of each type of plant

darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")
# use kable extra functions to make a nice table (could be replaced with kable() if needed)
#tables are in viewer not in plots
# pivot data to wide format then subtract Selfed plant heights from Crossed plant heights

darwin_wide <- darwin %>% 
  pivot_wider(names_from = type, values_from = height) %>% 
  mutate(difference = Cross - Self)
#calculates the mean difference in heights between paired plants and the amount of variance (as standard deviation)

difference_summary <- darwin_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary
#gives a summary in the console 

difference_summary %>% 
  mutate(se= sd/sqrt(n))
#adds a standard error for the previously calculated standard deviation

#an idealised normal distribution
x <- seq(-4, 4, length=100)
#Create a sequence of 100 equally spaced numbers between -4 and 4

y <- dnorm(x)
#create a vector of values that shows the height of the probability distribution
#for each value in x

plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))
#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI
#working out 95% confidence interval range of estimated means
#these are printed in the console

