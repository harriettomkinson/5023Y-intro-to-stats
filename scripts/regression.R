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