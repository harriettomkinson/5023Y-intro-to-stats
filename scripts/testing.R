# Student's t-test ----
x <- seq(-4, 4, length=100)
#sets the length and limits of the x axis
hx <- dnorm(x)
#plots a line for a normal distribution

degf <- c(1, 3, 8, 30)
#sets degrees of freedom
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")
#colours and labels for the plot

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")
#creates the plot and adds in labels

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}
#there is 4 degrees of freedom

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)
  #adds in a key explaining what each line is

#Critical Values ----
#a plot showing the values for critical t at each degree of freedom up to 30

df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

#Summary Stats ----
lsmodel1 <- lm(height ~ type, data = darwin)
summary(lsmodel1)
tidy_model1 <- broom::tidy(lsmodel1)
tidy_model1[[2,2]] / tidy_model1[[2,3]]
#creates some summary stats for the darwin data

#Paired T ----
lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)
summary(lsmodel_darwin)
#now intercept is the height of crossed plants from pair 1

lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(1:2) 
  #just show first two rows
#generating confidence intervals for the paired t-test

m1 <- lm(height ~ type, data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="unpaired")
  #creates the unpaired line for our plot

m2 <- lm(height ~ type + factor(pair), data = darwin) %>% 
  broom::tidy(., conf.int=T) %>% 
  slice(2:2) %>% 
  mutate(model="paired")
  #creates the paired line for our plot

rbind(m1,m2) %>% 
  #combines the two 
  ggplot(aes(model, estimate))+
  geom_pointrange(aes(ymin=conf.high, ymax=conf.low))+
  #sets the range for the x axis
  geom_hline(aes(yintercept=0), linetype="dashed")+
  theme_minimal()+
  coord_flip()
  #sets aesthetics for the plot