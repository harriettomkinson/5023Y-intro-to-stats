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

