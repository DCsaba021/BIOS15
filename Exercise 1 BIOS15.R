# Exercise 1 BIOS15
# Csaba Dékány
# 16112025
# Starting RQ: How do the maternal and larval host plants influence adult weight of butterfly larvae?

#Read in the csv  file with the butterfly data set
data <- read.csv("C:/Users/dekan/Documents/Courses/Molecular Biology/BIOS15/Exercise 1/butterflies.csv")

# Produce means and standard errors 
data$MaternalHost <- paste0(data$MaternalHost, "M")
data$LarvalHost <- paste0(data$LarvalHost, "L")
means_weight <- tapply(data$AdultWeight, list(data$MaternalHost, data$LarvalHost), mean)

se <- function(x){sd(x)/sqrt(length(x))}

se_weight <- tapply(data$AdultWeight, list(data$MaternalHost, data$LarvalHost), se)

# Set the x-values for the plot
xvals <- c(1,2)

# plot an empty plot
plot(xvals, 
     means_weight[1,],
     ylim = c(45,75), 
     xaxt='n', 
     xlab = 'Larval Host', 
     type = 'n', 
     ylab = 'Adult weight (g)', 
     xlim = c(0.75,2.25))

# Plot the lines for the means of the larval weight
lines(xvals, 
      means_weight[1,], 
      type = 'b', 
      col='darkblue', 
      pch = 19)
lines(xvals, 
      means_weight[2,], 
      type = 'b', 
      col='darkorange', 
      pch = 1, 
      lty= 2)

# Plot the standard error bars for each of the points
arrows(xvals, means_weight[1,] - se_weight[1,], 
       xvals, means_weight[1,] + se_weight[1,],
       angle= 90, code = 3, length = 0.03, col = 'darkblue')
arrows(xvals, means_weight[2,] - se_weight[2,], 
       xvals, means_weight[2,] + se_weight[2,],
       angle= 90, code = 3, length = 0.03, col = 'darkorange')

#Add the legend and the x-axis 
legend('topright', legend = c('Maternal host', 'Barbarea', 'Berteroa'), pch = c(NA, 19, 1), col = c(NA,'darkblue','darkorange'), bty = 'n')
axis(1, at = xvals, labels = c('Barberea', 'Berteroa'))


# Run aov on the data 
anova(lm(formula = data$AdultWeight~data$LarvalHost*data$MaternalHost))
summary(lm(formula = data$AdultWeight~data$LarvalHost*data$MaternalHost))

# Mean weight Barbarea Larval
(means_weight[1,1]+means_weight[2,1])/2
# Mean weight Berteroa Larval
(means_weight[1,2]+means_weight[2,2])/2

# Mean weight Barbarea Maternal
(means_weight[1,1]+means_weight[1,2])/2
# Mean weight Berteroa Maternal
(means_weight[2,1]+means_weight[2,2])/2
