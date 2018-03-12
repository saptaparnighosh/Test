strength <- read.csv("C:/Users/Saptaparni/Desktop/Biostat_18/Datasets/strength/strength.csv", header = T, sep = ",")
View(strength)
names(strength)
summary(strength)
library(Hmisc)
describe(strength)


# Exploratory data analysis
table(strength$txt)
tapply(strength$age, strength$txt, mean)
tapply(strength$age, strength$txt, sd)
tapply(strength$str, strength$txt, mean)
tapply(strength$str, strength$txt, sd)

# Visual exploratory data analysis
par(mfrow = c(1,2))
boxplot(strength$age ~ strength$txt, xlab = "Intervention group", ylab = "Age, y",
        las = 1, main = "Variation of age in both treatment groups")
boxplot(strength$str ~ strength$txt, xlab = "Intervention group", ylab = "Strength",
        las = 1, main = "Variation of strength in both treatment groups")
par(mfrow = c(1,1))

plot(strength$age, strength$str, xlab = "Age, y", ylab = "Strength",
     main = "Variation of strength with age", las = 1, col = strength$txt+1, pch = 16, cex = 2)
legend(65, 35, legend = c("Control", "Treatment"), col = c("Black", "Red"), pch = 16, box.lty = 0)
lines(lowess(strength$age, strength$str), lty = 5, col = "Blue")
lines(lowess(strength$age[strength$txt == 0], strength$str[strength$txt == 0]), col = "Black", lwd = 3)
lines(lowess(strength$age[strength$txt == 1], strength$str[strength$txt == 1]), col = "Red", lwd = 3)

# Linear regression models
strenght_lm1 <- lm(str ~ age, data = strength)
summary(strenght_lm1)
strength_p1 <- predict(strenght_lm1)
plot(strength$age, strength$str, xlab = "Age, y", ylab = "Strength",
     main = "Variation of strength with age", las = 1, pch = 16, cex = 2)
lines(lowess(strength$age, strength$str), lwd = 2)
lines(strength$age, strength_p1, col = "Red", lwd = 3)

strenght_lm2 <- lm(str ~ age + txt, data = strength)
summary(strenght_lm2)
strength_p2 <- predict(strenght_lm2)
plot(strength$age, strength$str, xlab = "Age, y", ylab = "Strength",
     main = "Variation of strength with age", las = 1, col = strength$txt+1, pch = 16, cex = 2)
legend(65, 35, legend = c("Control", "Treatment"), col = c("Black", "Red"), pch = 16, box.lty = 0)
lines(lowess(strength$age[strength$txt == 0], strength$str[strength$txt == 0]), col = "Black", lty = 5)
lines(lowess(strength$age[strength$txt == 1], strength$str[strength$txt == 1]), col = "Red", lty = 5)
lines(strength$age[strength$txt == 0], strength_p2[strength$txt == 0], col = "Black", lwd = 3)
lines(strength$age[strength$txt == 1], strength_p2[strength$txt == 1], col = "Red", lwd = 3)


## Strength decreases with increase of age irrespective of the treatment group.
## However, a person in the intervention group has higher strength than a person
## in the control group when matched for age.