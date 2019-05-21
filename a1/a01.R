bike = read.csv(file = "bike_share.csv", header = TRUE)

View(bike)

x = bike$temp
y = bike$count

plot(x, y, ylab = "Number of Bike Rentals in a Given Hourly Period", xlab = "Outdoor Temperature (in Farenheit)", main = "Number of Bike Rentals vs Outdoor Temperature")

cor(x, y)

model = lm(y ~ x)
s = summary(model)

abline(model, col = "red", lwd = 2)

beta1_hat = cor(x,y) * sd(y) / sd(x)
beta0_hat = mean(y) - beta1_hat * mean(x)
mu_hat = beta0_hat + beta1_hat * x
sigma_hat_lse = sqrt(sum((y - mu_hat)^2)/model$df.residual)

n = dim(bike)[1]
crit_val = qt(p = 0.975, df = n - 2, lower.tail = TRUE)

x0 = seq(from = min(x), to = max(x), length.out = 100)
sxx = (n-1) * var(x)

mu0_hat = beta0_hat + beta1_hat * x0
se_mu0 = sigma_hat_lse * sqrt((1/n) + ((x0-mean(x))^2/sxx))
low_CL = mu0_hat - crit_val * se_mu0
upp_CL = mu0_hat + crit_val * se_mu0

y0_hat = beta0_hat + beta1_hat * x0 # this is the same as mu0_hat above
se_y0 = sigma_hat_lse * sqrt(1 + (1/n) + ((x0-mean(x))^2/sxx))
low_PL = y0_hat - crit_val * se_y0
upp_PL = y0_hat + crit_val * se_y0

plot(x, y, ylab = "Number of Bike Rentals in a Given Hourly Period", xlab = "Outdoor Temperature (in Farenheit)", main = "Number of Bike Rentals vs Outdoor Temperature")
abline(model, col = "red", lwd = 2) #this is the fitted line

lines(x0, low_CL, col = "blue", lwd = 2, lty = 2)
lines(x0, low_PL, col = "purple", lwd = 2, lty = 2)
lines(x0, upp_CL, col = "blue", lwd = 2, lty = 2)
lines(x0, upp_PL, col = "purple", lwd = 2, lty = 2)
legend("topright", legend = c("Fitted Values", "95% CI", "95% PI"), lwd = c(2,2,2), lty = c(1,2,2), col = c("red", "blue", "purple"))

val = beta0_hat + beta1_hat * 70
se_y01 = sigma_hat_lse * sqrt(1 + (1/n) + ((70-mean(x))^2/sxx))
low_PL1 = val - crit_val * se_y01
upp_PL1 = val + crit_val * se_y01

se_beta0 = s$coefficients[1,2]
t = beta0_hat / se_beta0
p_val = 2*pt(q = abs(t), df = n - 2, lower.tail = FALSE)
print(paste("The p-value associated with Ho: beta0 = 0 is ", p_val, sep = ""))

se_beta1 = s$coefficients[2,2]
t = beta1_hat / se_beta1
p_val = 2*pt(q = abs(t), df = n - 2, lower.tail = FALSE)
print(paste("The p-value associated with Ho: beta1 = 0 is ", p_val, sep = ""))
