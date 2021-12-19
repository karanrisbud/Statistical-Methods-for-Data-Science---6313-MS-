cancer_data <- read.csv("D:/Fall'21/STATS/mini_project_6/prostate_cancer.csv",header = T, sep = ',')

# side by side plots
par(mfrow=c(1,2))
boxplot(cancer_data$psa, main = "Boxplot of PSA")
qqnorm(cancer_data$psa)
qqline(cancer_data$psa)

# side by side plots
par(mfrow=c(1,2))
boxplot(log(cancer_data$psa), main = "Boxplot of log(PSA)")
qqnorm(log(cancer_data$psa))
qqline(log(cancer_data)$psa)

#scatterplots
par(mfrow = c(2,4))
y <- log(cancer_data$psa)
plot(cancer_data$cancervol,y)
fit1 <- lm(y ~ cancervol, data = cancer_data)
abline(fit1)
plot(cancer_data$weight, y)
fit2 <- lm(y ~ weight, data = cancer_data)
abline(fit2)
plot(cancer_data$age, y)
fit3 <- lm(y ~ age, data = cancer_data)
abline(fit3)
plot(cancer_data$benpros, y)
fit4 <- lm(y ~ benpros, data = cancer_data)
abline(fit4)
plot(cancer_data$vesinv, y)
fit5 <- lm(y ~ vesinv, data = cancer_data)
abline(fit5)
plot(cancer_data$capspen, y)
fit6 <- lm(y ~ capspen, data = cancer_data) 
abline(fit6)
plot(cancer_data$gleason, y)
fit7 <- lm(y ~ gleason, data = cancer_data)
abline(fit7)


install.packages("GGally")
library(GGally)
ggpairs(data=cancer_data, columns=c(1:9), title="PSA vs all predictors")

# automated techniques for models

fit8.forward <- step(lm(y ~ 1, data = cancer_data), scope = list(upper = ~ cancervol + weight + age + benpros + as.factor(vesinv) + capspen + gleason), direction = "forward")

gbv_fit = lm(y ~ gleason + benpros + as.factor(vesinv), data = cancer_data)
anova(gbv_fit, fit8.forward)
summary(gbv_fit)$adj.r.squared


cbv_fit = lm(y ~ cancervol + benpros + as.factor(vesinv), data = cancer_data)
anova(cbv_fit, fit8.forward)
summary(cbv_fit)$adj.r.squared

cgv_fit = lm(y ~ cancervol + gleason + as.factor(vesinv), data = cancer_data)
anova(cgv_fit,fit8.forward)
summary(cgv_fit)$adj.r.squared

cgb_fit = lm(y ~ cancervol + gleason + benpros, data = cancer_data)
anova(cgb_fit, fit8.forward)
summary(cgb_fit)$adj.r.squared

summary(fit8.forward)$adj.r.squared

summary(fit8.forward)


par(mfrow=c(1,1))
plot(fitted(fit8.forward), resid(fit8.forward), main="Scatter Plot of Residuals")
abline(h = 0)

qqnorm(resid(fit8.forward))
qqline(resid(fit8.forward))


plot(resid(fit8.forward), type = 'l', main = "Time Series Plot for Residuals")
abline(h=0)


prediction = fit8.forward$coefficients["(Intercept)"] +
  (fit8.forward$coefficients["cancervol"]*mean(cancer_data$cancervol)) +
  (fit8.forward$coefficients["benpros"]*mean(cancer_data$benpros)) +
  (fit8.forward$coefficients["as.factor(vesinv)1"]*unique(cancer_data$vesinv)[which.max(tabulate(match(cancer_data$vesinv, unique(cancer_data$vesinv))))]) +
  (fit8.forward$coefficients["gleason"]*mean(cancer_data$gleason))
prediction

exp(prediction)


