library("Rarity")

set.seed(0)
setwd("/Users/sletkeman/Baysesian_Project/")
data <- read.csv("Life Expectancy Data.csv")
summary(data)

num_data = data[, 4:22]
corPlot(num_data, method = "pearson")

# random train test split
#smp_size = nrow(data) * 0.75
#train_ind <- sample(seq_len(nrow(data)), size = smp_size)
#train <- data[train_ind, ]
#test <- data[-train_ind, ]

filtered_data = data[!(is.na(data$Life.expectancy) | is.na(data$GDP) | is.na(data$Adult.Mortality) | is.na(data$BMI) | is.na(data$Diphtheria) | is.na(data$HIV.AIDS) | is.na(data$Income.composition.of.resources) | is.na(data$Schooling)),]

year = 2004
train <- filtered_data[filtered_data$Year <= year,]
test <- filtered_data[filtered_data$Year > year,]
write.csv(train, "train.csv")

model = lm(Life.expectancy~GDP+Adult.Mortality+BMI+Diphtheria+HIV.AIDS+Income.composition.of.resources+Schooling, data=train)
summary(model)

conf = predict(model, test, interval = "confidence", level = 0.99)
pred = predict(model, test)
summary(pred)

plot(pred, test$Life.expectancy, xlab="predicted", ylab="actual")
abline(a=0, b=1)

inbounds = test$Life.expectancy > conf[,2] & test$Life.expectancy < conf[,3]
summary(inbounds)


mean(abs(test$Life.expectancy - conf[,1]))

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                      5.004e+01  7.401e-01  67.616  < 2e-16 ***
#   GDP                              1.066e-04  1.815e-05   5.872 6.46e-09 ***
#   Adult.Mortality                 -1.175e-02  1.472e-03  -7.979 5.48e-15 ***
#   BMI                              7.795e-02  1.145e-02   6.811 1.98e-11 ***
#   Diphtheria                       6.467e-02  6.755e-03   9.574  < 2e-16 ***
#   HIV.AIDS                        -4.882e-01  2.612e-02 -18.690  < 2e-16 ***
#   Income.composition.of.resources  3.504e+00  9.576e-01   3.659 0.000271 ***
#   Schooling                        9.173e-01  7.219e-02  12.706  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.355 on 755 degrees of freedom
# Multiple R-squared:  0.8239,	Adjusted R-squared:  0.8223 
# F-statistic: 504.6 on 7 and 755 DF,  p-value: < 2.2e-16

