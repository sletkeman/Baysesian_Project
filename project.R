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
# (Intercept)                      50.04  7.401e-01  67.616  < 2e-16 ***
#   GDP                              0.0001066  1.815e-05   5.872 6.46e-09 ***
#   Adult.Mortality                 -0.01175  1.472e-03  -7.979 5.48e-15 ***
#   BMI                              0.07795  1.145e-02   6.811 1.98e-11 ***
#   Diphtheria                       0.06467  6.755e-03   9.574  < 2e-16 ***
#   HIV.AIDS                        -0.4882  2.612e-02 -18.690  < 2e-16 ***
#   Income.composition.of.resources  3.504  9.576e-01   3.659 0.000271 ***
#   Schooling                        0.9173  7.219e-02  12.706  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.355 on 755 degrees of freedom
# Multiple R-squared:  0.8239,	Adjusted R-squared:  0.8223 
# F-statistic: 504.6 on 7 and 755 DF,  p-value: < 2.2e-16

	 node	 mean	 sd	 MC error	2.5%	median	97.5%	start	sample
	BR2	-0.04285	0.05349	5.408E-4	-0.1518	-0.04101	0.05633	501	10000
	BR2adj	-0.05253	0.05399	5.459E-4	-0.1625	-0.05067	0.04757	501	10000
	beta[1]	49.11	1.799	0.01848	45.59	49.1	52.61	501	10000
	Adult.Mortality	-0.0116	0.003537	3.554E-5	-0.01848	-0.0116	-0.004586	501	10000
	BMI	0.07557	0.02724	2.706E-4	0.02264	0.07546	0.1285	501	10000
	Diphtheria	0.06326	0.01625	1.412E-4	0.03168	0.06341	0.09496	501	10000
	HIV.AIDS	-0.4865	0.06335	5.57E-4	-0.6123	-0.4864	-0.3632	501	10000
	GDP	0.0001044	4.373E-5	4.213E-7	1.869E-5	1.044E-4	1.898E-4	501	10000
	beta[7]	3.371	2.31	0.02116	-1.13	3.365	7.909	501	10000
	Schooling	0.9015	0.1729	0.00158	0.5667	0.9001	1.239	501	10000
	sigma2	111.8	5.736	0.058	101.2	111.6	123.5	501	10000

    	 node	 mean	 sd	 MC error	2.5%	median	97.5%	start	sample
	BR2	0.7681	0.01168	1.172E-4	0.7441	0.7685	0.79	4001	46500
	BR2adj	0.7659	0.01179	1.182E-4	0.7418	0.7663	0.7881	4001	46500
	beta[1]	50.03	0.8466	0.00388	48.37	50.03	51.7	4001	46500
	beta[2]	-0.01179	0.001685	7.689E-6	-0.01507	-0.01178	-0.008458	4001	46500
	beta[3]	0.07731	0.01302	6.051E-5	0.05166	0.07735	0.1027	4001	46500
	beta[4]	0.06438	0.007731	3.277E-5	0.04922	0.06438	0.07948	4001	46500
	beta[5]	-0.4951	0.03021	1.451E-4	-0.5548	-0.4952	-0.4361	4001	46500
	beta[6]	1.063E-4	2.061E-5	9.236E-8	6.586E-5	1.063E-4	1.466E-4	4001	46500
	beta[7]	3.509	1.102	0.005429	1.33	3.507	5.683	4001	46500
	beta[8]	0.9157	0.08237	3.788E-4	0.754	0.9156	1.079	4001	46500
	sigma2	24.87	1.252	0.01256	22.52	24.83	27.44	4001	46500