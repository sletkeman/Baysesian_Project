data <- read.csv("cardekho_cleaned.csv")
summary(data)

smp_size = nrow(data) * 0.75

train <- sample(seq_len(nrow(data)), size = smp_size)

train <- data[train_ind, ]
test <- data[-train_ind, ]

model = lm(selling_price~vehicle_age+km_driven+mileage+engine+max_power+seats, data=train)
summary(model)