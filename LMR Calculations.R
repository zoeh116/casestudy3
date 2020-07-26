kc_house_data <- read.csv("kc_house_data.csv")

clean_data <- filter(kc_house_data, price<4000000, bedrooms<10, bathrooms<6)

set.seed(250)
splitdata <- caret::createDataPartition(clean_data[,1], p = 0.8, list=F, times=1)
traindata <- clean_data[splitdata,]
testdata <- clean_data[!row.names(clean_data) %in% row.names(traindata),]


lr1 <- lm(price ~ bedrooms, data=traindata)
fitted(lr1)
resid(lr1)
summary(lr1)

print(lr1)

p1 = qplot(bedrooms, price, data=clean_data) + geom_point(colour = "#3366FF", size = 3) + geom_abline(intercept = lr1[1]$coefficients[1], slope = lr1[1]$coefficients[2], color="red")
p1

lr2 <- lm(price ~ bathrooms, data=traindata)
fitted(lr2)
resid(lr2)
summary(lr2)

print(lr2)

p2 = qplot(bathrooms, price, data=clean_data) + geom_point(colour = "#3366FF", size = 3) + geom_abline(intercept = lr2[1]$coefficients[1], slope = lr2[1]$coefficients[2], color="red")
p2


lr3 <- lm(price ~ sqft_living, data=traindata)
fitted(lr3)
resid(lr3)
summary(lr3)

print(lr3)

p3 = qplot(sqft_living, price, data=clean_data) + geom_point(colour = "#3366FF", size = 3) + geom_abline(intercept = lr3[1]$coefficients[1], slope = lr3[1]$coefficients[2], color="red")
p3

cor(clean_data$price, clean_data$bedrooms)
