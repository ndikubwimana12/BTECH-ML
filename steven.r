house = read.csv('Housing.csv')
str(house)
head(house)
View(house)
is.na(house)
dim(house)
nrow(house)
sapply(house,class)
summary(house)
sum(is.na(house))
colSums(is.na(house))
sum(duplicated(house))
table(house$Neighborhood)

boxplot(house$price,
main="House price vs area distribution",
xlab="Area",
ylab="price",
pch=19,
col="blue"
)

boxplot(house$area, house$price,
     main = "Price vs Area",
     xlab = "Area",
     ylab = "Price",
     pch = 19,
     col = "blue")
