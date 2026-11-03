library(tidyverse)
df <- mtcars

dim(df)
View(df)

colnames(df)

m <- lm(mpg ~ cyl+hp+wt+carb+am, data=df)
summary(m)
confint(m, level = 0.95)
par(mfrow=c(2,2))

plot(m)
