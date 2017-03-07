## Regression Analysis for Continuous variable.


 

First load the dataset. 

library(MASS)

data(cats)

View how the data looks like. 

head(cats)

summary(cats)

"Bwt" is the body weight in kg, "Hwt" is the heart weight in gms, and "Sex” obvious. 


There are several ways of plotting the data. Lets try ggplot first. 

library(ggplot2)

ggplot(cats, aes(x = Bwt, y = Hwt))+ geom_point()+xlab("Body Weight")+ylab("Heart Weight")

We can be interested to look into how the body weight compares with heart weight for each gender

ggplot(cats, aes(x = Bwt, y = Hwt, group = "Sex"))+ geom_point()+facet_wrap(~Sex)+xlab("Body Weight")+ylab("Heart Weight")

plot(Hwt ~ Bwt, data=cats, xlab = "Body Weight", ylab = "Heart Weight")

The graph shows a moderate relationship between Heart and Body weight. But we need a strong mathematical quantity in order to understand the effect of one variable on the other. 

with(cats, cor(Bwt, Hwt))
[1] 0.8041274



