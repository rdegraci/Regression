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

Alternatively you can try to use

with(cats, cor.test(Bwt, Hwt))

Other scatter plots can be 

with(cats, plot(Bwt, Hwt, type="n", las=1, xlab="Body Weight in kg", ylab="Heart Weight in g",main="Heart Weight vs. Body Weight of Cats"))

with(cats, points(Bwt[Sex=="F"], Hwt[Sex=="F"], pch=16, col="red"))

with(cats, points(Bwt[Sex=="M"], Hwt[Sex=="M"], pch=17, col="blue"))


With strong correlation, it makes sense to fit a linear model. 

l1 = with(data = cats, lm(Bwt~Hwt))

summary(l1)

Predicted : Bwt = f(Hwt)Predict new data points 


bwt = seq(0, 5, length = 200)

hwt = predict(l1, data.frame(Bwt = bwt))

plot(Hwt ~ Bwt, data=cats, xlab = "Body Weight", ylab = "Heart Weight", xlim = c(0,5))

points(bwt, hwt, type = "l", col = "red")

Since the data is scattered, a confidence interval can be obtained as well in the regression model

ggplot(newdata, aes(x = BW, y = HW))+geom_line(col ="red")+geom_point(data = cats, aes(x = Bwt, y = Hwt))+xlim(c(0,5))+xlab("Body Weight")+ ylab ("Heart Weight")


hwt = predict(l1, data.frame(Bwt = bwt), interval = "confidence")

colnames(hwt) = c("HW", "Lower", "Upper")

newdata = data.frame(BW = bwt, hwt)

ggplot(newdata, aes(x = BW, y = HW))+geom_line(col ="red")+geom_ribbon(aes(ymin = Lower, ymax = Upper))+geom_point(data = cats, aes(x = Bwt, y = Hwt))+xlim(c(0,5))+xlab("Body Weight")+ ylab ("Heart Weight")











