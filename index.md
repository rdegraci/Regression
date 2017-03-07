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

l1 = with(data = cats, lm(Hwt~I(Bwt)+I(Bwt^2)))

bwt = seq(0, 5, length = 200)

hwt = predict(l1, data.frame(Bwt = bwt))

plot(Hwt ~ Bwt, data=cats, xlab = "Body Weight", ylab = "Heart Weight", xlim = c(0,5))
points(bwt, hwt, type = "l", col = "red")

newdata = data.frame(BW = bwt, HW = hwt)

ggplot(newdata, aes(x = BW, y = HW))+geom_line(col ="red")+geom_point(data = cats, aes(x = Bwt, y = Hwt))+xlim(c(0,5))+xlab("Body Weight")+ ylab ("Heart Weight")

bwt = seq(0, 5, length = 200)

hwt = predict(l1, data.frame(Bwt = bwt), interval = "confidence")
colnames(hwt) = c("HW", "Lower", "Upper")
newdata = data.frame(BW = bwt, hwt)

ggplot(newdata, aes(x = BW, y = HW))+geom_line(col ="red")+geom_ribbon(aes(ymin = Lower, ymax = Upper))+geom_point(data = cats, aes(x = Bwt, y = Hwt))+xlim(c(0,5))+xlab("Body Weight")+ ylab ("Heart Weight")


l1 = with(data = cats, lm(Hwt~poly(Bwt,3)))

bwt = seq(0, 5, length = 200)

hwt = predict(l1, data.frame(Bwt = bwt))

plot(Hwt ~ Bwt, data=cats, xlab = "Body Weight", ylab = "Heart Weight", xlim = c(0,5))
points(bwt, hwt, type = "l", col = "red")

newdata = data.frame(BW = bwt, HW = hwt)

ggplot(newdata, aes(x = BW, y = HW))+geom_line(col ="red")+geom_point(data = cats, aes(x = Bwt, y = Hwt))+xlim(c(0,5))+xlab("Body Weight")+ ylab ("Heart Weight")

bwt = seq(0, 5, length = 200)

hwt = predict(l1, data.frame(Bwt = bwt), interval = "confidence")
colnames(hwt) = c("HW", "Lower", "Upper")
newdata = data.frame(BW = bwt, hwt)

ggplot(newdata, aes(x = BW, y = HW))+geom_line(col ="red")+geom_ribbon(aes(ymin = Lower, ymax = Upper))+geom_point(data = cats, aes(x = Bwt, y = Hwt))+xlim(c(0,5))+xlab("Body Weight")+ ylab ("Heart Weight")

hwt = predict(l1, data.frame(Bwt = bwt), interval = "confidence")

# Confidence interval cannot be predicted using predict function.


library(nlstools)
l1 = nls(Hwt~fn(Bwt,a,b), cats, start = list(a=0,b=0.5),algorithm = "port")

Boo <- nlsBoot(l1, niter = 200)
Param_Boo <- Boo$coefboot


x <- bwt
curveDF <- data.frame(matrix(0,ncol = 2,nrow = 200*length(x)))

for(i in 1:200)
{
  for(j in 1:length(x))
  {
    
    # Function value
    curveDF[j+(i-1)*200,1] <- fn(x[j],Param_Boo[i,1],Param_Boo[i,2])
    # Bootstrap sample number
    curveDF[j+(i-1)*200,2] <- i
    # x value
    curveDF[j+(i-1)*200,3] <- x[j]
  }
}

colnames(curveDF) <- c('ys','bsP','xs')

cat = data.frame(cats, bsP = 1)

ggplot(data = curveDF, aes(x=xs, y=ys, group=bsP)) +geom_line(col = "yellow", alpha = 0.2) +geom_point(data = cat, aes(x = Bwt, y = Hwt))+ylim(c(0,20))+xlim(c(0,5))+xlab("Body Weight")+ ylab ("Heart Weight")



# Categorical Variable


ggplot(cats,aes(x = Bwt, y = Hwt, group = Sex))+geom_point(aes(color = Sex))

mod.form = "Sex ~ Bwt*Hwt"    

glm.out = glm(mod.form, family=binomial(logit), data=cats)

anova(glm.out, test="Chisq")

summary(glm.out)

plot(glm.out)




















