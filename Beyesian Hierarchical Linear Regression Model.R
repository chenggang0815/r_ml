library(MCMCpack)
library(AER)
library(nlme)
library(lattice)

data("TeachingRatings")
names(TeachingRatings)
#拟合简单线性模型,并用最小二乘方法进行估计
#Model 1
fm.form<-formula(eval ~ beauty + gender + minority + native + tenure + division + credits+age)
fmw <- lm(fm.form,weights = students, data = TeachingRatings)
coeftest(fmw, vcov = sandwich)
bwplot(resid(fmw,type="pearson")~prof,data=TeachingRatings)
#Model 2
##拟合随机截距和斜率的异方差模型,并用限制最大似然方法进行估计
#异方差为幂指型
fmw_rsh<-update(fmw_rh,random=~1+beauty|prof,weights=varPower(form=~students),data=TeachingRatings,method="REML")
summary(fmw_rsh)
bwplot(resid(fmw_rsh,type="pearson")~prof,data=TeachingRatings）
       
AIC(fmw,fmw_rsh)
BIC(fmw,fmw_rsh)
#Model 3
##拟合随机截距和斜率的贝叶斯分层线性模型,并用MCMC方法进行估计
model_beyesian <- MCMChregress(
  fixed=eval~minority+age+credits+division+native+tenure+gender, random=~beauty, group="prof",
  data=TeachingRatings, burnin=1000, mcmc=1000, thin=1,verbose=1,
  seed=NA, beta.start=0, sigma2.start=1,
  Vb.start=1, mubeta=0, Vbeta=1.0E6,
  r=3, R=diag(c(1,0.1)), nu=0.001, delta=0.001)

pdf("Posteriors-MCMChregress.pdf")
plot(model$mcmc)
dev.off()
summary(model_beyesian)
str(model_beyesian)
summary(model_beyesian$mcmc)
model_beyesian$Y.pred
plot(TeachingRatings$eval,model_beyesian$Y.pred)
abline(a=0,b=1)
#最后====>如何将模型二与模型三的结果相比较？

