gui_sim=read.csv("gui_sim.csv",sep=",",header=T)
attach(gui_sim)
set.seed(833650833)
mysample<-gui_sim[sample(1:nrow(gui_sim),500,replace=F),]
detach(gui_sim)
attach(mysample)
maths_engagement<-bcce46a
levels(maths_engagement)
table(maths_engagement)
levels(maths_engagement)<-c("Others","Not interesting","Others")
maths_engagement
table(maths_engagement)
table(bphc02a)
cross<-table(bphc02a,maths_engagement)
cross
#Problem statement: Is the child gender variable independent of maths_engagement variable ?
#H0 : Child gender variable is independent of maths_engagement variable
#H1 : Child gender variable is dependednt on maths_engagement variable
#Chisq test with .05 significance level
chisq.test(cross,correct = F)
#Since p value (0.1946) is greater than 0.05 we conclude we cannot reject the Null Hypothesis that the child gender variable is independent of maths_engagement
lg=glm(maths_engagement~bphc02a,data = mysample,family = binomial)
summary(lg)
1-pchisq(0.74,1)
#Test for Significance bphc02a: pvalue is 0.391 hence we cannot reject the Null Hypothesis that the coefficient bphc02a = 0
#Test for overall regression: Null Dev - Res Dev = 459.04 - 458.30 = 0.74, with pvalue of 0.38966 hence we cannot reject the Null Hypothesis that both coefficients( Intercept & bphc02a) are equal to zero. ie the overall regression is not statistically significant
class(bphc02a)
levels(bphc02a)

logitfunc1<-function(x)
{
  0.25-(exp(x)/(1+exp(x)))
}
Beta0=uniroot(logitfunc1,lower=-10000,upper = 0)$root
logitfunc2<-function(y)
{
  0.18-(exp(-1.098618+y)/(1+exp(-1.098618+y)))
}
Beta1=uniroot(logitfunc2,lower=-10000,upper = 10)$root
Betahat=matrix(c(Beta0,Beta1),nrow=2)

levels(bphc02a)
Malfem=factor(mysample$bphc02a)
colnames(X)=c("Intercept","bphc02a")
X=model.matrix(~Malfem)
Eta=X%*%Betahat
library(LaplacesDemon)
Etap=invlogit(Eta)
Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
mysample1=data.frame(mysample,Ynew)
lgnewdash=glm(Ynew~bphc02a,data = mysample1,family = binomial)
summary(lgnewdash)
summary(lgnewdash)$coefficients[2,4] 
lgnewdash$coefficients
set.seed(9048490112)
Evali=c()
for (i in 1:1000) {
  mysample<-gui_sim[sample(1:nrow(gui_sim),500,replace=F),]
  Malfem=factor(mysample$bphc02a)
  colnames(X)=c("Intercept","bphc02a")
  X=model.matrix(~Malfem)
  Eta=X%*%Betahat
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
  mysample2=data.frame(mysample,Ynew)
  lgnew=glm(Ynew~bphc02a,data = mysample2,family = binomial)
  Evali=c(Evali,lgnew$coefficients,summary(lgnew)$coefficients[2,4],as.numeric(summary(lgnew)$coefficients[2,4]<0.05) )
  }
Eval<-matrix(Evali,ncol = 4,byrow = T)
colnames(Eval)=c('Beta0','Beta1',"Pvalue Beta1","Pvalue Indicator")
Overall_Mean_Beta0=mean(Eval[,1])
Overall_Mean_Beta1=mean(Eval[,2])
# Effect on Eta (Regressed on bphc02a, With Monte Carlo Simulation &  resampling of 500 sample)     

# Equation= -1.09861 + -0.4177 *(male == 1) 
 

# Our model = -1.091985 + -0.43367211 *(male == 1)

beta1_statistcially_significant=(sum(Eval[,4])/1000)
#Since P value for this test is >0.05 we fail to reject the Null Hypothesis
#Power of the test is 50.1%
par(mfrow=c(2,2))
Beta0_Dist=hist(Eval[,1],breaks = 50,main = "Histogram of Beta0",xlab = "Beta0") #Skewed to Left
Beta1_Dist=hist(Eval[,2],breaks = 100,main = "Histogram of Beta1", xlab = "Beta1")
Betahatdash =matrix(c(-1,0, 1),nrow=3)
col1=c(as.numeric(mysample$acce16c == 'Always like it'))
col2=c(as.numeric(mysample$acce16c == 'Sometimes like it'))
col3=c(as.numeric(mysample$acce16c == 'Never like it'))
Xdash=matrix(c(col1,col2,col3), ncol=3)
Malfem=factor(mysample$bphc02a)
colnames(X)=c("Intercept","bphc02a")
X=model.matrix(~Malfem)
Xfinal=matrix(c(X, Xdash), ncol=5)
Betahatfinal =matrix(c(Betahat, Betahatdash), nrow=5) 
Etafinal=Xfinal%*%Betahatfinal
Etafinalp=invlogit(Etafinal)
Ynewfinal=matrix(sapply(Etafinalp,rbinom,n=1,size=1),nrow=nrow(Etafinalp))
mysample2=data.frame(mysample,Ynewfinal)
lgnewdashfinal=glm(Ynewfinal~ bphc02a + acce16c,data = mysample2,family = binomial)
summary(lgnewdashfinal)
# Effect on Eta (Without Monte Carlo Simulation)     

# Equation= -1.09861 + -0.4177 *(male == 1) + 1 *(Never like it ==1) + -1*(Always Like it ==1)

   # Eta (male, never like it) = -0.51631
   # Eta (female, never like it) = -0.09861
   # Eta (male, always like it) = -2.51631
   # Eta (female, always like it) = -2.09861
   # Eta (male, sometimes like it) = -1.51631
   # Eta (female, sometimes like it) = -1.09861

# Our model = -1.8921 -0.7505 *(male == 1) + 2.2951 *(Never like it ==1) + 1.2332*(Sometimes like it ==1)

    # Eta (male, never like it) = -0.3475
    # Eta (female, never like it) = 0.403
    # Eta (male, always like it) = -2.6426
    # Eta (female, always like it) = -1.8921
    # Eta (male, sometimes like it) = -1.4904
    # Eta (female, sometimes like it) = -0.6589

# Results are almost close.. 
# Conclusion : The present sample does not estimate the parent population well enough...Lets use Monte carlo..

