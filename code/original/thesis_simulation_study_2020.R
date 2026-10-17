#Jan 2020-Stage 2
#**************************************************************************************************************************************************************
# Uploading the data

gui_sim=read.csv("gui_sim_2.csv",sep=",",header=T)
attach(gui_sim)
#**************************************************************************************************************************************************************#
# Fitting a Model with Math_Engagement variable as response

maths_engagement<-bcce46a
levels(maths_engagement)<-c("Others","Not interesting","Others")
fit = glm(factor(maths_engagement) ~ trans_diff_qrt + acce14 + acce16a + tposscale + tnegscale + bphc02a + bcce37 + bcce50, data = gui_sim, family = "binomial")
summary(fit)
model.matrix(~ trans_diff_qrt + acce14 + acce16a + tposscale + tnegscale + bphc02a + bcce37 + bcce50)
#****************************************************************************************************************************************************************
# Significant Predictors chosen :
#                                          **VARIABLE NAME**                                               **RESPONSES**     
#                                 1.	trans_diff_qrt----Measure of transition difficulty                 ([5, 10], [13,15],[10,13],[15,35])
#                                 2.	acce14------------What do you think about school ?                 (Always Like it, Never Like it, Sometimes Like it)
#                                 3.  tposscale---------Positive interaction with teachers               (2.75, 3, <= 2.5, >3)  
#                                 4.  tnegscale---------Negative interaction with teachers               (2.75, 3, <= 2.5, >3)
#                                 5.  bphc02a-----------Male/Female                                      (Male, Female)
#                                 6.  bcce37------------1st/2nd year of secondary school                 (First year, Second Year)
#                                 7.  acce16a-----------Like: Maths                                      (Always Like it, Never Like it, Sometimes Like it)
#                                 8.	Bcce50-----Highest qualification                                   (Certificate or Diploma (including plc., apprenticeship), Degree or higher degree, Junior Cert, Leaving Cert)
# Our Equation : Eta1 = Beta0 + Beta1*(trans_diff_qrt == [13, 15]) + Beta2*(trans_diff_qrt == [10,13]) + Beta3*(trans_diff_qrt == [15,35]) + Beta4*(Qualification == 'Junior cert')...........
# No of Betas = 8 (Significant Predictors)

#*****************************************************************************************************************************************************************
# Simulated Binary responses.....
# Evaluating whether AIC Model Selection captures all the significant predictors

set.seed(904849011)
countf_AIC = 0
countf_TransDiff = 0
countf_LikeSch = 0
countf_TPoss = 0
countf_TNeg = 0
countf_MalFem = 0
countf_School = 0
countf_LikeMat = 0
countf_Quali = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
detach(gui_sim)
for (i in 1:1000) 
{
  mysample<-gui_sim[sample(1:nrow(gui_sim),5000,replace=T),]
  attach(mysample)
  Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
  Eta = matrix(c(predict(fit, newdata=mysample))) 
  library(LaplacesDemon)
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
  
  mysample1=data.frame(mysample,Ynew)
  
  AIC=step(glm(Ynew~1,family= binomial, data = mysample1),scope=list(lower=~ 1,upper=~trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50), data = mysample1, direction="both", trace=1)
  AIC_Coeff<- if(is.null(AIC$xlevels)) matrix(,) else matrix(c(names(AIC$xlevels)),byrow = F)
  count_0 = count_0 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==0)
  count_1 = count_1 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==1)
  count_2 = count_2 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==2)
  count_3 = count_3 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==3)
  count_4 = count_4 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==4)
  count_5 = count_5 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==5)
  count_6 = count_6 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==6)
  count_7 = count_7 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==7)
  count_8 = count_8 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==8)
  countf_AIC = countf_AIC +as.numeric(identical(sort(AIC_Coeff),sort(Beta_Significant)))
  countf_TransDiff = countf_TransDiff + as.numeric('trans_diff_qrt' %in% AIC_Coeff)
  countf_LikeSch = countf_LikeSch + as.numeric('acce14' %in% AIC_Coeff)
  countf_TPoss = countf_TPoss + as.numeric('tposscale' %in% AIC_Coeff)
  countf_TNeg = countf_TNeg + as.numeric('tnegscale' %in% AIC_Coeff)
  countf_MalFem = countf_MalFem + as.numeric('bphc02a' %in% AIC_Coeff)
  countf_School = countf_School + as.numeric('bcce37' %in% AIC_Coeff)
  countf_LikeMat = countf_LikeMat + as.numeric('acce16a' %in% AIC_Coeff)
  countf_Quali = countf_Quali + as.numeric('bcce50' %in% AIC_Coeff)

}
Counts = matrix(c(0,0,0,0,0,5,115,454,426))
CountH = matrix(c(0,1,2,3,4,5,6,7,8))
y = data.frame(CountH,Counts)
Plot = barplot(y$Counts, names.arg=y$CountH, main = "Frequency of The No of Predictors chosen by Stepwise Method in the absence of Noise Predictors", xlab = "No of Predictors", ylab = "Frequency") 

#5000 setting

countf_AIC # Countf_AIC gives value of 426 in 1000 runs i.e the AIC technique only captures the right significant predictors 427 times in 1000 runs
countf_TransDiff # 765 times trans_diff_qrt was chosen
countf_LikeSch # 765 times acce14 was chosen 
countf_TPoss # 998 times tposscale was chosen
countf_TNeg # 1000 times tnegscale was chosen
countf_MalFem # 1000 times bphc02a was chosen
countf_School # 958 times bcce37 was chosen
countf_LikeMat # 1000 times acce16a was chosen
countf_Quali # 815 times bcce50 was chosen
count_0 # 0 s.predictors  chosen 0 times
count_1 # 1 s.predictors  chosen 0 times
count_2 # 2 s.predictors  chosen 0 times
count_3 # 3 s.predictors  chosen 0 times
count_4 # 4 s.predictors  chosen 0 times
count_5 # 5 s.predictors  chosen 5 times
count_6 # 6 s.predictors  chosen 115 times
count_7 # 7 s.predictors  chosen 454 times
count_8 # 8 s.predictors  chosen 426 times

#500 setting

countf_AIC # Countf_AIC gives value of 426 in 1000 runs i.e the AIC technique only captures the right significant predictors 427 times in 1000 runs
countf_TransDiff # 249 times trans_diff_qrt was chosen
countf_LikeSch # 236 times acce14 was chosen 
countf_TPoss # 426 times tposscale was chosen
countf_TNeg # 820 times tnegscale was chosen
countf_MalFem # 454 times bphc02a was chosen
countf_School # 302 times bcce37 was chosen
countf_LikeMat # 879 times acce16a was chosen
countf_Quali # 275 times bcce50 was chosen
count_0 # 0 s.predictors  chosen 1 times
count_1 # 1 s.predictors  chosen 18 times
count_2 # 2 s.predictors  chosen 138 times
count_3 # 3 s.predictors  chosen 320 times
count_4 # 4 s.predictors  chosen 297 times
count_5 # 5 s.predictors  chosen 165 times
count_6 # 6 s.predictors  chosen 53 times
count_7 # 7 s.predictors  chosen 8 times
count_8 # 8 s.predictors  chosen 0 times


# Evaluating whether Lasso Model Selection captures all the significant predictors

set.seed(904849011)
countf_Lasso = 0
countf_TransDiff_l = 0
countf_LikeSch_l = 0
countf_TPoss_l = 0
countf_TNeg_l = 0
countf_MalFem_l = 0
countf_School_l = 0
countf_LikeMat_l = 0
countf_Quali_l = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
detach(gui_sim)
Lasso_Coeff_rd = c()
for (i in 1:1000) {
  mysample<-gui_sim[sample(1:nrow(gui_sim),5000,replace=T),]
  attach(mysample)
  Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
  Eta = matrix(c(predict(fit, newdata=mysample)))
  library(LaplacesDemon)
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
  mysample1=data.frame(mysample,Ynew)

  library(glmnet)
  preds=model.matrix(~trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50)
  cv=cv.glmnet(preds,Ynew,family="binomial",alpha=1)
  plot(cv)
    lasso_fit=glmnet(preds,Ynew,family="binomial",alpha=1,lambda=cv$lambda.1se)
  Lasso_Coeff<- if(is.null(names(which(abs(lasso_fit$beta[,1])>0)))) matrix(,) else matrix(c(names(which(abs(lasso_fit$beta[,1])>0))), byrow = F)
  for (i in 1:length(Beta_Significant)) 
  Lasso_Coeff_rd = c(Lasso_Coeff_rd, regmatches(Lasso_Coeff, regexpr(Beta_Significant[i], Lasso_Coeff)))
  Lasso_Coeff_rdn = matrix(unique(Lasso_Coeff_rd))
  count_0 = count_0 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==0)
  count_1 = count_1 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==1)
  count_2 = count_2 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==2)
  count_3 = count_3 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==3)
  count_4 = count_4 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==4)
  count_5 = count_5 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==5)
  count_6 = count_6 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==6)
  count_7 = count_7 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==7)
  count_8 = count_8 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==8)
  countf_Lasso = countf_Lasso + as.numeric(identical(sort(Lasso_Coeff_rdn),sort(Beta_Significant)))
  countf_TransDiff_l = countf_TransDiff_l + as.numeric('trans_diff_qrt' %in% Lasso_Coeff_rdn)
  countf_LikeSch_l = countf_LikeSch_l + as.numeric('acce14' %in% Lasso_Coeff_rdn)
  countf_TPoss_l = countf_TPoss_l + as.numeric('tposscale' %in% Lasso_Coeff_rdn)
  countf_TNeg_l = countf_TNeg_l + as.numeric('tnegscale' %in% Lasso_Coeff_rdn)
  countf_MalFem_l = countf_MalFem_l + as.numeric('bphc02a' %in% Lasso_Coeff_rdn)
  countf_School_l = countf_School_l + as.numeric('bcce37' %in% Lasso_Coeff_rdn)
  countf_LikeMat_l = countf_LikeMat_l + as.numeric('acce16a' %in% Lasso_Coeff_rdn)
  countf_Quali_l = countf_Quali_l + as.numeric('bcce50' %in% Lasso_Coeff_rdn)
}

Counts_l = matrix(c(1,0,0,1,0,2,1,115,880))
CountH_l = matrix(c(0,1,2,3,4,5,6,7,8))
y_l = data.frame(CountH_l,Counts_l)
Plot_l=barplot(y_l$Counts_l, names.arg=y_l$CountH_l, main = "Frequency of Different No of Predictors chosen by Lasso Method in the absence of Noise Predictors ", xlab = "No of Predictors", ylab = "Frequency") 

#5000 setting

countf_Lasso # Countf_Lasso gives value of 994 in 1000 runs i.e the Lasso technique captures none of the right significant predictors in 1000 runs
countf_TransDiff_l # 999 times trans_diff_qrt was chosen
countf_LikeSch_l # 1000 times acce14 was chosen 
countf_TPoss_l # 1000 times tposscale was chosen
countf_TNeg_l # 1000 times tnegscale was chosen
countf_MalFem_l # 1000 times bphc02a was chosen
countf_School_l # 998 times bcce37 was chosen
countf_LikeMat_l # 1000 times acce16a was chosen
countf_Quali_l # 1000 times bcce50 was chosen
count_0 # 0 s.predictors  chosen 0 times
count_1 # 1 s.predictors  chosen 0 times
count_2 # 2 s.predictors  chosen 0 times
count_3 # 3 s.predictors  chosen 0 times
count_4 # 4 s.predictors  chosen 0 times
count_5 # 5 s.predictors  chosen 0 times
count_6 # 6 s.predictors  chosen 1 times
count_7 # 7 s.predictors  chosen 1 times
count_8 # 8 s.predictors  chosen 998 times

#500 setting

countf_Lasso # Countf_Lasso gives value of 880 in 1000 runs i.e the Lasso technique captures none of the right significant predictors in 1000 runs
countf_TransDiff_l # 999 times trans_diff_qrt was chosen
countf_LikeSch_l # 998 times acce14 was chosen 
countf_TPoss_l # 999 times tposscale was chosen
countf_TNeg_l # 999 times tnegscale was chosen
countf_MalFem_l # 998 times bphc02a was chosen
countf_School_l # 880 times bcce37 was chosen
countf_LikeMat_l # 995 times acce16a was chosen
countf_Quali_l # 996 times bcce50 was chosen
count_0 # 0 s.predictors  chosen 1 times
count_1 # 1 s.predictors  chosen 0 times
count_2 # 2 s.predictors  chosen 0 times
count_3 # 3 s.predictors  chosen 1 times
count_4 # 4 s.predictors  chosen 0 times
count_5 # 5 s.predictors  chosen 2 times
count_6 # 6 s.predictors  chosen 1 times
count_7 # 7 s.predictors  chosen 115 times
count_8 # 8 s.predictors  chosen 880 times


# Evaluating whether AIC Model Selection captures all the significant predictors amongst addition of 6 insignficant predictors


#                                               **VARIABLE NAME**                                               **RESPONSES**     
# Insignificant Predictors chosen :1.	Bphc19a----How adequate No of teachers                             (Fair, Good, Poor)
#                                  2.	Bcce50-----Highest qualification                                   (Certificate or Diploma (including plc., apprenticeship), Degree or higher degree, Junior Cert, Leaving Cert)
#                                  3.	Bpsd40a----Primary caretaker’s highest qualification               (Hi Sec/TechVoc/UppSec+Tech/Voc,Lower Sec, Non Degree, Postgrad, Primary) )
#                                  4.	Bdsd57a----HSD class (family’s social)                             (Non/skilled man, Not active, Prof/manager, Semi-U.S. man)
#                                  5.	Bhpc55-----Religious ethos of school                               (Non RC, Roman Catholic)
#                                  6.	Bhpc61a----Induction Day                                           (No, Yes)


set.seed(904849011)
countf_AIC_i=0
countf_TransDiff_i =0
countf_LikeSch_i =0
countf_TPoss_i = 0
countf_TNeg_i = 0
countf_MalFem_i = 0
countf_School_i = 0
countf_LikeMat_i = 0
countf_SchoolPT_i = 0
countf_Adeq_No_i = 0
countf_Quali_i = 0
countf_CareQ_i = 0
countf_HSD_i = 0
countf_RelE_i = 0
countf_Ind_i = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
ncount_0 = 0
ncount_1 = 0
ncount_2 = 0
ncount_3 = 0
ncount_4 = 0
ncount_5 = 0
ncount_6 = 0
detach(gui_sim)
NonBeta_Significant=matrix(c('bpce33a','bhpc19a','bpsd40a','bdsd57a','bhpc55','bhpc61a'),byrow = T)
for (i in 1:1000) 
{ 
mysample<-gui_sim[sample(1:nrow(gui_sim),500,replace=T),]
attach(mysample)
Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
Eta = matrix(c(predict(fit, newdata=mysample)))
library(LaplacesDemon)
Etap=invlogit(Eta)
Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
mysample1=data.frame(mysample,Ynew)

AIC=step(glm(Ynew~1,family= binomial, data = mysample1),scope=list(lower=~ 1,upper=~trans_diff_qrt + acce14 + acce16a + tposscale + tnegscale + bphc02a + bcce37 + bcce50 + bhpc19a + bpce33a + bpsd40a + bdsd57a + bhpc55 + bhpc61a ), data = mysample1, direction="both", trace=1)
AIC_Coeff<- if(is.null(AIC$xlevels)) matrix(,) else matrix(c(names(AIC$xlevels)),byrow = F)
count_0 = count_0 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==0)
count_1 = count_1 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==1)
count_2 = count_2 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==2)
count_3 = count_3 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==3)
count_4 = count_4 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==4)
count_5 = count_5 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==5)
count_6 = count_6 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==6)
count_7 = count_7 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==7)
count_8 = count_8 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==8)
ncount_0 = ncount_0 + as.numeric(length(intersect(AIC_Coeff,NonBeta_Significant))==0)
ncount_1 = ncount_1 + as.numeric(length(intersect(AIC_Coeff,NonBeta_Significant))==1)
ncount_2 = ncount_2 + as.numeric(length(intersect(AIC_Coeff,NonBeta_Significant))==2)
ncount_3 = ncount_3 + as.numeric(length(intersect(AIC_Coeff,NonBeta_Significant))==3)
ncount_4 = ncount_4 + as.numeric(length(intersect(AIC_Coeff,NonBeta_Significant))==4)
ncount_5 = ncount_5 + as.numeric(length(intersect(AIC_Coeff,NonBeta_Significant))==5)
ncount_6 = ncount_6 + as.numeric(length(intersect(AIC_Coeff,NonBeta_Significant))==6)
countf_AIC_i = countf_AIC_i + as.numeric(identical(sort(AIC_Coeff),sort(Beta_Significant)))
countf_TransDiff_i = countf_TransDiff_i + as.numeric('trans_diff_qrt' %in% AIC_Coeff)
countf_LikeSch_i = countf_LikeSch_i + as.numeric('acce14' %in% AIC_Coeff)
countf_TPoss_i = countf_TPoss_i + as.numeric('tposscale' %in% AIC_Coeff)
countf_TNeg_i = countf_TNeg_i + as.numeric('tnegscale' %in% AIC_Coeff)
countf_MalFem_i = countf_MalFem_i + as.numeric('bphc02a' %in% AIC_Coeff)
countf_School_i = countf_School_i + as.numeric('bcce37' %in% AIC_Coeff)
countf_LikeMat_i = countf_LikeMat_i + as.numeric('acce16a' %in% AIC_Coeff)
countf_SchoolPT_i = countf_SchoolPT_i + as.numeric('bpce33a' %in% AIC_Coeff)
countf_Adeq_No_i = countf_Adeq_No_i + as.numeric('bhpc19a' %in% AIC_Coeff)
countf_Quali_i = countf_Quali_i + as.numeric('bcce50' %in% AIC_Coeff)
countf_CareQ_i = countf_CareQ_i + as.numeric('bpsd40a' %in% AIC_Coeff)
countf_HSD_i = countf_HSD_i + as.numeric('bdsd57a' %in% AIC_Coeff)
countf_RelE_i = countf_RelE_i + as.numeric('bhpc55' %in% AIC_Coeff)
countf_Ind_i = countf_Ind_i + as.numeric('bhpc61a' %in% AIC_Coeff)
}
Counts_i = matrix(c(count_0,count_1,count_2,count_3,count_4,count_5,count_6,count_7,count_8))
CountH_i = matrix(c(0,1,2,3,4,5,6,7,8))
y_i = data.frame(CountH_i,Counts_i)
Plot_i = barplot(y_i$Counts_i, names.arg=y_i$CountH_i, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

#5000 setting

countf_AIC_i # Countf_AIC_i gives value of 91 in 1000 runs i.e the AIC technique only captures the right significant predictors 91 times in 1000 runs
countf_TransDiff_i # 760 times trans_diff_qrt was chosen
countf_LikeSch_i # 760 times acce14 was chosen 
countf_TPoss_i # 998 times tposscale was chosen
countf_TNeg_i # 1000 times tnegscale was chosen
countf_MalFem_i # 1000 times bphc02a was chosen
countf_School_i # 947 times bcce37 was chosen
countf_LikeMat_i # 1000 times acce16a was chosen
countf_Quali_i # 809 times bcce50 was chosen

# Insignificant predictors chosen
countf_Adeq_No_i # 81 times bhpc19a was chosen
countf_SchoolPT_i # 172 times bpce33a was chosen
countf_CareQ_i # 95 times bpsd40a was chosen
countf_HSD_i # 102 times bdsd57a was chosen
countf_RelE_i # 129 times bhpc55 was chosen
countf_Ind_i # 121 times bhpc61a was chosen

count_0 # 0 s.predictors  chosen 0 times
count_1 # 1 s.predictors  chosen 0 times
count_2 # 2 s.predictors  chosen 0 times
count_3 # 3 s.predictors  chosen 0 times
count_4 # 4 s.predictors  chosen 0 times
count_5 # 5 s.predictors  chosen 10 times
count_6 # 6 s.predictors  chosen 120 times
count_7 # 7 s.predictors  chosen 456 times
count_8 # 8 s.predictors  chosen 414 times

ncount_0 # 0 s.predictors  chosen 497 times
ncount_1 # 1 s.predictors  chosen 354 times
ncount_2 # 2 s.predictors  chosen 126 times
ncount_3 # 3 s.predictors  chosen 20 times
ncount_4 # 4 s.predictors  chosen 3 times
ncount_5 # 5 s.predictors  chosen 0 times
ncount_6 # 6 s.predictors  chosen 0 times

#500 setting

countf_AIC_i # Countf_AIC_i gives value of 91 in 1000 runs i.e the AIC technique only captures the right significant predictors 91 times in 1000 runs
countf_TransDiff_i # 255 times trans_diff_qrt was chosen
countf_LikeSch_i # 234 times acce14 was chosen 
countf_TPoss_i # 435 times tposscale was chosen
countf_TNeg_i # 811 times tnegscale was chosen
countf_MalFem_i # 462 times bphc02a was chosen
countf_School_i # 303 times bcce37 was chosen
countf_LikeMat_i # 878 times acce16a was chosen
countf_Quali_i # 266 times bcce50 was chosen

# Insignificant predictors chosen
countf_Adeq_No_i # 88 times bhpc19a was chosen
countf_SchoolPT_i # 157 times bpce33a was chosen
countf_CareQ_i # 107 times bpsd40a was chosen
countf_HSD_i # 140 times bdsd57a was chosen
countf_RelE_i # 140 times bhpc55 was chosen
countf_Ind_i # 118 times bhpc61a was chosen

count_0 # 0 s.predictors  chosen 2 times
count_1 # 1 s.predictors  chosen 21 times
count_2 # 2 s.predictors  chosen 138 times
count_3 # 3 s.predictors  chosen 305 times
count_4 # 4 s.predictors  chosen 300 times
count_5 # 5 s.predictors  chosen 179 times
count_6 # 6 s.predictors  chosen 48 times
count_7 # 7 s.predictors  chosen 7 times
count_8 # 8 s.predictors  chosen 0 times

ncount_0 # 0 s.predictors  chosen 456 times
ncount_1 # 1 s.predictors  chosen 375 times
ncount_2 # 2 s.predictors  chosen 134 times
ncount_3 # 3 s.predictors  chosen 33 times
ncount_4 # 4 s.predictors  chosen 2 times
ncount_5 # 5 s.predictors  chosen 0 times
ncount_6 # 6 s.predictors  chosen 0 times




# # Evaluating whether Lasso Model Selection captures all the significant predictors amongst addition of 6 insignficant predictors


#                                               **VARIABLE NAME**                                               **RESPONSES**     
# Insignificant Predictors chosen :1.	Bphc19a----How adequate No of teachers                             (Fair, Good, Poor)
#                                  2. bpce33a-----------Have parents attended PT Meeting ?               (No, Yes)
#                                  3.	Bpsd40a----Primary caretaker’s highest qualification               (Hi Sec/TechVoc/UppSec+Tech/Voc,Lower Sec, Non Degree, Postgrad, Primary) )
#                                  4.	Bdsd57a----HSD class (family’s social)                             (Non/skilled man, Not active, Prof/manager, Semi-U.S. man)
#                                  5.	Bhpc55-----Religious ethos of school                               (Non RC, Roman Catholic)
#                                  6.	Bhpc61a----Induction Day                                           (No, Yes)



set.seed(904849011)
countf_Lasso_i=0
countf_TransDiff_l_i =0
countf_LikeSch_l_i =0
countf_TPoss_l_i = 0
countf_TNeg_l_i = 0
countf_MalFem_l_i = 0
countf_School_l_i = 0
countf_LikeMat_l_i = 0
countf_SchoolPT_l_i = 0
countf_Adeq_No_l_i = 0
countf_Quali_l_i = 0
countf_CareQ_l_i = 0
countf_HSD_l_i = 0
countf_RelE_l_i = 0
countf_Ind_l_i = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
ncount_0 = 0
ncount_1 = 0
ncount_2 = 0
ncount_3 = 0
ncount_4 = 0
ncount_5 = 0
ncount_6 = 0
NonBeta_Significant=matrix(c('bpce33a','bhpc19a','bpsd40a','bdsd57a','bhpc55','bhpc61a'),byrow = T)
f =1
detach(gui_sim)

for (i in 1:1000) {
  Lasso_Coeff_rd = c()
  mysample<-gui_sim[sample(1:nrow(gui_sim),5000,replace=T),]
  attach(mysample)
  Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
  All_Predictors = matrix(c(names(gui_sim[,-1])))
  Eta = matrix(c(predict(fit, newdata=mysample)))
  library(LaplacesDemon)
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
  mysample1=data.frame(mysample,Ynew)
  library(glmnet)
  preds=model.matrix(~ trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a +  bcce50 + bhpc19a +bpce33a + bpsd40a + bdsd57a + bhpc55 + bhpc61a)
  cv=cv.glmnet(preds,Ynew,family="binomial",alpha=1)
  lasso_fit=glmnet(preds,Ynew,family="binomial",alpha=1,lambda=cv$lambda.1se)
  Lasso_Coeff<- if(is.null(names(which(abs(lasso_fit$beta[,1])>0)))) matrix(,) else matrix(c(names(which(abs(lasso_fit$beta[,1])>0))), byrow = F)
  for (j in 1:length(All_Predictors)) 
    Lasso_Coeff_rd = c(Lasso_Coeff_rd, regmatches(Lasso_Coeff, regexpr(All_Predictors[j], Lasso_Coeff)))
  Lasso_Coeff_rdn = matrix(unique(Lasso_Coeff_rd))

  ncount_0 = ncount_0 + as.numeric(length(intersect(Lasso_Coeff_rdn,NonBeta_Significant))==0)
  ncount_1 = ncount_1 + as.numeric(length(intersect(Lasso_Coeff_rdn,NonBeta_Significant))==1)
  ncount_2 = ncount_2 + as.numeric(length(intersect(Lasso_Coeff_rdn,NonBeta_Significant))==2)
  ncount_3 = ncount_3 + as.numeric(length(intersect(Lasso_Coeff_rdn,NonBeta_Significant))==3)
  ncount_4 = ncount_4 + as.numeric(length(intersect(Lasso_Coeff_rdn,NonBeta_Significant))==4)
  ncount_5 = ncount_5 + as.numeric(length(intersect(Lasso_Coeff_rdn,NonBeta_Significant))==5)
  ncount_6 = ncount_6 + as.numeric(length(intersect(Lasso_Coeff_rdn,NonBeta_Significant))==6)

detach(mysample)
  
  f = f + 1
  print(f)
  }
Counts_l_i = matrix(c(count_0,count_1,count_2,count_3,count_4,count_5,count_6,count_7,count_8))
CountH_l_i = matrix(c(0,1,2,3,4,5,6,7,8))
y_l_i = data.frame(CountH_l_i,Counts_l_i)
Plot_l_i=barplot(y_l_i$Counts_l_i, names.arg=y_l_i$CountH_l_i, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

#5000 setting

#Sign
Counts_l_i = matrix(c(0,0,0,7,58,126,234,313,262))
CountH_l_i = matrix(c(0,1,2,3,4,5,6,7,8))
y_l_i = data.frame(CountH_l_i,Counts_l_i)
Plot_l_i=barplot(y_l_i$Counts_l_i, names.arg=y_l_i$CountH_l_i, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

#Insign

Counts_l_i = matrix(c(count_0,count_1,count_2,count_3,count_4,count_5,count_6,count_7,count_8))
CountH_l_i = matrix(c(0,1,2,3,4,5,6,7,8))
y_l_i = data.frame(CountH_l_i,Counts_l_i)
Plot_l_i=barplot(y_l_i$Counts_l_i, names.arg=y_l_i$CountH_l_i, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

#500 setting

#Sign
Counts_l_i = matrix(c(623,244,53,31,23,18,5,2,1))
CountH_l_i = matrix(c(0,1,2,3,4,5,6,7,8))
y_l_i = data.frame(CountH_l_i,Counts_l_i)
Plot_l_i=barplot(y_l_i$Counts_l_i, names.arg=y_l_i$CountH_l_i, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

#Insign

Counts_l_i = matrix(c(950,37,7,5,1,0))
CountH_l_i = matrix(c(0,1,2,3,4,5,6))
y_l_i = data.frame(CountH_l_i,Counts_l_i)
Plot_l_i=barplot(y_l_i$Counts_l_i, names.arg=y_l_i$CountH_l_i, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 


#5000 setting

countf_Lasso_i # Countf_Lasso_i gives value of 102 in 1000 runs i.e the Lasso technique only captures all the right significant predictors 73 time in 1000 runs
countf_TransDiff_l_i # 666 times trans_diff_qrt was chosen 
countf_LikeSch_l_i # 763 times acce14 was chosen
countf_TPoss_l_i # 957 times tposscale was chosen
countf_TNeg_l_i # 1000 times tnegscale was chosen
countf_MalFem_l_i # 774 times bphc02a was chosen
countf_School_l_i # 432 times bcce37 was chosen
countf_LikeMat_l_i # 1000 times acce16a was chosen
countf_Quali_l_i # 982 times bcce50 was chosen

# Insignificant predictors chosen
countf_Adeq_No_l_i # 91 times bhpc19a was chosen
countf_SchoolPT_l_i # 64 times bpce33a was chosen
countf_CareQ_l_i # 115 times bpsd40a was chosen
countf_HSD_l_i # 98 times bdsd57a was chosen
countf_RelE_l_i # 29 times bhpc55 was chosen
countf_Ind_l_i # 57 times bhpc61a was chosen


count_0 # 0 s.predictors  chosen 0 times
count_1 # 1 s.predictors  chosen 0 times
count_2 # 2 s.predictors  chosen 0 times
count_3 # 3 s.predictors  chosen 7 times
count_4 # 4 s.predictors  chosen 58 times
count_5 # 5 s.predictors  chosen 126 times
count_6 # 6 s.predictors  chosen 234 times
count_7 # 7 s.predictors  chosen 313 times
count_8 # 8 s.predictors  chosen 262 times

ncount_0 # 0 s.predictors  chosen 673 times
ncount_1 # 1 s.predictors  chosen 232 times
ncount_2 # 2 s.predictors  chosen 72 times
ncount_3 # 3 s.predictors  chosen 16 times
ncount_4 # 4 s.predictors  chosen 5 times
ncount_5 # 5 s.predictors  chosen 2 times
ncount_6 # 6 s.predictors  chosen 0 times

#500 setting

countf_Lasso_i # Countf_Lasso_i gives value of 0 in 1000 runs i.e the Lasso technique only captures all the right significant predictors 73 time in 1000 runs
countf_TransDiff_l_i # 42 times trans_diff_qrt was chosen 
countf_LikeSch_l_i # 55 times acce14 was chosen
countf_TPoss_l_i # 57 times tposscale was chosen
countf_TNeg_l_i # 137 times tnegscale was chosen
countf_MalFem_l_i # 23 times bphc02a was chosen
countf_School_l_i # 11 times bcce37 was chosen
countf_LikeMat_l_i # 249 times acce16a was chosen
countf_Quali_l_i # 103 times bcce50 was chosen

# Insignificant predictors chosen
countf_Adeq_No_l_i # 15 times bhpc19a was chosen
countf_SchoolPT_l_i #5 times bpce33a was chosen
countf_CareQ_l_i # 21 times bpsd40a was chosen
countf_HSD_l_i # 12 times bdsd57a was chosen
countf_RelE_l_i # 10 times bhpc55 was chosen
countf_Ind_l_i # 7 times bhpc61a was chosen


count_0 # 0 s.predictors  chosen 623 times
count_1 # 1 s.predictors  chosen 244 times
count_2 # 2 s.predictors  chosen 53 times
count_3 # 3 s.predictors  chosen 31 times
count_4 # 4 s.predictors  chosen 23 times
count_5 # 5 s.predictors  chosen 18 times
count_6 # 6 s.predictors  chosen 5 times
count_7 # 7 s.predictors  chosen 2 times
count_8 # 8 s.predictors  chosen 1 times

ncount_0 # 0 s.predictors  chosen 950 times
ncount_1 # 1 s.predictors  chosen 37 times
ncount_2 # 2 s.predictors  chosen 7 times
ncount_3 # 3 s.predictors  chosen 5 times
ncount_4 # 4 s.predictors  chosen 1 times
ncount_5 # 5 s.predictors  chosen 0 times
ncount_6 # 6 s.predictors  chosen 0 times

#*******************************************************************************************************************************************************************************************

## Adding a Normal variate to Eta: AIC - With Significant Predictors Only

set.seed(904849011)
countf_AIC_n = 0
countf_TransDiff_n = 0
countf_LikeSch_n = 0
countf_TPoss_n = 0
countf_TNeg_n = 0
countf_MalFem_n = 0
countf_School_n = 0
countf_LikeMat_n = 0
countf_Quali_i_n = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
detach(gui_sim)
for (i in 1:1000) 
{
  mysample<-gui_sim[sample(1:nrow(gui_sim),500,replace=T),]
  attach(mysample)
  Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
  Eta = matrix(c(predict(fit, newdata=mysample))) + rnorm(500,0,4)
  library(LaplacesDemon)
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
 
  mysample1=data.frame(mysample,Ynew)

  AIC=step(glm(Ynew~1,family= binomial, data = mysample1),scope=list(lower=~ 1,upper=~trans_diff_qrt + acce14 + acce16a + tposscale + tnegscale + bphc02a + bcce37 + bcce50 ), data = mysample1, direction="both", trace=1)
  AIC_Coeff<- if(is.null(AIC$xlevels)) matrix(,) else matrix(c(names(AIC$xlevels)),byrow = F)
  count_0 = count_0 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==0)
  count_1 = count_1 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==1)
  count_2 = count_2 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==2)
  count_3 = count_3 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==3)
  count_4 = count_4 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==4)
  count_5 = count_5 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==5)
  count_6 = count_6 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==6)
  count_7 = count_7 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==7)
  count_8 = count_8 + as.numeric(length(intersect(AIC_Coeff,Beta_Significant))==8)
  countf_AIC_n = countf_AIC_n + as.numeric(identical(sort(AIC_Coeff),sort(Beta_Significant)))
  countf_TransDiff_n = countf_TransDiff_n + as.numeric('trans_diff_qrt' %in% AIC_Coeff)
  countf_LikeSch_n = countf_LikeSch_n + as.numeric('acce14' %in% AIC_Coeff)
  countf_TPoss_n = countf_TPoss_n + as.numeric('tposscale' %in% AIC_Coeff)
  countf_TNeg_n = countf_TNeg_n + as.numeric('tnegscale' %in% AIC_Coeff)
  countf_MalFem_n = countf_MalFem_n + as.numeric('bphc02a' %in% AIC_Coeff)
  countf_School_n = countf_School_n + as.numeric('bcce37' %in% AIC_Coeff)
  countf_LikeMat_n = countf_LikeMat_n + as.numeric('acce16a' %in% AIC_Coeff)
  countf_Quali_i_n = countf_Quali_i_n + as.numeric('bcce50' %in% AIC_Coeff)
}

Counts_n = matrix(c(count_0,count_1,count_2,count_3,count_4,count_5,count_6,count_7,count_8))
CountH_n = matrix(c(0,1,2,3,4,5,6,7,8))
y_n = data.frame(CountH_n,Counts_n)
Plot_n = barplot(y_n$Counts_n, names.arg=y_n$CountH_n, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

# 5000 setting

countf_AIC_n # Countf_AIC gives value of 10 in 1000 runs i.e the AIC technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_n # 316 times trans_diff_qrt was chosen
countf_LikeSch_n # 295 times acce14 was chosen 
countf_TPoss_n # 748 times tposscale was chosen
countf_TNeg_n # 978 times tnegscale was chosen
countf_MalFem_n # 748 times bphc02a was chosen
countf_School_n # 537 times bcce37 was chosen
countf_LikeMat_n # 993 times acce16a was chosen
countf_Quali_i_n # 355 times bpce33a was chosen

count_0 # 0 s.predictors  chosen 0 times
count_1 # 1 s.predictors  chosen 0 times
count_2 # 2 s.predictors  chosen 8 times
count_3 # 3 s.predictors  chosen 87 times
count_4 # 4 s.predictors  chosen 267 times
count_5 # 5 s.predictors  chosen 341 times
count_6 # 6 s.predictors  chosen 220 times
count_7 # 7 s.predictors  chosen 72 times
count_8 # 8 s.predictors  chosen 5 times

# 500 setting

countf_AIC_n # Countf_AIC gives value of 0 in 1000 runs i.e the AIC technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_n # 140 times trans_diff_qrt was chosen
countf_LikeSch_n # 160 times acce14 was chosen 
countf_TPoss_n # 217 times tposscale was chosen
countf_TNeg_n # 300 times tnegscale was chosen
countf_MalFem_n # 228 times bphc02a was chosen
countf_School_n # 176 times bcce37 was chosen
countf_LikeMat_n # 391 times acce16a was chosen
countf_Quali_i_n # 166 times bpce33a was chosen

count_0 # 0 s.predictors  chosen 116 times
count_1 # 1 s.predictors  chosen 313 times
count_2 # 2 s.predictors  chosen 321 times
count_3 # 3 s.predictors  chosen 185 times
count_4 # 4 s.predictors  chosen 58 times
count_5 # 5 s.predictors  chosen 6 times
count_6 # 6 s.predictors  chosen 1 times
count_7 # 7 s.predictors  chosen 0 times
count_8 # 8 s.predictors  chosen 0 times


#*******************************************************************************************************************************************************************************************

## Adding a Normal variate to Eta: Lasso - With Significant Predictors Only


set.seed(904849011)
countf_Lasso_n = 0
countf_TransDiff_l_n = 0
countf_LikeSch_l_n = 0
countf_TPoss_l_n = 0
countf_TNeg_l_n = 0
countf_MalFem_l_n = 0
countf_School_l_n = 0
countf_LikeMat_l_n = 0
countf_Quali_l_n = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
detach(gui_sim)
Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
All_Predictors = matrix(c(names(gui_sim[,-1])))
for (i in 1:1000) 
{
  Lasso_Coeff_rd = c()
  mysample<-gui_sim[sample(1:nrow(gui_sim),500,replace=T),]
  attach(mysample)

  Eta = matrix(c(predict(fit,newdata=mysample))) + rnorm(500,0,4)
  library(LaplacesDemon)
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
  mysample1=data.frame(mysample,Ynew)
  library(glmnet)
  preds=model.matrix(~trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50)
  cv=cv.glmnet(preds,Ynew,family="binomial",alpha=1)
  lasso_fit=glmnet(preds,Ynew,family="binomial",alpha=1,lambda=cv$lambda.1se)
  Lasso_Coeff<- if(is.null(names(which(abs(lasso_fit$beta[,1])>0)))) matrix(,) else matrix(c(names(which(abs(lasso_fit$beta[,1])>0))), byrow = F)
  for (j in 1:length(All_Predictors)) 
    Lasso_Coeff_rd = c(Lasso_Coeff_rd, regmatches(Lasso_Coeff, regexpr(All_Predictors[j], Lasso_Coeff)))
  Lasso_Coeff_rdn = matrix(unique(Lasso_Coeff_rd))
  count_0 = count_0 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==0)
  count_1 = count_1 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==1)
  count_2 = count_2 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==2)
  count_3 = count_3 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==3)
  count_4 = count_4 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==4)
  count_5 = count_5 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==5)
  count_6 = count_6 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==6)
  count_7 = count_7 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==7)
  count_8 = count_8 + as.numeric(length(intersect(Lasso_Coeff_rdn,Beta_Significant))==8)
  countf_Lasso_n = countf_Lasso_n + as.numeric(identical(sort(Lasso_Coeff_rdn),sort(Beta_Significant)))
  countf_TransDiff_l_n = countf_TransDiff_l_n + as.numeric('trans_diff_qrt' %in% Lasso_Coeff_rdn)
  countf_LikeSch_l_n = countf_LikeSch_l_n + as.numeric('acce14' %in% Lasso_Coeff_rdn)
  countf_TPoss_l_n = countf_TPoss_l_n + as.numeric('tposscale' %in% Lasso_Coeff_rdn)
  countf_TNeg_l_n = countf_TNeg_l_n + as.numeric('tnegscale' %in% Lasso_Coeff_rdn)
  countf_MalFem_l_n = countf_MalFem_l_n + as.numeric('bphc02a' %in% Lasso_Coeff_rdn)
  countf_School_l_n = countf_School_l_n + as.numeric('bcce37' %in% Lasso_Coeff_rdn)
  countf_LikeMat_l_n = countf_LikeMat_l_n + as.numeric('acce16a' %in% Lasso_Coeff_rdn)
  countf_Quali_l_n = countf_Quali_l_n + as.numeric('bcce50' %in% Lasso_Coeff_rdn)
}

Counts_l_n = matrix(c(count_0,count_1,count_2,count_3,count_4,count_5,count_6,count_7,count_8))
CountH_l_n = matrix(c(0,1,2,3,4,5,6,7,8))
y_l_n = data.frame(CountH_l_n,Counts_l_n)
Plot_l_n=barplot(y_l_n$Counts_l_n, names.arg=y_l_n$CountH_l_n, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

#5000 setting

countf_Lasso_n # Countf_Lasso_n gives value of 7 in 1000 runs i.e the Lasso technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_l_n # 147 times trans_diff_qrt was chosen
countf_LikeSch_l_n # 153 times acce14 was chosen 
countf_TPoss_l_n # 299 times tposscale was chosen
countf_TNeg_l_n # 533 times tnegscale was chosen
countf_MalFem_l_n # 124 times bphc02a was chosen
countf_School_l_n # 53 times bcce37 was chosen
countf_LikeMat_l_n # 593 times acce16a was chosen
countf_Quali_l_n # 378 times bpce33a was chosen

count_0 # 0 s.predictors  chosen 282 times
count_1 # 1 s.predictors  chosen 170 times
count_2 # 2 s.predictors  chosen 117 times
count_3 # 3 s.predictors  chosen 151 times
count_4 # 4 s.predictors  chosen 113 times
count_5 # 5 s.predictors  chosen 81 times
count_6 # 6 s.predictors  chosen 46 times
count_7 # 7 s.predictors  chosen 30 times
count_8 # 8 s.predictors  chosen 10 times

#500 setting

countf_Lasso_n # Countf_Lasso_n gives value of 0 in 1000 runs i.e the Lasso technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_l_n # 38 times trans_diff_qrt was chosen
countf_LikeSch_l_n # 33 times acce14 was chosen 
countf_TPoss_l_n # 38 times tposscale was chosen
countf_TNeg_l_n # 53 times tnegscale was chosen
countf_MalFem_l_n # 17 times bphc02a was chosen
countf_School_l_n # 7 times bcce37 was chosen
countf_LikeMat_l_n # 64 times acce16a was chosen
countf_Quali_l_n # 46 times bpce33a was chosen

count_0 # 0 s.predictors  chosen 745 times
count_1 # 1 s.predictors  chosen 234 times
count_2 # 2 s.predictors  chosen 9 times
count_3 # 3 s.predictors  chosen 6 times
count_4 # 4 s.predictors  chosen 4 times
count_5 # 5 s.predictors  chosen 2 times
count_6 # 6 s.predictors  chosen 0 times
count_7 # 7 s.predictors  chosen 0 times
count_8 # 8 s.predictors  chosen 0 times

#********************************************************************************************************************************************************************************

#Evaluating if the LRT captures significant predictors


set.seed(904849011)
countf_LRT_i_d=0
countf_TransDiff_i_d =0
countf_LikeSch_i_d =0
countf_TPoss_i_d = 0
countf_TNeg_i_d = 0
countf_MalFem_i_d = 0
countf_School_i_d = 0
countf_LikeMat_i_d = 0
countf_SchoolPT_i_d = 0
countf_Adeq_No_i_d = 0
countf_Quali_i_d = 0
countf_CareQ_i_d = 0
countf_HSD_i_d = 0
countf_RelE_i_d = 0
countf_Ind_i_d = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
detach(gui_sim)
Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
for (i in 1:1000) 
{ 
  mysample<-gui_sim[sample(1:nrow(gui_sim),5000,replace=T),]
  attach(mysample)
  Eta = matrix(c(predict(fit, newdata=mysample)))
  library(LaplacesDemon)
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
  
  mysample1=data.frame(mysample,Ynew)
  
  LRT = drop1(glm(Ynew~trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50, family= binomial, data = mysample1), test = "LRT")
  LRT_Coeff =matrix(Beta_Significant[ which(LRT$`Pr(>Chi)`<=0.05)-1])
  count_0 = count_0 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==0)
  count_1 = count_1 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==1)
  count_2 = count_2 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==2)
  count_3 = count_3 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==3)
  count_4 = count_4 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==4)
  count_5 = count_5 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==5)
  count_6 = count_6 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==6)
  count_7 = count_7 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==7)
  count_8 = count_8 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==8)
  countf_LRT_i_d = countf_LRT_i_d + as.numeric(identical(sort(LRT_Coeff),sort(Beta_Significant)))
  countf_TransDiff_i_d = countf_TransDiff_i_d + as.numeric('trans_diff_qrt' %in% LRT_Coeff)
  countf_LikeSch_i_d = countf_LikeSch_i_d + as.numeric('acce14' %in% LRT_Coeff)
  countf_TPoss_i_d = countf_TPoss_i_d + as.numeric('tposscale' %in% LRT_Coeff)
  countf_TNeg_i_d = countf_TNeg_i_d + as.numeric('tnegscale' %in% LRT_Coeff)
  countf_MalFem_i_d = countf_MalFem_i_d + as.numeric('bphc02a' %in% LRT_Coeff)
  countf_School_i_d = countf_School_i_d + as.numeric('bcce37' %in% LRT_Coeff)
  countf_LikeMat_i_d = countf_LikeMat_i_d + as.numeric('acce16a' %in% LRT_Coeff)
  countf_Quali_i_d = countf_Quali_i_d + as.numeric('bcce50' %in% LRT_Coeff)
}

Counts_i_d = matrix(c(count_0,count_1,count_2,count_3,count_4,count_5,count_6,count_7,count_8))
CountH_i_d = matrix(c(0,1,2,3,4,5,6,7,8))
y_i_d = data.frame(CountH_i_d,Counts_i_d)
Plot_i_d = barplot(y_i_d$Counts_i_d,y_i_d$CountH_i_d, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

#5000 setting

countf_LRT_i_d # Countf_LRT_i_d gives value of 0 in 1000 runs i.e the LRT technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_i_d # 626 times trans_diff_qrt was chosen
countf_LikeSch_i_d # 588 times acce14 was chosen 
countf_TPoss_i_d # 998 times tposscale was chosen
countf_TNeg_i_d # 1000 times tnegscale was chosen
countf_MalFem_i_d # 996 times bphc02a was chosen
countf_School_i_d # 868 times bcce37 was chosen
countf_LikeMat_i_d # 1000 times acce16a was chosen
countf_Quali_i_d # 692 times bpce33a was chosen



count_0 # 0 s.predictors  chosen 0 times
count_1 # 1 s.predictors  chosen 0 times
count_2 # 2 s.predictors  chosen 0 times
count_3 # 3 s.predictors  chosen 0 times
count_4 # 4 s.predictors  chosen 2 times
count_5 # 5 s.predictors  chosen 65 times
count_6 # 6 s.predictors  chosen 292 times
count_7 # 7 s.predictors  chosen 445 times
count_8 # 8 s.predictors  chosen 196 times

#500 setting

countf_LRT_i_d # Countf_LRT_i_d gives value of 0 in 1000 runs i.e the LRT technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_i_d # 136 times trans_diff_qrt was chosen
countf_LikeSch_i_d # 110 times acce14 was chosen 
countf_TPoss_i_d # 262 times tposscale was chosen
countf_TNeg_i_d # 689 times tnegscale was chosen
countf_MalFem_i_d # 318 times bphc02a was chosen
countf_School_i_d # 173 times bcce37 was chosen
countf_LikeMat_i_d # 692 times acce16a was chosen
countf_Quali_i_d # 122 times bpce33a was chosen

count_0 # 0 s.predictors  chosen 27 times
count_1 # 1 s.predictors  chosen 158 times
count_2 # 2 s.predictors  chosen 325 times
count_3 # 3 s.predictors  chosen 312 times
count_4 # 4 s.predictors  chosen 139 times
count_5 # 5 s.predictors  chosen 32 times
count_6 # 6 s.predictors  chosen 7 times
count_7 # 7 s.predictors  chosen 0 times
count_8 # 8 s.predictors  chosen 0 times



#Evaluating if the LRT captures with Normal variate


set.seed(904849011)
countf_LRT_i_d=0
countf_TransDiff_i_d =0
countf_LikeSch_i_d =0
countf_TPoss_i_d = 0
countf_TNeg_i_d = 0
countf_MalFem_i_d = 0
countf_School_i_d = 0
countf_LikeMat_i_d = 0
countf_SchoolPT_i_d = 0
countf_Adeq_No_i_d = 0
countf_Quali_i_d = 0
countf_CareQ_i_d = 0
countf_HSD_i_d = 0
countf_RelE_i_d = 0
countf_Ind_i_d = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
detach(gui_sim)
Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
for (i in 1:1000) 
{ 
  mysample<-gui_sim[sample(1:nrow(gui_sim),500,replace=T),]
  attach(mysample)
  Eta = matrix(c(predict(fit, newdata=mysample))) + rnorm(500,0,4)
  library(LaplacesDemon)
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
  
  mysample1=data.frame(mysample,Ynew)
  
  LRT = drop1(glm(Ynew~trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50, family= binomial, data = mysample1), test = "LRT")
  LRT_Coeff =matrix(Beta_Significant[ which(LRT$`Pr(>Chi)`<=0.05)-1])
  count_0 = count_0 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==0)
  count_1 = count_1 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==1)
  count_2 = count_2 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==2)
  count_3 = count_3 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==3)
  count_4 = count_4 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==4)
  count_5 = count_5 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==5)
  count_6 = count_6 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==6)
  count_7 = count_7 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==7)
  count_8 = count_8 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==8)
  countf_LRT_i_d = countf_LRT_i_d + as.numeric(identical(sort(LRT_Coeff),sort(Beta_Significant)))
  countf_TransDiff_i_d = countf_TransDiff_i_d + as.numeric('trans_diff_qrt' %in% LRT_Coeff)
  countf_LikeSch_i_d = countf_LikeSch_i_d + as.numeric('acce14' %in% LRT_Coeff)
  countf_TPoss_i_d = countf_TPoss_i_d + as.numeric('tposscale' %in% LRT_Coeff)
  countf_TNeg_i_d = countf_TNeg_i_d + as.numeric('tnegscale' %in% LRT_Coeff)
  countf_MalFem_i_d = countf_MalFem_i_d + as.numeric('bphc02a' %in% LRT_Coeff)
  countf_School_i_d = countf_School_i_d + as.numeric('bcce37' %in% LRT_Coeff)
  countf_LikeMat_i_d = countf_LikeMat_i_d + as.numeric('acce16a' %in% LRT_Coeff)
  countf_Quali_i_d = countf_Quali_i_d + as.numeric('bcce50' %in% LRT_Coeff)
}

Counts_i_d = matrix(c(count_0,count_1,count_2,count_3,count_4,count_5,count_6,count_7,count_8))
CountH_i_d = matrix(c(0,1,2,3,4,5,6,7,8))
y_i_d = data.frame(CountH_i_d,Counts_i_d)
Plot_i_d = barplot(y_i_d$Counts_i_d,y_i_d$CountH_i_d, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

#5000 setting

countf_LRT_i_d # Countf_LRT_i_d gives value of 1 in 1000 runs i.e the LRT technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_i_d # 182 times trans_diff_qrt was chosen
countf_LikeSch_i_d # 138 times acce14 was chosen 
countf_TPoss_i_d # 497 times tposscale was chosen
countf_TNeg_i_d # 953 times tnegscale was chosen
countf_MalFem_i_d # 622 times bphc02a was chosen
countf_School_i_d # 334 times bcce37 was chosen
countf_LikeMat_i_d # 967 times acce16a was chosen
countf_Quali_i_d # 184 times bpce33a was chosen



count_0 # 0 s.predictors  chosen 0 times
count_1 # 1 s.predictors  chosen 2 times
count_2 # 2 s.predictors  chosen 85 times
count_3 # 3 s.predictors  chosen 295 times
count_4 # 4 s.predictors  chosen 341 times
count_5 # 5 s.predictors  chosen 215 times
count_6 # 6 s.predictors  chosen 54 times
count_7 # 7 s.predictors  chosen 7 times
count_8 # 8 s.predictors  chosen 1 times

#500 setting

countf_LRT_i_d # Countf_LRT_i_d gives value of 0 in 1000 runs i.e the LRT technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_i_d # 59 times trans_diff_qrt was chosen
countf_LikeSch_i_d # 62 times acce14 was chosen 
countf_TPoss_i_d # 97 times tposscale was chosen
countf_TNeg_i_d # 167 times tnegscale was chosen
countf_MalFem_i_d # 118 times bphc02a was chosen
countf_School_i_d # 78 times bcce37 was chosen
countf_LikeMat_i_d # 192 times acce16a was chosen
countf_Quali_i_d # 68 times bpce33a was chosen

count_0 # 0 s.predictors  chosen 406 times
count_1 # 1 s.predictors  chosen 390 times
count_2 # 2 s.predictors  chosen 165 times
count_3 # 3 s.predictors  chosen 35 times
count_4 # 4 s.predictors  chosen 4 times
count_5 # 5 s.predictors  chosen 0 times
count_6 # 6 s.predictors  chosen 0 times
count_7 # 7 s.predictors  chosen 0 times
count_8 # 8 s.predictors  chosen 0 times


#Evaluating if the LRT captures  non significant predictors


f=1
set.seed(904849011)
countf_LRT_i_d=0
countf_TransDiff_i_d =0
countf_LikeSch_i_d =0
countf_TPoss_i_d = 0
countf_TNeg_i_d = 0
countf_MalFem_i_d = 0
countf_School_i_d = 0
countf_LikeMat_i_d = 0
countf_SchoolPT_i_d = 0
countf_Adeq_No_i_d = 0
countf_Quali_i_d = 0
countf_CareQ_i_d = 0
countf_HSD_i_d = 0
countf_RelE_i_d = 0
countf_Ind_i_d = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
ncount_0 = 0
ncount_1 = 0
ncount_2 = 0
ncount_3 = 0
ncount_4 = 0
ncount_5 = 0
ncount_6 = 0
ncount_7 = 0
ncount_8 = 0
NonBeta_Significant=matrix(c('bpce33a','bhpc19a','bpsd40a','bdsd57a','bhpc55','bhpc61a'),byrow = T)
detach(gui_sim)
Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
List = matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50','bhpc19a','bpce33a','bpsd40a','bdsd57a','bhpc55','bhpc61a'),byrow = T)
All_Predictors = matrix(c(names(gui_sim[,-1])))
for (i in 1:1000) 
{ 
  mysample<-gui_sim[sample(1:nrow(gui_sim),5000,replace=T),]
  attach(mysample)
  Eta = matrix(c(predict(fit, newdata=mysample)))
  library(LaplacesDemon)
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))

  mysample1=data.frame(mysample,Ynew)
  
  LRT = drop1(glm(Ynew~trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50 +bhpc19a + bpce33a + bpsd40a + bdsd57a + bhpc55 + bhpc61a, family= binomial, data = mysample1), test = "LRT")
  LRT_Coeff =matrix(List[ which(LRT$`Pr(>Chi)`<=0.05)-1])
  count_0 = count_0 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==0)
  count_1 = count_1 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==1)
  count_2 = count_2 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==2)
  count_3 = count_3 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==3)
  count_4 = count_4 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==4)
  count_5 = count_5 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==5)
  count_6 = count_6 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==6)
  count_7 = count_7 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==7)
  count_8 = count_8 + as.numeric(length(intersect(LRT_Coeff,Beta_Significant))==8)
  ncount_0 = ncount_0 + as.numeric(length(intersect(LRT_Coeff,NonBeta_Significant))==0)
  ncount_1 = ncount_1 + as.numeric(length(intersect(LRT_Coeff,NonBeta_Significant))==1)
  ncount_2 = ncount_2 + as.numeric(length(intersect(LRT_Coeff,NonBeta_Significant))==2)
  ncount_3 = ncount_3 + as.numeric(length(intersect(LRT_Coeff,NonBeta_Significant))==3)
  ncount_4 = ncount_4 + as.numeric(length(intersect(LRT_Coeff,NonBeta_Significant))==4)
  ncount_5 = ncount_5 + as.numeric(length(intersect(LRT_Coeff,NonBeta_Significant))==5)
  ncount_6 = ncount_6 + as.numeric(length(intersect(LRT_Coeff,NonBeta_Significant))==6)
  countf_LRT_i_d = countf_LRT_i_d + as.numeric(identical(sort(LRT_Coeff),sort(Beta_Significant)))
  countf_TransDiff_i_d = countf_TransDiff_i_d + as.numeric('trans_diff_qrt' %in% LRT_Coeff)
  countf_LikeSch_i_d = countf_LikeSch_i_d + as.numeric('acce14' %in% LRT_Coeff)
  countf_TPoss_i_d = countf_TPoss_i_d + as.numeric('tposscale' %in% LRT_Coeff)
  countf_TNeg_i_d = countf_TNeg_i_d + as.numeric('tnegscale' %in% LRT_Coeff)
  countf_MalFem_i_d = countf_MalFem_i_d + as.numeric('bphc02a' %in% LRT_Coeff)
  countf_School_i_d = countf_School_i_d + as.numeric('bcce37' %in% LRT_Coeff)
  countf_LikeMat_i_d = countf_LikeMat_i_d + as.numeric('acce16a' %in% LRT_Coeff)
  countf_SchoolPT_i_d = countf_SchoolPT_i_d + as.numeric('bpce33a' %in% LRT_Coeff)
  countf_Adeq_No_i_d = countf_Adeq_No_i_d + as.numeric('bhpc19a' %in% LRT_Coeff)
  countf_Quali_i_d = countf_Quali_i_d + as.numeric('bcce50' %in% LRT_Coeff)
  countf_CareQ_i_d = countf_CareQ_i_d + as.numeric('bpsd40a' %in% LRT_Coeff)
  countf_HSD_i_d = countf_HSD_i_d + as.numeric('bdsd57a' %in% LRT_Coeff)
  countf_RelE_i_d = countf_RelE_i_d + as.numeric('bhpc55' %in% LRT_Coeff)
  countf_Ind_i_d = countf_Ind_i_d + as.numeric('bhpc61a' %in% LRT_Coeff)
detach(mysample)
  f =f+1
  print(f)
  }

Counts_i_d = matrix(c(count_0,count_1,count_2,count_3,count_4,count_5,count_6,count_7,count_8))
CountH_i_d = matrix(c(0,1,2,3,4,5,6,7,8))
y_i_d = data.frame(CountH_i_d,Counts_i_d)
Plot_i_d = barplot(y_i_d$Counts_i_d,y_i_d$CountH_i_d, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

#5000 setting

countf_LRT_i_d # Countf_LRT_i_d gives value of 0 in 1000 runs i.e the LRT technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_i_d # 636 times trans_diff_qrt was chosen
countf_LikeSch_i_d # 577 times acce14 was chosen 
countf_TPoss_i_d # 998 times tposscale was chosen
countf_TNeg_i_d # 1000 times tnegscale was chosen
countf_MalFem_i_d # 996 times bphc02a was chosen
countf_School_i_d # 860 times bcce37 was chosen
countf_LikeMat_i_d # 1000 times acce16a was chosen
countf_Quali_i_d # 676 times bpce33a was chosen
 

# Insignificant predictors chosen

countf_Adeq_No_i_d # 52 times bpce33a was chosen
countf_SchoolPT_i_d # 65 times bpce33a was chosen
countf_CareQ_i_d # 55 times bpce33a was chosen
countf_HSD_i_d # 56 times bpce33a was chosen
countf_RelE_i_d # 49 times bpce33a was chosen
countf_Ind_i_d # 65 times bpce33a was chosen

count_0 # 0 s.predictors  chosen 0 times
count_1 # 1 s.predictors  chosen 0 times
count_2 # 2 s.predictors  chosen 0 times
count_3 # 3 s.predictors  chosen 0 times
count_4 # 4 s.predictors  chosen 4 times
count_5 # 5 s.predictors  chosen 69 times
count_6 # 6 s.predictors  chosen 298 times
count_7 # 7 s.predictors  chosen 438 times
count_8 # 8 s.predictors  chosen 191 times

ncount_0 # 0 s.predictors  chosen 714 times
ncount_1 # 1 s.predictors  chosen 236 times
ncount_2 # 2 s.predictors  chosen 44 times
ncount_3 # 3 s.predictors  chosen 6 times
ncount_4 # 4 s.predictors  chosen 0 times
ncount_5 # 5 s.predictors  chosen 0 times
ncount_6 # 6 s.predictors  chosen 0 times

#500 setting

countf_LRT_i_d # Countf_LRT_i_d gives value of 0 in 1000 runs i.e the LRT technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_i_d # 144 times trans_diff_qrt was chosen
countf_LikeSch_i_d # 125 times acce14 was chosen 
countf_TPoss_i_d # 261 times tposscale was chosen
countf_TNeg_i_d # 686 times tnegscale was chosen
countf_MalFem_i_d # 327 times bphc02a was chosen
countf_School_i_d # 174 times bcce37 was chosen
countf_LikeMat_i_d # 693 times acce16a was chosen
countf_Quali_i_d # 136 times bpce33a was chosen


# Insignificant predictors chosen

countf_Adeq_No_i_d # 71 times bpce33a was chosen
countf_SchoolPT_i_d # 65 times bpce33a was chosen
countf_CareQ_i_d # 71 times bpce33a was chosen
countf_HSD_i_d # 80 times bpce33a was chosen
countf_RelE_i_d # 53 times bpce33a was chosen
countf_Ind_i_d # 68 times bpce33a was chosen

count_0 # 0 s.predictors  chosen 19 times
count_1 # 1 s.predictors  chosen 106 times
count_2 # 2 s.predictors  chosen 251 times
count_3 # 3 s.predictors  chosen 322 times
count_4 # 4 s.predictors  chosen 180 times
count_5 # 5 s.predictors  chosen 90 times
count_6 # 6 s.predictors  chosen 24 times
count_7 # 7 s.predictors  chosen 6 times
count_8 # 8 s.predictors  chosen 2 times

ncount_0 # 0 s.predictors  chosen 667 times
ncount_1 # 1 s.predictors  chosen 273 times
ncount_2 # 2 s.predictors  chosen 53 times
ncount_3 # 3 s.predictors  chosen 7 times
ncount_4 # 4 s.predictors  chosen 0 times
ncount_5 # 5 s.predictors  chosen 0 times
ncount_6 # 6 s.predictors  chosen 0 times


# PLOT OF FREQ OF NO OF CORRECT PREDICTORS





##*****************************************************************************************************************************************************************************************************************************************

#*****************************************************************************************************************************************************************************************************************************

#*****************************************************************************************************************************************************************************

#AUTOMATION

# Automating the Stepwise Model Selection to continously update the significant predictors amongst addition of 8 insignificant predictors from the predictor pool

# Initial Significant Predictors chosen :
#                                          **VARIABLE NAME**                                               **RESPONSES**     
#                                 1.	trans_diff_qrt----Measure of transition difficulty                 ([5, 10], [13,15],[10,13],[15,35])
#                                 2.	acce14------------What do you think about school ?                 (Always Like it, Never Like it, Sometimes Like it)
#                                 3.  tposscale---------Positive interaction with teachers               (2.75, 3, <= 2.5, >3)  
#                                 4.  tnegscale---------Negative interaction with teachers               (2.75, 3, <= 2.5, >3)
#                                 5.  bphc02a-----------Male/Female                                      (Male, Female)
#                                 6.  bcce37------------1st/2nd year of secondary school                 (First year, Second Year)
#                                 7.  acce16a-----------Like: Maths                                      (Always Like it, Never Like it, Sometimes Like it)
#                                 8.  bpce33a-----------Have parents attended PT Meeting ?               (No, Yes)


set.seed(9048490112)
detach(gui_sim)
All_Predictors = matrix(c(names(gui_sim[,-1])))
Beta_Significant = matrix(c(All_Predictors[4],All_Predictors[5],All_Predictors[9],All_Predictors[20],All_Predictors[22],All_Predictors[82],All_Predictors[83], All_Predictors[90]))
for (i in length(All_Predictors)) 
{
  fit2 = glm(factor(maths_engagement) ~., data = gui_sim[,which(colnames(gui_sim)%in%Beta_Significant)], family = "binomial")
  Beta_NonSignificant_full = matrix(c(setdiff(All_Predictors, Beta_Significant)))
  Beta_NonSignificant = matrix(Beta_NonSignificant_full[sample(1:length(Beta_NonSignificant_full),8,replace = T)])
  Beta_Predictors = matrix(c(Beta_Significant,Beta_NonSignificant))
  countf_AIC_i=0
  countf_Full= matrix(0,nrow = 16)
  for (i in 1:1000) 
  { 
    mysample<-gui_sim[sample(1:nrow(gui_sim),5000,replace=T),]
    attach(mysample)
    Eta = matrix(c(predict(fit2, newdata=mysample)))
    library(LaplacesDemon)
    Etap=invlogit(Eta)
    Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
    mysample1=data.frame(mysample,Ynew)
    fitd=glm(Ynew~1, family= binomial, data = mysample1)
    AIC=step(fitd,scope=list(lower=~1,upper=terms(factor(maths_engagement) ~., data = mysample1[,which(colnames(gui_sim)%in%Beta_Predictors)]),direction="both",trace=1))
    AIC_Coeff<- if(is.null(AIC$xlevels)) matrix(,) else matrix(c(names(AIC$xlevels)),byrow = F)
    identical(sort(AIC_Coeff),sort(Beta_Significant))
    countf_AIC_i = countf_AIC_i + as.numeric(identical(sort(AIC_Coeff),sort(Beta_Significant)))
    countf_Full[1] = countf_Full[1] + as.numeric(Beta_Significant[1] %in% AIC_Coeff)
    countf_Full[2] = countf_Full[2] + as.numeric(Beta_Significant[2] %in% AIC_Coeff)
    countf_Full[3] = countf_Full[3] + as.numeric(Beta_Significant[3] %in% AIC_Coeff)
    countf_Full[4] = countf_Full[4] + as.numeric(Beta_Significant[4] %in% AIC_Coeff)
    countf_Full[5] = countf_Full[5] + as.numeric(Beta_Significant[5] %in% AIC_Coeff)
    countf_Full[6] = countf_Full[6] + as.numeric(Beta_Significant[6] %in% AIC_Coeff)
    countf_Full[7] = countf_Full[7] + as.numeric(Beta_Significant[7] %in% AIC_Coeff)
    countf_Full[8] = countf_Full[8] + as.numeric(Beta_Significant[8] %in% AIC_Coeff)
    countf_Full[9] = countf_Full[9] + as.numeric(Beta_NonSignificant[1] %in% AIC_Coeff)
    countf_Full[10] = countf_Full[10] + as.numeric(Beta_NonSignificant[2] %in% AIC_Coeff)
    countf_Full[11] = countf_Full[11] + as.numeric(Beta_NonSignificant[3] %in% AIC_Coeff)
    countf_Full[12] = countf_Full[12] + as.numeric(Beta_NonSignificant[4] %in% AIC_Coeff)
    countf_Full[13] = countf_Full[13] + as.numeric(Beta_NonSignificant[5] %in% AIC_Coeff)
    countf_Full[14] = countf_Full[14] + as.numeric(Beta_NonSignificant[6] %in% AIC_Coeff)
    countf_Full[15] = countf_Full[15] + as.numeric(Beta_NonSignificant[7] %in% AIC_Coeff)
    countf_Full[16] = countf_Full[16] + as.numeric(Beta_NonSignificant[8] %in% AIC_Coeff)
  }
  countf_Full_s <- sort(countf_Full,index.return=TRUE, decreasing=TRUE)
  lapply(countf_Full_s, `[`, countf_Full_s$x %in% head(unique(countf_Full_s$x),8))
  Beta_Significant = matrix(c(Beta_Predictors[countf_Full_s$ix[1:8]]))
}

#********************************************************************************************************************************************************************************************

#STABILITY SELECTION# with insignificant predictors



#                                               **VARIABLE NAME**                                               **RESPONSES**     
# Insignificant Predictors chosen :1.	Bphc19a----How adequate No of teachers                             (Fair, Good, Poor)
#                                  2.	Bcce50-----Highest qualification                                   (Certificate or Diploma (including plc., apprenticeship), Degree or higher degree, Junior Cert, Leaving Cert)
#                                  3.	Bpsd40a----Primary caretaker’s highest qualification               (Hi Sec/TechVoc/UppSec+Tech/Voc,Lower Sec, Non Degree, Postgrad, Primary) )
#                                  4.	Bdsd57a----HSD class (family’s social)                             (Non/skilled man, Not active, Prof/manager, Semi-U.S. man)
#                                  5.	Bhpc55-----Religious ethos of school                               (Non RC, Roman Catholic)
#                                  6.	Bhpc61a----Induction Day                                           (No, Yes)

f =1
detach(gui_sim)
set.seed(904849011)
stabfinal = c()
countf_TransDiff_s =0
countf_LikeSch_s =0
countf_TPoss_s = 0
countf_TNeg_s = 0
countf_MalFem_s = 0
countf_School_s = 0
countf_LikeMat_s = 0
countf_SchoolPT_s = 0
countf_Adeq_No_s = 0
countf_Quali_s = 0
countf_CareQ_s = 0
countf_HSD_s = 0
countf_RelE_s = 0
countf_Ind_s = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
ncount_0 = 0
ncount_1 = 0
ncount_2 = 0
ncount_3 = 0
ncount_4 = 0
ncount_5 = 0
ncount_6 = 0
ncount_7 = 0
ncount_8 = 0
Stabsel = c()
NonBeta_Significant=matrix(c('bpce33a','bhpc19a','bpsd40a','bdsd57a','bhpc55','bhpc61a'),byrow = T)
Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
All_Predictors = matrix(c(names(gui_sim[,-1])))
for (q in 1:1000) 
{

countf_Lasso_i=0
countf_TransDiff_l_i =0
countf_LikeSch_l_i =0
countf_TPoss_l_i = 0
countf_TNeg_l_i = 0
countf_MalFem_l_i = 0
countf_School_l_i = 0
countf_LikeMat_l_i = 0
countf_SchoolPT_l_i = 0
countf_Adeq_No_l_i = 0
countf_Quali_l_i = 0
countf_CareQ_l_i = 0
countf_HSD_l_i = 0
countf_RelE_l_i = 0
countf_Ind_l_i = 0

stabcount = c()


L =c()
mysample<-gui_sim[sample(1:nrow(gui_sim),500,replace=T),]
attach(mysample)
Eta = matrix(c(predict(fit, newdata=mysample)))
library(LaplacesDemon)
Etap=invlogit(Eta)
Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
mysample1=data.frame(mysample,Ynew)
library(glmnet)
preds=model.matrix(~ trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50 + bhpc19a + bpce33a + bpsd40a + bdsd57a + bhpc55 + bhpc61a)
cv=cv.glmnet(preds,Ynew,family="binomial",alpha=1)
L = matrix(c(L,cv$lambda[cv$lambda >= cv$lambda.min & cv$lambda <= cv$lambda.1se]))
Luniq = matrix(unique(L))
detach(mysample)
length(Luniq)
  countf_Lasso_i=0
  countf_TransDiff_l_i =0
  countf_LikeSch_l_i =0
  countf_TPoss_l_i = 0
  countf_TNeg_l_i = 0
  countf_MalFem_l_i = 0
  countf_School_l_i = 0
  countf_LikeMat_l_i = 0
  countf_SchoolPT_l_i = 0
  countf_Adeq_No_l_i = 0
  countf_Quali_l_i = 0
  countf_CareQ_l_i = 0
  countf_HSD_l_i = 0
  countf_RelE_l_i = 0
  countf_Ind_l_i = 0
for (i in 1:500) {
  Lasso_Coeff_rd = c()
  sample_k<-mysample[sample(1:nrow(mysample),250,replace=F),]
  attach(sample_k)
  Eta = matrix(c(predict(fit, newdata=sample_k)))
  library(LaplacesDemon)
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
  sample_k_1=data.frame(sample_k,Ynew)
  library(glmnet)
  preds=model.matrix(~ trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bpce33a + bhpc19a + bcce50 + bpsd40a + bdsd57a + bhpc55 + bhpc61a)
  lasso_fit=glmnet(preds,Ynew,family="binomial",alpha=1,lambda=Luniq[length(Luniq)])
  Lasso_Coeff<- if(is.null(names(which(abs(lasso_fit$beta[,1])>0)))) matrix(,) else matrix(c(names(which(abs(lasso_fit$beta[,1])>0))), byrow = F)
  for (j in 1:length(All_Predictors)) 
    Lasso_Coeff_rd = c(Lasso_Coeff_rd, regmatches(Lasso_Coeff, regexpr(All_Predictors[j], Lasso_Coeff)))
  Lasso_Coeff_rdn = matrix(unique(Lasso_Coeff_rd))
  Lasso_Coeff_rdn_l = matrix(c(Lasso_Coeff_rdn,Luniq[length(Luniq)]))
  countf_Lasso_i = countf_Lasso_i + as.numeric(identical(sort(Lasso_Coeff_rdn),sort(Beta_Significant)))
  countf_TransDiff_l_i = countf_TransDiff_l_i + as.numeric('trans_diff_qrt' %in% Lasso_Coeff_rdn)
  countf_LikeSch_l_i = countf_LikeSch_l_i + as.numeric('acce14' %in% Lasso_Coeff_rdn)
  countf_TPoss_l_i = countf_TPoss_l_i + as.numeric('tposscale' %in% Lasso_Coeff_rdn)
  countf_TNeg_l_i = countf_TNeg_l_i + as.numeric('tnegscale' %in% Lasso_Coeff_rdn)
  countf_MalFem_l_i = countf_MalFem_l_i + as.numeric('bphc02a' %in% Lasso_Coeff_rdn)
  countf_School_l_i = countf_School_l_i + as.numeric('bcce37' %in% Lasso_Coeff_rdn)
  countf_LikeMat_l_i = countf_LikeMat_l_i + as.numeric('acce16a' %in% Lasso_Coeff_rdn)
  countf_SchoolPT_l_i = countf_SchoolPT_l_i + as.numeric('bpce33a' %in% Lasso_Coeff_rdn)
  countf_Adeq_No_l_i = countf_Adeq_No_l_i + as.numeric('bhpc19a' %in% Lasso_Coeff_rdn)
  countf_Quali_l_i = countf_Quali_l_i + as.numeric('bcce50' %in% Lasso_Coeff_rdn)
  countf_CareQ_l_i = countf_CareQ_l_i + as.numeric('bpsd40a' %in% Lasso_Coeff_rdn)
  countf_HSD_l_i = countf_HSD_l_i + as.numeric('bdsd57a' %in% Lasso_Coeff_rdn)
  countf_RelE_l_i = countf_RelE_l_i + as.numeric('bhpc55' %in% Lasso_Coeff_rdn)
  countf_Ind_l_i = countf_Ind_l_i + as.numeric('bhpc61a' %in% Lasso_Coeff_rdn)
  detach(sample_k)
  }
  stabcount = rbind(stabcount,cbind(countf_TransDiff_l_i,countf_LikeSch_l_i,countf_TPoss_l_i,countf_TNeg_l_i,countf_MalFem_l_i,countf_School_l_i, countf_LikeMat_l_i,countf_SchoolPT_l_i,countf_Adeq_No_l_i,countf_Quali_l_i,countf_CareQ_l_i,countf_HSD_l_i,countf_RelE_l_i,countf_Ind_l_i))
  rownames(stabcount) = c(paste("Lambda", Luniq[length(Luniq)]))
  colnames(stabcount) = c("trans_diff_qrt","acce14","tposscale","tnegscale","bphc02a","bcce37","acce16a","bpce33a","bhpc19a","bcce50","bpsd40a","bdsd57a","bhpc55","bhpc61a")
  stabfinal = rbind(stabfinal,stabcount)
  Stabsel = matrix(colnames(stabcount)[which(stabfinal[q,]>=350)])
  count_0 = count_0 + as.numeric(length(intersect(Stabsel,Beta_Significant))==0)
  count_1 = count_1 + as.numeric(length(intersect(Stabsel,Beta_Significant))==1)
  count_2 = count_2 + as.numeric(length(intersect(Stabsel,Beta_Significant))==2)
  count_3 = count_3 + as.numeric(length(intersect(Stabsel,Beta_Significant))==3)
  count_4 = count_4 + as.numeric(length(intersect(Stabsel,Beta_Significant))==4)
  count_5 = count_5 + as.numeric(length(intersect(Stabsel,Beta_Significant))==5)
  count_6 = count_6 + as.numeric(length(intersect(Stabsel,Beta_Significant))==6)
  count_7 = count_7 + as.numeric(length(intersect(Stabsel,Beta_Significant))==7)
  count_8 = count_8 + as.numeric(length(intersect(Stabsel,Beta_Significant))==8)
  ncount_0 = ncount_0 + as.numeric(length(intersect(Stabsel,NonBeta_Significant))==0)
  ncount_1 = ncount_1 + as.numeric(length(intersect(Stabsel,NonBeta_Significant))==1)
  ncount_2 = ncount_2 + as.numeric(length(intersect(Stabsel,NonBeta_Significant))==2)
  ncount_3 = ncount_3 + as.numeric(length(intersect(Stabsel,NonBeta_Significant))==3)
  ncount_4 = ncount_4 + as.numeric(length(intersect(Stabsel,NonBeta_Significant))==4)
  ncount_5 = ncount_5 + as.numeric(length(intersect(Stabsel,NonBeta_Significant))==5)
  ncount_6 = ncount_6 + as.numeric(length(intersect(Stabsel,NonBeta_Significant))==6)
  ncount_7 = ncount_7 + as.numeric(length(intersect(Stabsel,NonBeta_Significant))==7)
  ncount_8 = ncount_8 + as.numeric(length(intersect(Stabsel,NonBeta_Significant))==8)
  countf_TransDiff_s = countf_TransDiff_s + as.numeric('trans_diff_qrt' %in% Stabsel)
  countf_LikeSch_s = countf_LikeSch_s + as.numeric('acce14' %in% Stabsel)
  countf_TPoss_s = countf_TPoss_s + as.numeric('tposscale' %in% Stabsel)
  countf_TNeg_s = countf_TNeg_s + as.numeric('tnegscale' %in% Stabsel)
  countf_MalFem_s = countf_MalFem_s + as.numeric('bphc02a' %in% Stabsel)
  countf_School_s = countf_School_s + as.numeric('bcce37' %in% Stabsel)
  countf_LikeMat_s = countf_LikeMat_s + as.numeric('acce16a' %in% Stabsel)
  countf_Quali_s = countf_Quali_s + as.numeric('bcce50' %in% Stabsel)
  countf_SchoolPT_s = countf_SchoolPT_s + as.numeric('bpce33a' %in% Stabsel)
  countf_Adeq_No_s = countf_Adeq_No_s + as.numeric('bhpc19a' %in% Stabsel)
  countf_CareQ_s = countf_CareQ_s + as.numeric('bpsd40a' %in% Stabsel)
  countf_HSD_s = countf_HSD_s + as.numeric('bdsd57a' %in% Stabsel)
  countf_RelE_s = countf_RelE_s + as.numeric('bhpc55' %in% Stabsel)
  countf_Ind_s = countf_Ind_s + as.numeric('bhpc61a' %in% Stabsel)

  f = f+1
  
}


Counts_l_i = matrix(c(count_0,count_1,count_2,count_3,count_4,count_5,count_6,count_7,count_8))
CountH_l_i = matrix(c(0,1,2,3,4,5,6,7,8))
y_l_i = data.frame(CountH_l_i,Counts_l_i)
Plot_l_i=barplot(y_l_i$Counts_l_i, names.arg=y_l_i$CountH_l_i, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 



#5000 setting

countf_Lasso_s # Countf_Lasso_i gives value of 995 in 250 runs i.e the Lasso technique only captures the right significant predictors 0 time in 1000 runs
countf_TransDiff_s # 1000 times trans_diff_qrt was chosen
countf_LikeSch_s # 1000 times acce14 was chosen 
countf_TPoss_s # 1000 times tposscale was chosen
countf_TNeg_s # 1000 times tnegscale was chosen
countf_MalFem_s # 1000 times bphc02a was chosen
countf_School_s # 1000 times bcce37 was chosen
countf_LikeMat_s # 1000 times acce16a was chosen
countf_Quali_s # 1000 times bcce50 was chosen

# Insignificant predictors chosen
countf_Adeq_No_s # 1000 times bhpc19a was chosen
countf_SchoolPT_s # 905 times bpce33a was chosen
countf_CareQ_s # 1000 times bpsd40a was chosen
countf_HSD_s # 1000 times bdsd57a was chosen
countf_RelE_s # 979 times bhpc55 was chosen
countf_Ind_s # 993 times bhpc61a was chosen


count_0 # 0 s.predictors  chosen 0 times
count_1 # 1 s.predictors  chosen 0 times
count_2 # 2 s.predictors  chosen 0 times
count_3 # 3 s.predictors  chosen 0 times
count_4 # 4 s.predictors  chosen 0 times
count_5 # 5 s.predictors  chosen 0 times
count_6 # 6 s.predictors  chosen 0 times
count_7 # 7 s.predictors  chosen 0 times
count_8 # 8 s.predictors  chosen 250 times

ncount_0 # 0 s.predictors  chosen 0 times
ncount_1 # 1 s.predictors  chosen 0 times
ncount_2 # 2 s.predictors  chosen 0 times
ncount_3 # 3 s.predictors  chosen 7 times
ncount_4 # 4 s.predictors  chosen 14 times
ncount_5 # 5 s.predictors  chosen 74 times
ncount_6 # 6 s.predictors  chosen 905 times


#500 setting

countf_Lasso_s # Countf_Lasso_i gives value of 995 in 250 runs i.e the Lasso technique only captures the right significant predictors 0 time in 1000 runs
countf_TransDiff_s # 707 times trans_diff_qrt was chosen
countf_LikeSch_s # 561 times acce14 was chosen 
countf_TPoss_s # 794 times tposscale was chosen
countf_TNeg_s # 882 times tnegscale was chosen
countf_MalFem_s # 156 times bphc02a was chosen
countf_School_s # 58 times bcce37 was chosen
countf_LikeMat_s # 936 times acce16a was chosen
countf_Quali_s # 801 times bcce50 was chosen

# Insignificant predictors chosen
countf_Adeq_No_s # 719 times bhpc19a was chosen
countf_SchoolPT_s # 31 times bpce33a was chosen
countf_CareQ_s # 744 times bpsd40a was chosen
countf_HSD_s # 633 times bdsd57a was chosen
countf_RelE_s # 87 times bhpc55 was chosen
countf_Ind_s # 169 times bhpc61a was chosen


count_0 # 0 s.predictors  chosen 63 times
count_1 # 1 s.predictors  chosen 56 times
count_2 # 2 s.predictors  chosen 63 times
count_3 # 3 s.predictors  chosen 39 times
count_4 # 4 s.predictors  chosen 74 times
count_5 # 5 s.predictors  chosen 144 times
count_6 # 6 s.predictors  chosen 405 times
count_7 # 7 s.predictors  chosen 98 times
count_8 # 8 s.predictors  chosen 58 times

ncount_0 # 0 s.predictors  chosen 253 times
ncount_1 # 1 s.predictors  chosen 31 times
ncount_2 # 2 s.predictors  chosen 83 times
ncount_3 # 3 s.predictors  chosen 464 times
ncount_4 # 4 s.predictors  chosen 82 times
ncount_5 # 5 s.predictors  chosen 56 times
ncount_6 # 6 s.predictors  chosen 31 times


#STABILITY SELECTION# with normal variate



#                                               **VARIABLE NAME**                                               **RESPONSES**     
# Insignificant Predictors chosen :1.	Bphc19a----How adequate No of teachers                             (Fair, Good, Poor)
#                                  2.	Bcce50-----Highest qualification                                   (Certificate or Diploma (including plc., apprenticeship), Degree or higher degree, Junior Cert, Leaving Cert)
#                                  3.	Bpsd40a----Primary caretaker’s highest qualification               (Hi Sec/TechVoc/UppSec+Tech/Voc,Lower Sec, Non Degree, Postgrad, Primary) )
#                                  4.	Bdsd57a----HSD class (family’s social)                             (Non/skilled man, Not active, Prof/manager, Semi-U.S. man)
#                                  5.	Bhpc55-----Religious ethos of school                               (Non RC, Roman Catholic)
#                                  6.	Bhpc61a----Induction Day                                           (No, Yes)

f =1
detach(gui_sim)
set.seed(904849011)
stabfinal = c()
countf_TransDiff_s =0
countf_LikeSch_s =0
countf_TPoss_s = 0
countf_TNeg_s = 0
countf_MalFem_s = 0
countf_School_s = 0
countf_LikeMat_s = 0
countf_Quali_s = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
Stabsel = c()
for (q in 1:1000) 
{
  
  countf_Lasso_i=0
  countf_TransDiff_l_i =0
  countf_LikeSch_l_i =0
  countf_TPoss_l_i = 0
  countf_TNeg_l_i = 0
  countf_MalFem_l_i = 0
  countf_School_l_i = 0
  countf_LikeMat_l_i = 0
  countf_Quali_l_i = 0
 
  stabcount = c()
  
  
  L =c()
  mysample<-gui_sim[sample(1:nrow(gui_sim),500,replace=T),]
  attach(mysample)
  Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
  Eta = matrix(c(predict(fit, newdata=mysample))) + rnorm(500,0,4)
  library(LaplacesDemon)
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
  mysample1=data.frame(mysample,Ynew)
  library(glmnet)
  preds=model.matrix(~ trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50 + bhpc19a + bpce33a + bpsd40a + bdsd57a + bhpc55 + bhpc61a)
  cv=cv.glmnet(preds,Ynew,family="binomial",alpha=1)
  L = matrix(c(L,cv$lambda[cv$lambda >= cv$lambda.min & cv$lambda <= cv$lambda.1se]))
  Luniq = matrix(unique(L))
  detach(mysample)
  length(Luniq)

  for (i in 1:500) {
    Lasso_Coeff_rd = c()
    sample_k<-mysample[sample(1:nrow(mysample),250,replace=F),]
    attach(sample_k)
    All_Predictors = matrix(c(names(gui_sim[,-1])))
    Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
    Eta = matrix(c(predict(fit, newdata=sample_k)))+ rnorm(250,0,4)
    library(LaplacesDemon)
    Etap=invlogit(Eta)
    Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
    sample_k_1=data.frame(sample_k,Ynew)
    library(glmnet)
    preds=model.matrix(~ trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50 + bhpc19a )
    lasso_fit=glmnet(preds,Ynew,family="binomial",alpha=1,lambda=Luniq[length(Luniq)])
    Lasso_Coeff<- if(is.null(names(which(abs(lasso_fit$beta[,1])>0)))) matrix(,) else matrix(c(names(which(abs(lasso_fit$beta[,1])>0))), byrow = F)
    for (j in 1:length(All_Predictors)) 
      Lasso_Coeff_rd = c(Lasso_Coeff_rd, regmatches(Lasso_Coeff, regexpr(All_Predictors[j], Lasso_Coeff)))
    Lasso_Coeff_rdn = matrix(unique(Lasso_Coeff_rd))
    Lasso_Coeff_rdn_l = matrix(c(Lasso_Coeff_rdn,Luniq[length(Luniq)]))
    countf_Lasso_i = countf_Lasso_i + as.numeric(identical(sort(Lasso_Coeff_rdn),sort(Beta_Significant)))
    countf_TransDiff_l_i = countf_TransDiff_l_i + as.numeric('trans_diff_qrt' %in% Lasso_Coeff_rdn)
    countf_LikeSch_l_i = countf_LikeSch_l_i + as.numeric('acce14' %in% Lasso_Coeff_rdn)
    countf_TPoss_l_i = countf_TPoss_l_i + as.numeric('tposscale' %in% Lasso_Coeff_rdn)
    countf_TNeg_l_i = countf_TNeg_l_i + as.numeric('tnegscale' %in% Lasso_Coeff_rdn)
    countf_MalFem_l_i = countf_MalFem_l_i + as.numeric('bphc02a' %in% Lasso_Coeff_rdn)
    countf_School_l_i = countf_School_l_i + as.numeric('bcce37' %in% Lasso_Coeff_rdn)
    countf_LikeMat_l_i = countf_LikeMat_l_i + as.numeric('acce16a' %in% Lasso_Coeff_rdn)
    countf_Quali_l_i = countf_Quali_l_i + as.numeric('bcce50' %in% Lasso_Coeff_rdn)
    detach(sample_k)
  }
  stabcount = rbind(stabcount,cbind(countf_TransDiff_l_i,countf_LikeSch_l_i,countf_TPoss_l_i,countf_TNeg_l_i,countf_MalFem_l_i,countf_School_l_i, countf_LikeMat_l_i,countf_Quali_l_i))
  rownames(stabcount) = c(paste("Lambda", Luniq[length(Luniq)]))
  colnames(stabcount) = c("trans_diff_qrt","acce14","tposscale","tnegscale","bphc02a","bcce37","acce16a","bcce50")
  stabfinal = rbind(stabfinal,stabcount)
  Stabsel = matrix(colnames(stabcount)[which(stabfinal[q,]>=350)])
  count_0 = count_0 + as.numeric(length(intersect(Stabsel,Beta_Significant))==0)
  count_1 = count_1 + as.numeric(length(intersect(Stabsel,Beta_Significant))==1)
  count_2 = count_2 + as.numeric(length(intersect(Stabsel,Beta_Significant))==2)
  count_3 = count_3 + as.numeric(length(intersect(Stabsel,Beta_Significant))==3)
  count_4 = count_4 + as.numeric(length(intersect(Stabsel,Beta_Significant))==4)
  count_5 = count_5 + as.numeric(length(intersect(Stabsel,Beta_Significant))==5)
  count_6 = count_6 + as.numeric(length(intersect(Stabsel,Beta_Significant))==6)
  count_7 = count_7 + as.numeric(length(intersect(Stabsel,Beta_Significant))==7)
  count_8 = count_8 + as.numeric(length(intersect(Stabsel,Beta_Significant))==8)
  countf_TransDiff_s = countf_TransDiff_s + as.numeric('trans_diff_qrt' %in% Stabsel)
  countf_LikeSch_s = countf_LikeSch_s + as.numeric('acce14' %in% Stabsel)
  countf_TPoss_s = countf_TPoss_s + as.numeric('tposscale' %in% Stabsel)
  countf_TNeg_s = countf_TNeg_s + as.numeric('tnegscale' %in% Stabsel)
  countf_MalFem_s = countf_MalFem_s + as.numeric('bphc02a' %in% Stabsel)
  countf_School_s = countf_School_s + as.numeric('bcce37' %in% Stabsel)
  countf_LikeMat_s = countf_LikeMat_s + as.numeric('acce16a' %in% Stabsel)
  countf_Quali_s = countf_Quali_s + as.numeric('bcce50' %in% Stabsel)
  f = f+1
  
}


Counts_l_i = matrix(c(count_0,count_1,count_2,count_3,count_4,count_5,count_6,count_7,count_8))
CountH_l_i = matrix(c(0,1,2,3,4,5,6,7,8))
y_l_i = data.frame(CountH_l_i,Counts_l_i)
Plot_l_i=barplot(y_l_i$Counts_l_i, names.arg=y_l_i$CountH_l_i, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

#500 setting

countf_Lasso_s # Countf_Lasso_i gives value of 995 in 250 runs i.e the Lasso technique only captures the right significant predictors 0 time in 1000 runs
countf_TransDiff_s # 199 times trans_diff_qrt was chosen
countf_LikeSch_s # 95 times acce14 was chosen 
countf_TPoss_s # 236 times tposscale was chosen
countf_TNeg_s # 233 times tnegscale was chosen
countf_MalFem_s # 3 times bphc02a was chosen
countf_School_s # 3 times bcce37 was chosen
countf_LikeMat_s # 182 times acce16a was chosen
countf_Quali_s # 225 times bcce50 was chosen


count_0 # 0 s.predictors  chosen 745 times
count_1 # 1 s.predictors  chosen 23 times
count_2 # 2 s.predictors  chosen 15 times
count_3 # 3 s.predictors  chosen 23 times
count_4 # 4 s.predictors  chosen 17 times
count_5 # 5 s.predictors  chosen 82 times
count_6 # 6 s.predictors  chosen 92 times
count_7 # 7 s.predictors  chosen 0 times
count_8 # 8 s.predictors  chosen 3 times

#5000 setting

countf_Lasso_s # Countf_Lasso_i gives value of 995 in 100 runs i.e the Lasso technique only captures the right significant predictors 0 time in 1000 runs
countf_TransDiff_s # 100 times trans_diff_qrt was chosen
countf_LikeSch_s # 951 times acce14 was chosen 
countf_TPoss_s # 997 times tposscale was chosen
countf_TNeg_s # 1000 times tnegscale was chosen
countf_MalFem_s # 899 times bphc02a was chosen
countf_School_s # 598 times bcce37 was chosen
countf_LikeMat_s # 1000 times acce16a was chosen
countf_Quali_s # 997 times bcce50 was chosen


count_0 # 0 s.predictors  chosen 0 times
count_1 # 1 s.predictors  chosen 0 times
count_2 # 2 s.predictors  chosen 3 times
count_3 # 3 s.predictors  chosen 0 times
count_4 # 4 s.predictors  chosen 11 times
count_5 # 5 s.predictors  chosen 35 times
count_6 # 6 s.predictors  chosen 52 times
count_7 # 7 s.predictors  chosen 301 times
count_8 # 8 s.predictors  chosen 598 times

#STABILITY SELECTION# with significant



#                                               **VARIABLE NAME**                                               **RESPONSES**     
# Insignificant Predictors chosen :1.	Bphc19a----How adequate No of teachers                             (Fair, Good, Poor)
#                                  2.	Bcce50-----Highest qualification                                   (Certificate or Diploma (including plc., apprenticeship), Degree or higher degree, Junior Cert, Leaving Cert)
#                                  3.	Bpsd40a----Primary caretaker’s highest qualification               (Hi Sec/TechVoc/UppSec+Tech/Voc,Lower Sec, Non Degree, Postgrad, Primary) )
#                                  4.	Bdsd57a----HSD class (family’s social)                             (Non/skilled man, Not active, Prof/manager, Semi-U.S. man)
#                                  5.	Bhpc55-----Religious ethos of school                               (Non RC, Roman Catholic)
#                                  6.	Bhpc61a----Induction Day                                           (No, Yes)

f =1
detach(gui_sim)
set.seed(904849011)
stabfinal = c()
countf_TransDiff_s =0
countf_LikeSch_s =0
countf_TPoss_s = 0
countf_TNeg_s = 0
countf_MalFem_s = 0
countf_School_s = 0
countf_LikeMat_s = 0
countf_Quali_s = 0
count_0 = 0
count_1 = 0
count_2 = 0
count_3 = 0
count_4 = 0
count_5 = 0
count_6 = 0
count_7 = 0
count_8 = 0
Stabsel = c()
for (q in 1:1000) 
{
  
  countf_Lasso_i=0
  countf_TransDiff_l_i =0
  countf_LikeSch_l_i =0
  countf_TPoss_l_i = 0
  countf_TNeg_l_i = 0
  countf_MalFem_l_i = 0
  countf_School_l_i = 0
  countf_LikeMat_l_i = 0
  countf_Quali_l_i = 0
  
  
  
  stabcount = c()
  
  
  L =c()
  mysample<-gui_sim[sample(1:nrow(gui_sim),500,replace=T),]
  attach(mysample)
  Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
  Eta = matrix(c(predict(fit, newdata=mysample))) 
  library(LaplacesDemon)
  Etap=invlogit(Eta)
  Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
  mysample1=data.frame(mysample,Ynew)
  library(glmnet)
  preds=model.matrix(~ trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50 + bhpc19a + bpce33a + bpsd40a + bdsd57a + bhpc55 + bhpc61a)
  cv=cv.glmnet(preds,Ynew,family="binomial",alpha=1)
  L = matrix(c(L,cv$lambda[cv$lambda >= cv$lambda.min & cv$lambda <= cv$lambda.1se]))
  Luniq = matrix(unique(L))
  detach(mysample)
  length(Luniq)
  
  for (i in 1:500) {
    Lasso_Coeff_rd = c()
    sample_k<-mysample[sample(1:nrow(mysample),250,replace=F),]
    attach(sample_k)
    All_Predictors = matrix(c(names(gui_sim[,-1])))
    Beta_Significant=matrix(c('trans_diff_qrt','acce14','tposscale','tnegscale','bphc02a','bcce37','acce16a','bcce50'),byrow = T)
    Eta = matrix(c(predict(fit, newdata=sample_k)))
    library(LaplacesDemon)
    Etap=invlogit(Eta)
    Ynew=matrix(sapply(Etap,rbinom,n=1,size=1),nrow=nrow(Etap))
    sample_k_1=data.frame(sample_k,Ynew)
    library(glmnet)
    preds=model.matrix(~ trans_diff_qrt + acce14 + tposscale + tnegscale + bphc02a + bcce37 + acce16a + bcce50 + bhpc19a )
    lasso_fit=glmnet(preds,Ynew,family="binomial",alpha=1,lambda=Luniq[length(Luniq)])
    Lasso_Coeff<- if(is.null(names(which(abs(lasso_fit$beta[,1])>0)))) matrix(,) else matrix(c(names(which(abs(lasso_fit$beta[,1])>0))), byrow = F)
    for (j in 1:length(All_Predictors)) 
      Lasso_Coeff_rd = c(Lasso_Coeff_rd, regmatches(Lasso_Coeff, regexpr(All_Predictors[j], Lasso_Coeff)))
    Lasso_Coeff_rdn = matrix(unique(Lasso_Coeff_rd))
    Lasso_Coeff_rdn_l = matrix(c(Lasso_Coeff_rdn,Luniq[length(Luniq)]))
    countf_Lasso_i = countf_Lasso_i + as.numeric(identical(sort(Lasso_Coeff_rdn),sort(Beta_Significant)))
    countf_TransDiff_l_i = countf_TransDiff_l_i + as.numeric('trans_diff_qrt' %in% Lasso_Coeff_rdn)
    countf_LikeSch_l_i = countf_LikeSch_l_i + as.numeric('acce14' %in% Lasso_Coeff_rdn)
    countf_TPoss_l_i = countf_TPoss_l_i + as.numeric('tposscale' %in% Lasso_Coeff_rdn)
    countf_TNeg_l_i = countf_TNeg_l_i + as.numeric('tnegscale' %in% Lasso_Coeff_rdn)
    countf_MalFem_l_i = countf_MalFem_l_i + as.numeric('bphc02a' %in% Lasso_Coeff_rdn)
    countf_School_l_i = countf_School_l_i + as.numeric('bcce37' %in% Lasso_Coeff_rdn)
    countf_LikeMat_l_i = countf_LikeMat_l_i + as.numeric('acce16a' %in% Lasso_Coeff_rdn)
    countf_Quali_l_i = countf_Quali_l_i + as.numeric('bcce50' %in% Lasso_Coeff_rdn)
    detach(sample_k)
  }
  stabcount = rbind(stabcount,cbind(countf_TransDiff_l_i,countf_LikeSch_l_i,countf_TPoss_l_i,countf_TNeg_l_i,countf_MalFem_l_i,countf_School_l_i, countf_LikeMat_l_i,countf_Quali_l_i))
  rownames(stabcount) = c(paste("Lambda", Luniq[length(Luniq)]))
  colnames(stabcount) = c("trans_diff_qrt","acce14","tposscale","tnegscale","bphc02a","bcce37","acce16a","bcce50")
  stabfinal = rbind(stabfinal,stabcount)
  Stabsel = matrix(colnames(stabcount)[which(stabfinal[q,]>=350)])
  count_0 = count_0 + as.numeric(length(intersect(Stabsel,Beta_Significant))==0)
  count_1 = count_1 + as.numeric(length(intersect(Stabsel,Beta_Significant))==1)
  count_2 = count_2 + as.numeric(length(intersect(Stabsel,Beta_Significant))==2)
  count_3 = count_3 + as.numeric(length(intersect(Stabsel,Beta_Significant))==3)
  count_4 = count_4 + as.numeric(length(intersect(Stabsel,Beta_Significant))==4)
  count_5 = count_5 + as.numeric(length(intersect(Stabsel,Beta_Significant))==5)
  count_6 = count_6 + as.numeric(length(intersect(Stabsel,Beta_Significant))==6)
  count_7 = count_7 + as.numeric(length(intersect(Stabsel,Beta_Significant))==7)
  count_8 = count_8 + as.numeric(length(intersect(Stabsel,Beta_Significant))==8)
  countf_TransDiff_s = countf_TransDiff_s + as.numeric('trans_diff_qrt' %in% Stabsel)
  countf_LikeSch_s = countf_LikeSch_s + as.numeric('acce14' %in% Stabsel)
  countf_TPoss_s = countf_TPoss_s + as.numeric('tposscale' %in% Stabsel)
  countf_TNeg_s = countf_TNeg_s + as.numeric('tnegscale' %in% Stabsel)
  countf_MalFem_s = countf_MalFem_s + as.numeric('bphc02a' %in% Stabsel)
  countf_School_s = countf_School_s + as.numeric('bcce37' %in% Stabsel)
  countf_LikeMat_s = countf_LikeMat_s + as.numeric('acce16a' %in% Stabsel)
  countf_Quali_s = countf_Quali_s + as.numeric('bcce50' %in% Stabsel)
  f = f+1
  
}


Counts_l_i = matrix(c(count_0,count_1,count_2,count_3,count_4,count_5,count_6,count_7,count_8))
CountH_l_i = matrix(c(0,1,2,3,4,5,6,7,8))
y_l_i = data.frame(CountH_l_i,Counts_l_i)
Plot_l_i=barplot(y_l_i$Counts_l_i, names.arg=y_l_i$CountH_l_i, main = "Frequency of Different No of Predictors chosen", xlab = "No of Predictors", ylab = "Frequency") 

#500 setting

countf_Lasso_s # Countf_Lasso_i gives value of 995 in 250 runs i.e the Lasso technique only captures the right significant predictors 0 time in 1000 runs
countf_TransDiff_s # 716 times trans_diff_qrt was chosen
countf_LikeSch_s # 577 times acce14 was chosen 
countf_TPoss_s # 801 times tposscale was chosen
countf_TNeg_s # 887 times tnegscale was chosen
countf_MalFem_s # 174 times bphc02a was chosen
countf_School_s # 76 times bcce37 was chosen
countf_LikeMat_s # 939 times acce16a was chosen
countf_Quali_s # 810 times bcce50 was chosen


count_0 # 0 s.predictors  chosen 60 times
count_1 # 1 s.predictors  chosen 54 times
count_2 # 2 s.predictors  chosen 63 times
count_3 # 3 s.predictors  chosen 35 times
count_4 # 4 s.predictors  chosen 72 times
count_5 # 5 s.predictors  chosen 139 times
count_6 # 6 s.predictors  chosen 401 times
count_7 # 7 s.predictors  chosen 102 times
count_8 # 8 s.predictors  chosen 74 times

#5000 setting

countf_Lasso_s # Countf_Lasso_i gives value of 995 in 100 runs i.e the Lasso technique only captures the right significant predictors 0 time in 1000 runs
countf_TransDiff_s # 1000 times trans_diff_qrt was chosen
countf_LikeSch_s # 1000 times acce14 was chosen 
countf_TPoss_s # 1000 times tposscale was chosen
countf_TNeg_s # 1000 times tnegscale was chosen
countf_MalFem_s # 1000 times bphc02a was chosen
countf_School_s # 1000 times bcce37 was chosen
countf_LikeMat_s # 1000 times acce16a was chosen
countf_Quali_s # 1000 times bcce50 was chosen


count_0 # 0 s.predictors  chosen 0 times
count_1 # 1 s.predictors  chosen 0 times
count_2 # 2 s.predictors  chosen 0 times
count_3 # 3 s.predictors  chosen 0 times
count_4 # 4 s.predictors  chosen 0 times
count_5 # 5 s.predictors  chosen 0 times
count_6 # 6 s.predictors  chosen 0 times
count_7 # 7 s.predictors  chosen 0 times
count_8 # 8 s.predictors  chosen 1000 times

#************************************************************************************************************************************************************************************************************************************

#LOGISTIC REGRESSION OF SELECTION METHODS#

countf_AIC_i # Countf_AIC_i gives value of 222 in 1000 runs i.e the AIC technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_i # 806 times trans_diff_qrt was chosen
countf_LikeSch_i # 788 times acce14 was chosen 
countf_TPoss_i # 999 times tposscale was chosen
countf_TNeg_i # 1000 times tnegscale was chosen
countf_MalFem_i # 1000 times bphc02a was chosen
countf_School_i # 973 times bcce37 was chosen
countf_LikeMat_i # 1000 times acce16a was chosen
countf_SchoolPT_i # 647 times bpce33a was chosen

# Insignificant predictors chosen
countf_Adeq_No_i # 84 times bhpc19a was chosen
countf_Quali_i # 118 times bcce50 was chosen
countf_CareQ_i # 87 times bpsd40a was chosen
countf_HSD_i # 112 times bdsd57a was chosen
countf_RelE_i # 121 times bhpc55 was chosen
countf_Ind_i # 117 times bhpc61a was chosen


countf_Lasso_i # Countf_Lasso_i gives value of 995 in 1000 runs i.e the Lasso technique only captures the right significant predictors 0 time in 1000 runs
countf_TransDiff_l_i # 1000 times trans_diff_qrt was chosen
countf_LikeSch_l_i # 1000 times acce14 was chosen 
countf_TPoss_l_i # 1000 times tposscale was chosen
countf_TNeg_l_i # 1000 times tnegscale was chosen
countf_MalFem_l_i # 999 times bphc02a was chosen
countf_School_l_i # 1000 times bcce37 was chosen
countf_LikeMat_l_i # 1000 times acce16a was chosen
countf_SchoolPT_l_i # 995 times bpce33a was chosen

# Insignificant predictors chosen
countf_Adeq_No_l_i # 0 times bhpc19a was chosen
countf_Quali_l_i # 0 times bcce50 was chosen
countf_CareQ_l_i # 0 times bpsd40a was chosen
countf_HSD_l_i # 0 times bdsd57a was chosen
countf_RelE_l_i # 0 times bhpc55 was chosen
countf_Ind_l_i # 0 times bhpc61a was chosen

countf_LRT_i_d # Countf_LRT_i_d gives value of 0 in 1000 runs i.e the LRT technique only captures the right significant predictors 0 times in 1000 runs
countf_TransDiff_i_d # 1000 times trans_diff_qrt was chosen
countf_LikeSch_i_d # 1000 times acce14 was chosen 
countf_TPoss_i_d # 1000 times tposscale was chosen
countf_TNeg_i_d # 999 times tnegscale was chosen
countf_MalFem_i_d # 345 times bphc02a was chosen
countf_School_i_d # 1000 times bcce37 was chosen
countf_LikeMat_i_d # 1000 times acce16a was chosen
countf_SchoolPT_i_d # 656 times bpce33a was chosen

# Insignificant predictors chosen

countf_Adeq_No_i_d # 0 times bpce33a was chosen
countf_Quali_i_d # 0 times bpce33a was chosen
countf_CareQ_i_d # 0 times bpce33a was chosen
countf_HSD_i_d # 0 times bpce33a was chosen
countf_RelE_i_d # 0 times bpce33a was chosen
countf_Ind_i_d # 0 times bpce33a was chosen

countf_Lasso_i # Stability Countf_Lasso_i gives value of 995 in 1000 runs i.e the Lasso technique only captures the right significant predictors 0 time in 1000 runs
countf_TransDiff_l_i # 500 times trans_diff_qrt was chosen
countf_LikeSch_l_i # 500 times acce14 was chosen 
countf_TPoss_l_i # 500 times tposscale was chosen
countf_TNeg_l_i # 500 times tnegscale was chosen
countf_MalFem_l_i # 500 times bphc02a was chosen
countf_School_l_i # 500 times bcce37 was chosen
countf_LikeMat_l_i # 500 times acce16a was chosen
countf_SchoolPT_l_i # 500 times bpce33a was chosen

# Insignificant predictors chosen
countf_Adeq_No_l_i # 0 times bhpc19a was chosen
countf_Quali_l_i # 0 times bcce50 was chosen
countf_CareQ_l_i # 0 times bpsd40a was chosen
countf_HSD_l_i # 0 times bdsd57a was chosen
countf_RelE_l_i # 0 times bhpc55 was chosen
countf_Ind_l_i # 0 times bhpc61a was chosen

# % TRUE PREDICTORS- Model 2

Res = cbind(0.76,0.765,0.998,1,1,0.947,1,0.809,0.666,0.763,0.957,1,0.774,0.432,1,0.982,0.636,0.577,0.998,1,0.86,1,0.676,1,1,1,1,1,1,1,1,1)
Xval = cbind("AIC","AIC","AIC","AIC","AIC","AIC","AIC","AIC","LASSO","LASSO","LASSO","LASSO","LASSO","LASSO","LASSO","LASSO","LRT","LRT","LRT","LRT","LRT","LRT","LRT","LRT","STAB","STAB","STAB","STAB","STAB","STAB","STAB","STAB")

list = matrix(c(rep(1,760),rep(0,240),rep(1,666),rep(0,334),rep(1,636),rep(0,364),rep(1,1000),rep(0,0),rep(1,765),rep(0,235),rep(1,763),rep(0,237),rep(1,577),rep(0,423),rep(1,1000),rep(0,0),rep(1,998),rep(0,2),rep(1,957),rep(0,43),rep(1,998),rep(0,2),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,774),rep(0,226),rep(1,996),rep(0,4),rep(1,1000),rep(0,0),rep(1,947),rep(0,53),rep(1,432),rep(0,568),rep(1,860),rep(0,140),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,809),rep(0,191),rep(1,982),rep(0,18),rep(1,676),rep(0,324),rep(1,1000),rep(0,0),rep(1,81),rep(0,919),rep(1,91),rep(0,909),rep(1,52),rep(0,948),rep(1,1000),rep(0,0),rep(1,172),rep(0,828),rep(1,64),rep(0,936),rep(1,65),rep(0,935),rep(1,904),rep(0,96),rep(1,95),rep(0,905),rep(1,115),rep(0,885),rep(1,55),rep(0,945),rep(1,1000),rep(0,0),rep(1,102),rep(0,898),rep(1,98),rep(0,902),rep(1,56),rep(0,944),rep(1,1000),rep(0,0),rep(1,129),rep(0,871),rep(1,29),rep(0,971),rep(1,49),rep(0,951),rep(1,964),rep(0,36),rep(1,121),rep(0,879),rep(1,57),rep(0,943),rep(1,65),rep(0,935),rep(1,988),rep(0,12),rep(1,255),rep(0,745),rep(1,42),rep(0,958),rep(1,144),rep(0,856),rep(1,707),rep(0,293),rep(1,234),rep(0,766),rep(1,55),rep(0,945),rep(1,125),rep(0,875),rep(1,561),rep(0,439),rep(1,435),rep(0,565),rep(1,57),rep(0,943),rep(1,261),rep(0,739),rep(1,794),rep(0,206),rep(1,811),rep(0,189),rep(1,137),rep(0,863),rep(1,686),rep(0,314),rep(1,882),rep(0,118),rep(1,462),rep(0,538),rep(1,23),rep(0,977),rep(1,327),rep(0,673),rep(1,156),rep(0,844),rep(1,303),rep(0,697),rep(1,11),rep(0,989),rep(1,174),rep(0,826),rep(1,58),rep(0,942),rep(1,878),rep(0,122),rep(1,249),rep(0,751),rep(1,693),rep(0,307),rep(1,936),rep(0,64),rep(1,266),rep(0,734),rep(1,103),rep(0,897),rep(1,136),rep(0,864),rep(1,801),rep(0,199),rep(1,88),rep(0,912),rep(1,15),rep(0,985),rep(1,71),rep(0,929),rep(1,719),rep(0,281),rep(1,157),rep(0,843),rep(1,5),rep(0,995),rep(1,65),rep(0,935),rep(1,31),rep(0,969),rep(1,107),rep(0,893),rep(1,21),rep(0,979),rep(1,71),rep(0,929),rep(1,744),rep(0,256),rep(1,140),rep(0,860),rep(1,12),rep(0,988),rep(1,80),rep(0,920),rep(1,633),rep(0,367),rep(1,140),rep(0,860),rep(1,10),rep(0,990),rep(1,53),rep(0,947),rep(1,87),rep(0,913),rep(1,118),rep(0,882),rep(1,7),rep(0,993),rep(1,68),rep(0,932),rep(1,169),rep(0,831)))
Var = matrix(c(rep("TransDiff",4000),rep("LikeSch",4000),rep("TPoss",4000),rep("TNeg",4000),rep("MalFem",4000),rep("School",4000),rep("LikeMat",4000),rep("Quali",4000),rep("Adeq_No", 4000),rep("School_PT", 4000),rep("CareQ", 4000),rep("HSD", 4000),rep("RelE", 4000),rep("Ind", 4000),rep("TransDiff",4000),rep("LikeSch",4000),rep("TPoss",4000),rep("TNeg",4000),rep("MalFem",4000),rep("School",4000),rep("LikeMat",4000),rep("Quali",4000),rep("Adeq_No", 4000),rep("School_PT", 4000),rep("CareQ", 4000),rep("HSD", 4000),rep("RelE", 4000),rep("Ind", 4000)))
Meth = matrix(c(rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000)))
Samp = matrix(c(rep("5000", 56000),rep("500",56000)))
LogTabu = data.frame(Response = list, Variable = Var, Method = Meth, Samples = Samp)
colnames(LogTabu)=c("Response","Variable","Method","Samples")              
detach(gui_sim)
attach(LogTabu)
detach(LogTabu)
Logfit = glm(factor(Response) ~ factor(Variable) + factor(Method) + factor(Samples), family = binomial, data =LogTabu) 
summary(Logfit)
library(LaplacesDemon)
detach(LogTabu)
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 -0.74984 = -2.56843) = 0.07119806
# The probability CareQ is selected by AIC is Inv logit (-1.97588 +0.15729  = -1.81859) = 0.1396031
# The probability CareQ is selected by STAB is Inv logit (-1.97588 +0.15729 +0.30082 = -3.2907) = 0.0358916
# The probability CareQ is selected by LRT is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286

Etap=invlogit(-1.81859)


# % TRUE PREDICTORS- Model 1

list1 = matrix(c(rep(1,765),rep(0,235),rep(1,999),rep(0,1),rep(1,626),rep(0,374),rep(1,1000),rep(0,0),rep(1,765),rep(0,235),rep(1,1000),rep(0,0),rep(1,588),rep(0,412),rep(1,1000),rep(0,0),rep(1,998),rep(0,2),rep(1,1000),rep(0,0),rep(1,998),rep(0,2),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,996),rep(0,4),rep(1,1000),rep(0,0),rep(1,958),rep(0,42),rep(1,998),rep(0,2),rep(1,868),rep(0,132),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,1000),rep(0,0),rep(1,815),rep(0,185),rep(1,1000),rep(0,0),rep(1,692),rep(0,308),rep(1,1000),rep(0,0),rep(1,249),rep(0,751),rep(1,999),rep(0,1),rep(1,136),rep(0,864),rep(1,716),rep(0,284),rep(1,236),rep(0,764),rep(1,998),rep(0,2),rep(1,110),rep(0,890),rep(1,577),rep(0,423),rep(1,426),rep(0,574),rep(1,999),rep(0,1),rep(1,262),rep(0,738),rep(1,801),rep(0,199),rep(1,820),rep(0,180),rep(1,999),rep(0,1),rep(1,689),rep(0,311),rep(1,887),rep(0,113),rep(1,454),rep(0,546),rep(1,998),rep(0,2),rep(1,318),rep(0,682),rep(1,174),rep(0,826),rep(1,302),rep(0,698),rep(1,880),rep(0,120),rep(1,173),rep(0,827),rep(1,76),rep(0,924),rep(1,879),rep(0,121),rep(1,995),rep(0,5),rep(1,692),rep(0,308),rep(1,939),rep(0,61),rep(1,275),rep(0,725),rep(1,996),rep(0,4),rep(1,122),rep(0,878),rep(1,810),rep(0,190)))
Var1 = matrix(c(rep("TransDiff",4000),rep("LikeSch",4000),rep("TPoss",4000),rep("TNeg",4000),rep("MalFem",4000),rep("School",4000),rep("LikeMat",4000),rep("Quali",4000),rep("TransDiff",4000),rep("LikeSch",4000),rep("TPoss",4000),rep("TNeg",4000),rep("MalFem",4000),rep("School",4000),rep("LikeMat",4000),rep("Quali",4000)))
Meth1 = matrix(c(rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000)))
Samp1 = matrix(c(rep("5000", 32000),rep("500",32000)))
LogTabu1 = data.frame(Response1 = list1, Variable1 = Var1, Method1 = Meth1, Samples1 = Samp1)
colnames(LogTabu1)=c("Response1","Variable1","Method1","Samples1")              
detach(gui_sim)
attach(LogTabu1)
Logfit1 = glm(factor(Response1) ~  factor(Variable1) + factor(Method1) + factor(Samples1), family = binomial, data =LogTabu1) 
summary(Logfit1)
library(LaplacesDemon)
detach(LogTabu1)
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 -0.74984 = -2.56843) = 0.07119806
# The probability CareQ is selected by AIC is Inv logit (-1.97588 +0.15729  = -1.81859) = 0.1396031
# The probability CareQ is selected by STAB is Inv logit (-1.97588 +0.15729 +0.30082 = -3.2907) = 0.0358916
# The probability CareQ is selected by LRT is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286

Etap=invlogit(-1.81859)


# % TRUE PREDICTORS- Model 3

list3 = matrix(c(rep(1,316),rep(0,684),rep(1,147),rep(0,853),rep(1,182),rep(0,818),rep(1,1000),rep(0,0),rep(1,295),rep(0,705),rep(1,153),rep(0,847),rep(1,138),rep(0,862),rep(1,980),rep(0,20),rep(1,748),rep(0,252),rep(1,299),rep(0,701),rep(1,497),rep(0,503),rep(1,1000),rep(0,0),rep(1,978),rep(0,22),rep(1,533),rep(0,467),rep(1,953),rep(0,47),rep(1,1000),rep(0,0),rep(1,748),rep(0,252),rep(1,124),rep(0,876),rep(1,622),rep(0,378),rep(1,920),rep(0,80),rep(1,537),rep(0,463),rep(1,53),rep(0,947),rep(1,334),rep(0,666),rep(1,670),rep(0,330),rep(1,993),rep(0,7),rep(1,593),rep(0,407),rep(1,967),rep(0,33),rep(1,1000),rep(0,0),rep(1,355),rep(0,645),rep(1,378),rep(0,622),rep(1,184),rep(0,816),rep(1,1000),rep(0,0),rep(1,140),rep(0,860),rep(1,38),rep(0,962),rep(1,59),rep(0,941),rep(1,199),rep(0,801),rep(1,160),rep(0,840),rep(1,33),rep(0,967),rep(1,62),rep(0,938),rep(1,95),rep(0,905),rep(1,217),rep(0,783),rep(1,38),rep(0,962),rep(1,97),rep(0,903),rep(1,236),rep(0,764),rep(1,300),rep(0,700),rep(1,53),rep(0,947),rep(1,167),rep(0,833),rep(1,233),rep(0,767),rep(1,228),rep(0,772),rep(1,17),rep(0,983),rep(1,118),rep(0,882),rep(1,3),rep(0,997),rep(1,176),rep(0,824),rep(1,7),rep(0,993),rep(1,78),rep(0,922),rep(1,3),rep(0,997),rep(1,391),rep(0,609),rep(1,64),rep(0,936),rep(1,192),rep(0,808),rep(1,182),rep(0,818),rep(1,166),rep(0,834),rep(1,46),rep(0,954),rep(1,68),rep(0,932),rep(1,225),rep(0,775)))
Var3 = matrix(c(rep("TransDiff",4000),rep("LikeSch",4000),rep("TPoss",4000),rep("TNeg",4000),rep("MalFem",4000),rep("School",4000),rep("LikeMat",4000),rep("Quali",4000),rep("TransDiff",4000),rep("LikeSch",4000),rep("TPoss",4000),rep("TNeg",4000),rep("MalFem",4000),rep("School",4000),rep("LikeMat",4000),rep("Quali",4000)))
Meth3 = matrix(c(rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000),rep("AIC",1000),rep("Lasso",1000),rep("LRT",1000),rep("Stab",1000)))
Samp3 = matrix(c(rep("5000", 32000),rep("500",32000)))

LogTabu3 = data.frame(Response3 = list3, Variable3 = Var3, Method3 = Meth3, Samples3 = Samp3)
colnames(LogTabu3)=c("Response3","Variable3","Method3","Samples3")              
detach(gui_sim)
attach(LogTabu3)
Logfit3 = glm(factor(Response3) ~  factor(Variable3) + factor(Method3) + factor(Samples3), family = binomial, data =LogTabu3) 
summary(Logfit3)
library(LaplacesDemon)
detach(LogTabu3)
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 -0.74984 = -2.56843) = 0.07119806
# The probability CareQ is selected by AIC is Inv logit (-1.97588 +0.15729  = -1.81859) = 0.1396031
# The probability CareQ is selected by STAB is Inv logit (-1.97588 +0.15729 +0.30082 = -3.2907) = 0.0358916
# The probability CareQ is selected by LRT is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286
# The probability CareQ is selected by Lasso is Inv logit (-1.97588 +0.15729 +0.28862 = -3.30287) = 0.03547286

Etap=invlogit(-1.81859)

#KRUSKAL WALLIS TEST#

# 5000 setting

NOP = matrix(c(rep(8,249),rep(7,476),rep(6,224),rep(5,49),rep(4,2),rep(8,414),rep(7,456),rep(6,120),rep(5,10),rep(8,262),rep(7,313),rep(6,234),rep(5,126),rep(4,58),rep(3,7),rep(8,1000)))
Met = matrix(c(rep("LRT", 1000), rep("AIC", 1000), rep("Lasso", 1000), rep("Stab", 1000)))
KWTab = data.frame(NumTrue = NOP, Method = Met)
colnames(KWTab) = c("NumTrue", "Method")
attach(KWTab)
KWfit = kruskal.test(NumTrue~factor(Method))
KWfit

# 500 setting---No noise

NOP1 = matrix(c(rep(8,0),rep(7,0),rep(6,7),rep(5,32),rep(4,139),rep(3,312),rep(2,325),rep(1,158),rep(0,27),rep(8,0),rep(7,8),rep(6,53),rep(5,165),rep(4,297),rep(3,320),rep(2,138),rep(1,18),rep(0,1),rep(8,880),rep(7,115),rep(6,1),rep(5,2),rep(4,0),rep(3,1),rep(2,0),rep(1,0),rep(0,1),rep(8,74),rep(7,102),rep(6,401),rep(5,139),rep(4,72),rep(3,35),rep(2,63),rep(1,54),rep(0,60)))
Met1 = matrix(c(rep("LRT", 1000), rep("Stepwise", 1000), rep("Lasso", 1000), rep("Stability", 1000)))
KWTab1 = data.frame(Number_of_True_Predictors_Selected = NOP1, Method = Met1)
colnames(KWTab1) = c("Number_of_True_Predictors_Selected", "Method")
attach(KWTab1)
boxplot(Number_of_True_Predictors_Selected~Method)
KWfit1 = kruskal.test(Number_of_True_Predictors_Selected~factor(Method))
KWfit1
WiFit1 = pairwise.wilcox.test(Number_of_True_Predictors_Selected,Method,p.adj='holm',exact=F)
WiFit1
detach(KWTab1)

# 500 setting--- noise predictors

NOP2 = matrix(c(rep(8,2),rep(7,6),rep(6,24),rep(5,90),rep(4,180),rep(3,322),rep(2,251),rep(1,106),rep(0,19),rep(8,0),rep(7,7),rep(6,48),rep(5,179),rep(4,300),rep(3,305),rep(2,138),rep(1,21),rep(0,2),rep(8,1),rep(7,2),rep(6,5),rep(5,18),rep(4,23),rep(3,31),rep(2,53),rep(1,244),rep(0,623),rep(8,58),rep(7,98),rep(6,405),rep(5,144),rep(4,74),rep(3,39),rep(2,63),rep(1,56),rep(0,63)))
Met2 = matrix(c(rep("LRT", 1000), rep("Stepwise", 1000), rep("Lasso", 1000), rep("Stability", 1000)))
KWTab2 = data.frame(Number_of_True_Predictors_Selected = NOP2, Method = Met2)
colnames(KWTab2) = c("Number_of_True_Predictors_Selected", "Method")
attach(KWTab2)
boxplot(Number_of_True_Predictors_Selected~Method)
KWfit2 = kruskal.test(Number_of_True_Predictors_Selected~factor(Method))
KWfit2
WiFit2 = pairwise.wilcox.test(Number_of_True_Predictors_Selected,Method,p.adj='holm',exact=F)
WiFit2
detach(KWTab2)

# 500 setting--- noise normal

NOP3 = matrix(c(rep(8,0),rep(7,0),rep(6,0),rep(5,0),rep(4,4),rep(3,35),rep(2,165),rep(1,390),rep(0,406),rep(8,0),rep(7,0),rep(6,1),rep(5,6),rep(4,58),rep(3,185),rep(2,321),rep(1,313),rep(0,116),rep(8,0),rep(7,0),rep(6,0),rep(5,2),rep(4,4),rep(3,6),rep(2,9),rep(1,234),rep(0,745),rep(8,3),rep(7,0),rep(6,92),rep(5,82),rep(4,17),rep(3,23),rep(2,15),rep(1,23),rep(0,745)))
Met3 = matrix(c(rep("LRT", 1000), rep("Stepwise", 1000), rep("Lasso", 1000), rep("Stability", 1000)))
KWTab3 = data.frame(Number_of_True_Predictors_Selected = NOP3, Method = Met3)
colnames(KWTab3) = c("Number_of_True_Predictors_Selected", "Method")
attach(KWTab3)
boxplot(Number_of_True_Predictors_Selected~Method)
KWfit3 = kruskal.test(Number_of_True_Predictors_Selected~factor(Method))
KWfit3
WiFit3 = pairwise.wilcox.test(Number_of_True_Predictors_Selected,Method,p.adj='holm',exact=F)
WiFit3
detach(KWTab3)

# 5000 setting---No noise

NOP1 = matrix(c(rep(8,196),rep(7,445),rep(6,292),rep(5,65),rep(4,2),rep(3,0),rep(2,0),rep(1,0),rep(0,0),rep(8,426),rep(7,454),rep(6,115),rep(5,5),rep(4,0),rep(3,0),rep(2,0),rep(1,0),rep(0,0),rep(8,998),rep(7,1),rep(6,1),rep(5,0),rep(4,0),rep(3,0),rep(2,0),rep(1,0),rep(0,0),rep(8,1000),rep(7,0),rep(6,0),rep(5,0),rep(4,0),rep(3,0),rep(2,0),rep(1,0),rep(0,0)))
Met1 = matrix(c(rep("LRT", 1000), rep("Stepwise", 1000), rep("Lasso", 1000), rep("Stability", 1000)))
KWTab1 = data.frame(Number_of_True_Predictors_Selected = NOP1, Method = Met1)
colnames(KWTab1) = c("Number_of_True_Predictors_Selected", "Method")
attach(KWTab1)
boxplot(Number_of_True_Predictors_Selected~Method)
KWfit1 = kruskal.test(Number_of_True_Predictors_Selected~factor(Method))
KWfit1
WiFit1 = pairwise.wilcox.test(Number_of_True_Predictors_Selected,Method,p.adj='holm',exact=F)
WiFit1
detach(KWTab1)

# 5000 setting--- noise predictors

NOP2 = matrix(c(rep(8,191),rep(7,438),rep(6,298),rep(5,69),rep(4,4),rep(3,0),rep(2,0),rep(1,0),rep(0,0),rep(8,414),rep(7,456),rep(6,120),rep(5,10),rep(4,0),rep(3,0),rep(2,0),rep(1,0),rep(0,0),rep(8,262),rep(7,313),rep(6,234),rep(5,126),rep(4,58),rep(3,7),rep(2,0),rep(1,0),rep(0,0),rep(8,1000),rep(7,0),rep(6,0),rep(5,0),rep(4,0),rep(3,0),rep(2,0),rep(1,0),rep(0,0)))
Met2 = matrix(c(rep("LRT", 1000), rep("Stepwise", 1000), rep("Lasso", 1000), rep("Stability", 1000)))
KWTab2 = data.frame(Number_of_True_Predictors_Selected = NOP2, Method = Met2)
colnames(KWTab2) = c("Number_of_True_Predictors_Selected", "Method")
attach(KWTab2)
boxplot(Number_of_True_Predictors_Selected~Method)
KWfit2 = kruskal.test(Number_of_True_Predictors_Selected~factor(Method))
KWfit2
WiFit2 = pairwise.wilcox.test(Number_of_True_Predictors_Selected,Method,p.adj='holm',exact=F)
WiFit2
detach(KWTab2)

# 5000 setting--- noise normal

NOP3 = matrix(c(rep(8,1),rep(7,7),rep(6,54),rep(5,215),rep(4,341),rep(3,295),rep(2,85),rep(1,2),rep(0,0),rep(8,5),rep(7,72),rep(6,220),rep(5,341),rep(4,267),rep(3,87),rep(2,8),rep(1,0),rep(0,0),rep(8,10),rep(7,30),rep(6,46),rep(5,81),rep(4,113),rep(3,151),rep(2,117),rep(1,170),rep(0,282),rep(8,598),rep(7,301),rep(6,52),rep(5,35),rep(4,11),rep(3,0),rep(2,3),rep(1,0),rep(0,0)))
Met3 = matrix(c(rep("LRT", 1000), rep("Stepwise", 1000), rep("Lasso", 1000), rep("Stability", 1000)))
KWTab3 = data.frame(Number_of_True_Predictors_Selected = NOP3, Method = Met3)
colnames(KWTab3) = c("Number_of_True_Predictors_Selected", "Method")
attach(KWTab3)
boxplot(Number_of_True_Predictors_Selected~Method)
KWfit3 = kruskal.test(Number_of_True_Predictors_Selected~factor(Method))
KWfit3
WiFit3 = pairwise.wilcox.test(Number_of_True_Predictors_Selected,Method,p.adj='holm',exact=F)
WiFit3
detach(KWTab3)

# 5000 setting--- noise predictors 2
detach(gui_sim)
NOP2 = matrix(c(rep(6,0),rep(5,0),rep(4,0),rep(3,6),rep(2,44),rep(1,236),rep(0,714),rep(6,0),rep(5,0),rep(4,3),rep(3,20),rep(2,126),rep(1,354),rep(0,497),rep(6,0),rep(5,2),rep(4,5),rep(3,16),rep(2,72),rep(1,232),rep(0,673),rep(6,905),rep(5,74),rep(4,14),rep(3,7),rep(2,0),rep(1,0),rep(0,0)))
Met2 = matrix(c(rep("LRT", 1000), rep("Stepwise", 1000), rep("Lasso", 1000), rep("Stability", 1000)))
KWTab2 = data.frame(Number_of_True_Predictors_Selected = NOP2, Method = Met2)
colnames(KWTab2) = c("Number_of_True_Predictors_Selected", "Method")
attach(KWTab2)
boxplot(Number_of_True_Predictors_Selected~Method)
KWfit2 = kruskal.test(Number_of_True_Predictors_Selected~factor(Method))
KWfit2
WiFit2 = pairwise.wilcox.test(Number_of_True_Predictors_Selected,Method,p.adj='holm',exact=F)
WiFit2
detach(KWTab2)

# 500 setting--- noise predictors 2

NOP2 = matrix(c(rep(6,0),rep(5,0),rep(4,0),rep(3,7),rep(2,53),rep(1,273),rep(0,667),rep(6,0),rep(5,0),rep(4,2),rep(3,33),rep(2,134),rep(1,375),rep(0,456),rep(6,0),rep(5,0),rep(4,1),rep(3,5),rep(2,7),rep(1,37),rep(0,950),rep(6,24),rep(5,60),rep(4,60),rep(3,496),rep(2,56),rep(1,44),rep(0,260)))
Met2 = matrix(c(rep("LRT", 1000), rep("Stepwise", 1000), rep("Lasso", 1000), rep("Stability", 1000)))
KWTab2 = data.frame(Number_of_Noise_Predictors_Selected = NOP2, Method = Met2)
colnames(KWTab2) = c("Number_of_Noise_Predictors_Selected", "Method")
attach(KWTab2)
boxplot(Number_of_Noise_Predictors_Selected~Method)
KWfit2 = kruskal.test(Number_of_True_Predictors_Selected~factor(Method))
KWfit2
WiFit2 = pairwise.wilcox.test(Number_of_True_Predictors_Selected,Method,p.adj='bonferroni',exact=F)
WiFit2
detach(KWTab2)

# Plots

# 500 No noise CASES
library(ggplot2)
par(mfrow=c(2,2))
library(gridExtra)

####LRT
Countsslr = matrix(c(0.027,0.158,0.325,0.312,0.139,0.032,0.007,0,0))
pCountsslr = matrix(c(percent(Countsslr)))
CountHlr = matrix(c(0,1,2,3,4,5,6,7,8))
ylr = data.frame(CountHlr,Countsslr)
Plotlr = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHlr, y =..Countsslr..), stat = "identity") + ggtitle("LRT Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotlr = barplot(ylr$Countsslr, names.arg=ylr$CountHlr, main = "LRT Method", xlab = "No of Predictors", ylab = "Frequency") 

####AIC
Countss = matrix(c(.001,.018,.138,.320,.297,.165,.053,.008,0))
CountHs = matrix(c(0,1,2,3,4,5,6,7,8))
ys = data.frame(CountHs,Countss)
Plots = ggplot(data = ys) + geom_bar(mapping = aes(x = CountHs, y =..Countss..), stat = "identity") + ggtitle("Stepwise Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plots = barplot(ys$Countss, names.arg=ys$CountHs, main = "Stepwise Method", xlab = "No of Predictors", ylab = "Frequency") 

####Lasso
Countsl = matrix(c(.001,0,0,.001,0,.002,.001,.115,.880))
CountHl = matrix(c(0,1,2,3,4,5,6,7,8))
yl = data.frame(CountHl,Countsl)
Plotl =ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHl, y =..Countsl..), stat = "identity") + ggtitle("Lasso Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotl = barplot(yl$Countsl, names.arg=yl$CountHl, main = "Lasso Method", xlab = "No of Predictors", ylab = "Frequency") 


####Stab
Countsst = matrix(c(0.06,0.054,0.063,0.035,0.072,0.139,0.401,0.102,0.074))
CountHst = matrix(c(0,1,2,3,4,5,6,7,8))
yst = data.frame(CountHst,Countsst)
Plotst = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHst, y =..Countsst..), stat = "identity") + ggtitle("Stability Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotst = barplot(yst$Countsst, names.arg=yst$CountHst, main = "Stability Method", xlab = "No of Predictors", ylab = "Frequency") 
grid.arrange(Plotlr, Plots, Plotl, Plotst, nrow = 2)





# 500 noise predictors CASES
library(ggplot2)
par(mfrow=c(2,2))


####LRT
Countsslr = matrix(c(0.019,0.106,0.251,0.322,0.180,0.09,0.024,0.006,0.002))
CountHlr = matrix(c(0,1,2,3,4,5,6,7,8))
ylr = data.frame(CountHlr,Countsslr)
Plotlr = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHlr, y =..Countsslr..), stat = "identity") + ggtitle("LRT Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected") + scale_y_continuous(labels = scales::percent_format())

#Plotlr = barplot(ylr$Countsslr, names.arg=ylr$CountHlr, main = "LRT Method", xlab = "No of Predictors", ylab = "Frequency") 

####AIC
Countss = matrix(c(.002,.021,.138,.305,.300,.179,.048,.007,0))
CountHs = matrix(c(0,1,2,3,4,5,6,7,8))
ys = data.frame(CountHs,Countss)
Plots = ggplot(data = ys) + geom_bar(mapping = aes(x = CountHs, y =..Countss..), stat = "identity") + ggtitle("Stepwise Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plots = barplot(ys$Countss, names.arg=ys$CountHs, main = "Stepwise Method", xlab = "No of Predictors", ylab = "Frequency") 

####Lasso
Countsl = matrix(c(.623,0.244,0.053,.031,0.023,.018,.005,.002,.001))
CountHl = matrix(c(0,1,2,3,4,5,6,7,8))
yl = data.frame(CountHl,Countsl)
Plotl =ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHl, y =..Countsl..), stat = "identity") + ggtitle("Lasso Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotl = barplot(yl$Countsl, names.arg=yl$CountHl, main = "Lasso Method", xlab = "No of Predictors", ylab = "Frequency") 


####Stab
Countsst = matrix(c(.063,.056,.063,.039,.074,.144,.405,.098,.058))
CountHst = matrix(c(0,1,2,3,4,5,6,7,8))
yst = data.frame(CountHst,Countsst)
Plotst = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHst, y =..Countsst..), stat = "identity") + ggtitle("Stability Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotst = barplot(yst$Countsst, names.arg=yst$CountHst, main = "Stability Method", xlab = "No of Predictors", ylab = "Frequency") 
grid.arrange(Plotlr, Plots, Plotl, Plotst, nrow = 2)


# 500 noise predictors - 'noise' CASES
library(ggplot2)
par(mfrow=c(2,2))


####LRT
Countsslr = matrix(c(0.667,0.273,0.053,0.007,0,0,0))
CountHlr = matrix(c(0,1,2,3,4,5,6))
ylr = data.frame(CountHlr,Countsslr)
Plotlr = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHlr, y =..Countsslr..), stat = "identity") + ggtitle("LRT Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected") + scale_y_continuous(labels = scales::percent_format())

#Plotlr = barplot(ylr$Countsslr, names.arg=ylr$CountHlr, main = "LRT Method", xlab = "No of Predictors", ylab = "Frequency") 

####AIC
Countss = matrix(c(.456,.375,.134,.033,.002,0,0))
CountHs = matrix(c(0,1,2,3,4,5,6))
ys = data.frame(CountHs,Countss)
Plots = ggplot(data = ys) + geom_bar(mapping = aes(x = CountHs, y =..Countss..), stat = "identity") + ggtitle("Stepwise Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plots = barplot(ys$Countss, names.arg=ys$CountHs, main = "Stepwise Method", xlab = "No of Predictors", ylab = "Frequency") 

####Lasso
Countsl = matrix(c(.95,0.37,0.007,.005,0.001,0,0))
CountHl = matrix(c(0,1,2,3,4,5,6))
yl = data.frame(CountHl,Countsl)
Plotl =ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHl, y =..Countsl..), stat = "identity") + ggtitle("Lasso Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotl = barplot(yl$Countsl, names.arg=yl$CountHl, main = "Lasso Method", xlab = "No of Predictors", ylab = "Frequency") 


####Stab
Countsst = matrix(c(.26,.044,.056,.496,.06,.06,.024))
CountHst = matrix(c(0,1,2,3,4,5,6))
yst = data.frame(CountHst,Countsst)
Plotst = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHst, y =..Countsst..), stat = "identity") + ggtitle("Stability Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotst = barplot(yst$Countsst, names.arg=yst$CountHst, main = "Stability Method", xlab = "No of Predictors", ylab = "Frequency") 
grid.arrange(Plotlr, Plots, Plotl, Plotst, nrow = 2)



# 500 noise normal variates CASES
library(ggplot2)
par(mfrow=c(2,2))


####LRT
Countsslr = matrix(c(0.406,0.39,0.165,0.350,0.004,0,0,0,0))
CountHlr = matrix(c(0,1,2,3,4,5,6,7,8))
ylr = data.frame(CountHlr,Countsslr)
Plotlr = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHlr, y =..Countsslr..), stat = "identity") + ggtitle("LRT Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotlr = barplot(ylr$Countsslr, names.arg=ylr$CountHlr, main = "LRT Method", xlab = "No of Predictors", ylab = "Frequency") 

####AIC
Countss = matrix(c(.116,.313,.321,.185,.058,.006,.001,0,0))
CountHs = matrix(c(0,1,2,3,4,5,6,7,8))
ys = data.frame(CountHs,Countss)
Plots = ggplot(data = ys) + geom_bar(mapping = aes(x = CountHs, y =..Countss..), stat = "identity") + ggtitle("Stepwise Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plots = barplot(ys$Countss, names.arg=ys$CountHs, main = "Stepwise Method", xlab = "No of Predictors", ylab = "Frequency") 

####Lasso
Countsl = matrix(c(.745,0.234,0.009,.006,0.004,.002,0,0,0))
CountHl = matrix(c(0,1,2,3,4,5,6,7,8))
yl = data.frame(CountHl,Countsl)
Plotl =ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHl, y =..Countsl..), stat = "identity") + ggtitle("Lasso Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotl = barplot(yl$Countsl, names.arg=yl$CountHl, main = "Lasso Method", xlab = "No of Predictors", ylab = "Frequency") 


####Stab
Countsst = matrix(c(.745,.023,.015,.023,.017,.082,.92,0,.003))
CountHst = matrix(c(0,1,2,3,4,5,6,7,8))
yst = data.frame(CountHst,Countsst)
Plotst = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHst, y =..Countsst..), stat = "identity") + ggtitle("Stability Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotst = barplot(yst$Countsst, names.arg=yst$CountHst, main = "Stability Method", xlab = "No of Predictors", ylab = "Frequency") 
grid.arrange(Plotlr, Plots, Plotl, Plotst, nrow = 2)


# 5000 No noise CASES
library(ggplot2)
par(mfrow=c(2,2))


####LRT
Countsslr = matrix(c(0,0,0,0,0.002,.065,0.292,0.445,0.196))
pCountsslr = matrix(c(percent(Countsslr)))
CountHlr = matrix(c(0,1,2,3,4,5,6,7,8))
ylr = data.frame(CountHlr,Countsslr)
Plotlr = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHlr, y =..Countsslr..), stat = "identity") + ggtitle("LRT Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotlr = barplot(ylr$Countsslr, names.arg=ylr$CountHlr, main = "LRT Method", xlab = "No of Predictors", ylab = "Frequency") 

####AIC
Countss = matrix(c(0,0,0,0,0,0.005,.115,0.454,0.426))
CountHs = matrix(c(0,1,2,3,4,5,6,7,8))
ys = data.frame(CountHs,Countss)
Plots = ggplot(data = ys) + geom_bar(mapping = aes(x = CountHs, y =..Countss..), stat = "identity") + ggtitle("Stepwise Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plots = barplot(ys$Countss, names.arg=ys$CountHs, main = "Stepwise Method", xlab = "No of Predictors", ylab = "Frequency") 

####Lasso
Countsl = matrix(c(0,0,0,0,0,0,0.001,.001,.998))
CountHl = matrix(c(0,1,2,3,4,5,6,7,8))
yl = data.frame(CountHl,Countsl)
Plotl =ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHl, y =..Countsl..), stat = "identity") + ggtitle("Lasso Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotl = barplot(yl$Countsl, names.arg=yl$CountHl, main = "Lasso Method", xlab = "No of Predictors", ylab = "Frequency") 


####Stab
Countsst = matrix(c(0,0,0,0,0,0,0,0,1))
CountHst = matrix(c(0,1,2,3,4,5,6,7,8))
yst = data.frame(CountHst,Countsst)
Plotst = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHst, y =..Countsst..), stat = "identity") + ggtitle("Stability Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotst = barplot(yst$Countsst, names.arg=yst$CountHst, main = "Stability Method", xlab = "No of Predictors", ylab = "Frequency") 
grid.arrange(Plotlr, Plots, Plotl, Plotst, nrow = 2)



# 500 noise predictors CASES
library(ggplot2)
par(mfrow=c(2,2))


####LRT
Countsslr = matrix(c(0.019,0.106,0.251,0.322,0.180,0.09,0.024,0.006,0.002))
CountHlr = matrix(c(0,1,2,3,4,5,6,7,8))
ylr = data.frame(CountHlr,Countsslr)
Plotlr = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHlr, y =..Countsslr..), stat = "identity") + ggtitle("LRT Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotlr = barplot(ylr$Countsslr, names.arg=ylr$CountHlr, main = "LRT Method", xlab = "No of Predictors", ylab = "Frequency") 

####AIC
Countss = matrix(c(.002,.021,.138,.305,.300,.179,.048,.007,0))
CountHs = matrix(c(0,1,2,3,4,5,6,7,8))
ys = data.frame(CountHs,Countss)
Plots = ggplot(data = ys) + geom_bar(mapping = aes(x = CountHs, y =..Countss..), stat = "identity") + ggtitle("Stepwise Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plots = barplot(ys$Countss, names.arg=ys$CountHs, main = "Stepwise Method", xlab = "No of Predictors", ylab = "Frequency") 

####Lasso
Countsl = matrix(c(.623,0.244,0.053,.031,0.023,.018,.005,.002,.001))
CountHl = matrix(c(0,1,2,3,4,5,6,7,8))
yl = data.frame(CountHl,Countsl)
Plotl =ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHl, y =..Countsl..), stat = "identity") + ggtitle("Lasso Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotl = barplot(yl$Countsl, names.arg=yl$CountHl, main = "Lasso Method", xlab = "No of Predictors", ylab = "Frequency") 


####Stab
Countsst = matrix(c(.06,.036,.064,.044,.084,.168,.376,.124,.044))
CountHst = matrix(c(0,1,2,3,4,5,6,7,8))
yst = data.frame(CountHst,Countsst)
Plotst = ggplot(data = ylr) + geom_bar(mapping = aes(x = CountHst, y =..Countsst..), stat = "identity") + ggtitle("Stability Method") +
  xlab("No of Predictors") + ylab("Percentage of times selected")+ scale_y_continuous(labels = scales::percent_format())

#Plotst = barplot(yst$Countsst, names.arg=yst$CountHst, main = "Stability Method", xlab = "No of Predictors", ylab = "Frequency") 
grid.arrange(Plotlr, Plots, Plotl, Plotst, nrow = 2)


