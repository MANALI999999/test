library(reshape)
library(tidyverse)
library(ggcorrplot)
library(corrplot)
library(Metrics)
library(hydroGOF)
library(e1071)
# library(caret)
library(dplyr)
library(ggplot2)
library(glmnet)
library(kernlab)
library(rpart)
library(randomForest)
library(gbm)

# # create example data
# X <- c("A", "B", "C", "A", "B", "C")
# Y <- c(10, 20, 30, 15, 25, 35)
# 
# # create dummy variables
# X_dummies <- model.matrix(~ X - 1)
# 
# # generate linear regression model
# model <- lm(Y ~ X_dummies)
# 
# # display summary of model
# summary(model)



#Property Data 

total_data <- read_csv("sdmproject.csv.csv")
total_df = as.data.frame(total_data)
total_df = total_df[,!(names(total_df) %in% c("...1","gender"))]
# total_df.drop("...1")
new_df = total_df
#outliers removal 
clean_df = total_df
#clean_df = drop_na(clean_df) # gender is not being used , 
#incomplete_flag
clean_df = clean_df[clean_df$incomplete_flag==0 ,]
#gender - o is being removed as we have only 4 observations
#clean_df = subset(clean_df, clean_df$gender %in% c("m", "f"))
# viewed = 0 , explored = 1 
clean_df = clean_df[!(clean_df$viewed == 0 & clean_df$explored == 1),]
#Explored , n_chapters
# Assumpotion - atleast one of students accessed all chapters in coursework
Agg = aggregate(nchapters ~ course_id, data = clean_df, FUN = max)
Agg$chapter_limit = round(Agg$nchapters/2,0)
Agg[Agg$course_id==1,2]/2
Agg = Agg[,!(names(Agg) %in% c('nchapters'))]
clean_df = merge(clean_df,Agg, by = 'course_id')
clean_df =  clean_df[(clean_df$explored == 1 & clean_df$nchapters >= clean_df$chapter_limit) | (clean_df$explored == 0 & clean_df$nchapters < clean_df$chapter_limit), ]
clean_df = clean_df[,!(names(clean_df) %in% c('chapter_limit'))]
#date 
clean_df = clean_df[(clean_df$last_event_DI >= clean_df$start_time_DI),]
#Grade, certified 
Agg1 = aggregate(grade ~ course_id + certified, data = clean_df, FUN = max )
Agg2 = aggregate(grade ~ course_id + certified, data = clean_df, FUN = min )
Agg_m = merge(Agg1,Agg2, by = c('course_id','certified') )
Agg_f = Agg_m[Agg_m$certified == 0 ,c('course_id','grade.x')]
clean_df = merge(clean_df,Agg_f, by = 'course_id')
clean_df = clean_df[(clean_df$certified == 0 & clean_df$grade <= clean_df$grade.x) | (clean_df$certified == 1 & clean_df$grade > clean_df$grade.x),]
clean_df = clean_df[,!(names(clean_df) %in% c('grade.x'))]
Agg1 = aggregate(grade ~ course_id + certified, data = clean_df, FUN = max )
Agg2 = aggregate(grade ~ course_id + certified, data = clean_df, FUN = min )
Agg_m = merge(Agg1,Agg2, by = c('course_id','certified') )

#age >= 9  - can be implemented
# adding days_diff
clean_df$n_days = as.numeric(difftime(clean_df$last_event_DI, clean_df$start_time_DI, units = "days")+1)

test_df = clean_df[clean_df$ndays_act > clean_df$n_days,c('start_time_DI','last_event_DI','ndays_act','n_days')]

# ndays_act <= n_day
clean_df = clean_df[clean_df$ndays_act <= clean_df$n_days,]


# anymore outlier conditions should be implemented



# Non Numeric columns are being factorised for correlation calculation
colnames(total_df)

#course_id 
L = factor(clean_df$course_id)
K = as.numeric(L)
clean_df$course_id = K
# [1] "14.73x" "2.01x"  "3.091x" "6.002x" "6.00x"  "7.00x"  "8.02x"  "8.MReV" "CB22x" 
#[10] "CS50x"  "ER22x"  "PH207x" "PH278x"

# institute - need to be needed as course and institute are directly related
L = factor(clean_df$institute, levels = c("MITx","HarvardX"))
K = as.numeric(L)
clean_df$institute = K
#"HarvardX" "MITx"


# Semester
L = factor(clean_df$semester)
K = as.numeric(L)
clean_df$semester = K
# "Fall"   "Spring" "Summer"

# LoE_DI
L = factor(clean_df$LoE_DI, levels = c("Less than Secondary","Secondary" ,"Bachelor's","Master's","Doctorate"))
levels(L)
K = as.numeric(L)
clean_df$LoE_DI = K

# gender
L = factor(clean_df$gender)
levels(L)
K = as.numeric(L)
clean_df$gender = K
#"f" "m" "o"

# final_cc_cname_DI
L = factor(clean_df$final_cc_cname_DI)
levels(L)
K = as.numeric(L)
clean_df$final_cc_cname_DI = K
# clean_df$final_cc_cname_DI = as.numeric(new_df$final_cc_cname_DI)
# [1] "Australia"                              "Bangladesh"                            
# [3] "Brazil"                                 "Canada"                                
# [5] "China"                                  "Colombia"                              
# [7] "Egypt"                                  "France"                                
# [9] "Germany"                                "Greece"                                
# [11] "India"                                  "Indonesia"                             
# [13] "Japan"                                  "Mexico"                                
# [15] "Morocco"                                "Nigeria"                               
# [17] "Other Africa"                           "Other East Asia"                       
# [19] "Other Europe"                           "Other Middle East/Central Asia"        
# [21] "Other North & Central Amer., Caribbean" "Other Oceania"                         
# [23] "Other South America"                    "Other South Asia"                      
# [25] "Pakistan"                               "Philippines"                           
# [27] "Poland"                                 "Portugal"                              
# [29] "Russian Federation"                     "Spain"                                 
# [31] "Ukraine"                                "United Kingdom"                        
# [33] "United States"                          "Unknown/Other" 

str(clean_df)

work_df = clean_df[,!(names(new_df) %in% c("userid_DI","start_time_DI","last_event_DI",'incomplete_flag'))]


# Correlation such as Spearman and Kendall can be used for comparision between
#numeric and non- numeric valued columns.

#Spearman_correlation

cor_work_data = cor(work_df,method = 'spearman')

#plotting heat map
uppertri_mean_pearson = cor_work_data
uppertri_mean_pearson[lower.tri(cor_work_data)] =NA 
uppertri_melted = melt(uppertri_mean_pearson, na.rm = TRUE)

# corrplot(cor_work_data,method = "number")

corrplot(cor_work_data, method = "circle", addCoef.col = "black", 
         type = "lower",  diag = FALSE ,tl.col ="black" ,tl.cex = 0.7,
         title = "Spearman Correlation matrix changed",number.cex = 0.5,
         mar=c(0,0,1,0))

#kendall_correlation

cor_work_data_Kendall = cor(work_df,method = 'kendall')

#plotting heat map
uppertri_mean_kendall = cor_work_data_Kendall
uppertri_mean_kendall[lower.tri(cor_work_data)] =NA 
uppertri_melted = melt(uppertri_mean_kendall, na.rm = TRUE)

# corrplot(cor_work_data,method = "number")

corrplot(cor_work_data, method = "circle", addCoef.col = "black", 
         type = "lower",  diag = FALSE ,tl.col ="black" ,tl.cex = 0.7,
         title = "Kendall Correlation matrix changed",number.cex = 0.5,
         mar=c(0,0,1,0))

#Pearson_correlation - plotiing it as kendall is taking a lot of time for proccesing

cor_work_data_Pearson = cor(work_df,method = 'pearson')

#plotting heat map
uppertri_mean_Pearson = cor_work_data_Pearson
uppertri_mean_Pearson[lower.tri(cor_work_data_Pearson)] =NA 
uppertri_melted = melt(uppertri_mean_Pearson, na.rm = TRUE)

# corrplot(cor_work_data,method = "number")

corrplot(cor_work_data_Pearson, method = "circle", addCoef.col = "black", 
         type = "lower",  diag = FALSE ,tl.col ="black" ,tl.cex = 0.7,
         title = "Pearson Correlation matrix changed",number.cex = 0.5,
         mar=c(0,0,1,0))


# At this point , outliers Data processing and wrangling is done and 
# Correlation is plotted

# Assumption : more the participation , more the learning , more the grade
# need to start selecting features and modelling for the 5 questions

#Codes for regression
#Multiple Linear Regression
set.seed(80)
# work_df = new_df[,(names(new_df) %in% c("grade","nplay_video"))]
work_df$grade = 100*work_df$grade
work_df = new_df[,(names(new_df) %in% c("grade","nevents","ndays_act","nplay_video","nchapters", "nforum_posts" ))]
# names(work_df) ,"institute"
mooc_datasort_mlr<-sample(1:nrow(work_df),nrow(work_df)*.8)
train_mlr<-work_df[mooc_datasort_mlr,]
test_mlr<-work_df[-mooc_datasort_mlr,]
mdl_mlr<-lm(grade~.,data=train_mlr)
pred_train_mlr<-predict(mdl_mlr,train_mlr)
pred_test_mlr<-predict(mdl_mlr,test_mlr)
#Get RMSE values
rmse_mlr_train<-rmse(pred_train_mlr,train_mlr$grade)
rmse_mlr_train
rmse_mlr_test<-rmse(pred_test_mlr,test_mlr$grade)
rmse_mlr_test
#R2 value for training data
sst<-sum((train_mlr$grade-mean(train_mlr$grade))^2,na.rm=TRUE)
sst
sse<-sum((pred_train_mlr-train_mlr$grade)^2,na.rm=TRUE)
sse
rsq<-1-sse/sst
rsq
hd = names(work_df)
plot(train_mlr$grade,pred_train_mlr,xlab="Actual",ylab="Predicted",main = hd)
abline(a=0,b=1,col = 'red')
summary(mdl_mlr)


# Principal Component Regression (PCR)
prop_PCR<-work_df$grade
feature_PCR<-work_df[,!names(work_df)%in%c("grade")]
feature_PCR_std<-as.data.frame(scale(feature_PCR))
pca<-prcomp(feature_PCR_std)
pca
plot(pca$sdev)
#Select number of PCs
#In this case, we'll select 4
scores<-pca$x[,1:4]
loads<-pca$rotation[,1:4]
pcr_data<-cbind(prop_PCR,scores)
#Have now defined new data with property and PCs
#Now repeat the MLR process
set.seed(508)
mooc_datasort_pcr<-sample(1:nrow(pcr_data),nrow(pcr_data)*.8)
train_pcr<-pcr_data[mooc_datasort_pcr,]
test_pcr<-pcr_data[-mooc_datasort_pcr,]
train_pcr_d<-as.data.frame(train_pcr)
test_pcr_d<-as.data.frame(test_pcr)
mdl_pcr<-lm(prop_PCR~.,data=train_pcr_d)
pred_train_pcr<-predict(mdl_pcr,train_pcr_d)
pred_test_pcr<-predict(mdl_pcr,test_pcr_d)
#Get RMSE values
rmse_pcr_train<-rmse(pred_train_pcr,train_pcr_d$prop_PCR)
rmse_pcr_train
rmse_pcr_test<-rmse(pred_test_pcr,test_pcr_d$prop_PCR)
rmse_pcr_test
#R2 value for training data
sst<-sum((train_pcr_d$prop_PCR-mean(train_pcr_d$prop_PCR))^2)
sse<-sum((pred_train_pcr-train_pcr_d$prop_PCR)^2)
rsq<-1-sse/sst
rsq
plot(train_pcr_d$prop_PCR,pred_train_pcr,xlab="Actual",ylab="Predicted")

#Ridge Regression
# data(mtcars)
set.seed(508)
datasort<-sample(1:nrow(work_df),nrow(work_df)*0.8)
train_ridge<-work_df[datasort,]
test_ridge<-work_df[-datasort,]
descriptors_train_ridge<-train_ridge[,! names(train_ridge) %in% c("grade")]
descriptors_test_ridge<-test_ridge[,! names(test_ridge) %in% c("grade")]
descriptors_train_ridge<-as.matrix(descriptors_train_ridge)
descriptors_test_ridge<-as.matrix(descriptors_test_ridge)
mdl_ridge<-glmnet(descriptors_train_ridge,train_ridge$grade,alpha=0)
mdl_ridge_cv<-cv.glmnet(descriptors_train_ridge,train_ridge$grade,alpha=0)
best_lambda<-mdl_ridge_cv$lambda.min
mdl_ridge_best<-glmnet(descriptors_train_ridge,train_ridge$grade,alpha=0,lambda=best_lambda)
coef(mdl_ridge_best)
pred_train_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_train_ridge)
pred_test_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_test_ridge)
pred_train_ridge<-as.data.frame(pred_train_ridge)
pred_test_ridge<-as.data.frame(pred_test_ridge)
rmse_ridge_train<-rmse(pred_train_ridge,train_ridge$grade)
rmse_ridge_test<-rmse(pred_test_ridge,test_ridge$grade)
rmse_ridge_train
rmse_ridge_test
sst<-sum((train_ridge$grade-mean(train_ridge$grade))^2)
sse<-sum((pred_train_ridge-train_ridge$grade)^2)
rsq<-1-sse/sst
rsq


#Lasso Regression
# data(mtcars)
set.seed(508)
datasort<-sample(1:nrow(work_df),nrow(work_df)*0.8)
train_ridge<-work_df[datasort,]
test_ridge<-work_df[-datasort,]
descriptors_train_ridge<-train_ridge[,! names(train_ridge) %in% c("grade")]
descriptors_test_ridge<-test_ridge[,! names(test_ridge) %in% c("grade")]
descriptors_train_ridge<-as.matrix(descriptors_train_ridge)
descriptors_test_ridge<-as.matrix(descriptors_test_ridge)
mdl_ridge<-glmnet(descriptors_train_ridge,train_ridge$grade,alpha=1)
mdl_ridge_cv<-cv.glmnet(descriptors_train_ridge,train_ridge$grade,alpha=1)
best_lambda<-mdl_ridge_cv$lambda.min
mdl_ridge_best<-glmnet(descriptors_train_ridge,train_ridge$grade,alpha=1,lambda=best_lambda)
coef(mdl_ridge_best)
pred_train_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_train_ridge)
pred_test_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_test_ridge)
pred_train_ridge<-as.data.frame(pred_train_ridge)
pred_test_ridge<-as.data.frame(pred_test_ridge)
rmse_ridge_train<-rmse(pred_train_ridge,train_ridge$grade)
rmse_ridge_test<-rmse(pred_test_ridge,test_ridge$grade)
rmse_ridge_train
rmse_ridge_test
sst<-sum((train_ridge$grade-mean(train_ridge$grade))^2)
sse<-sum((pred_train_ridge-train_ridge$grade)^2)
rsq<-1-sse/sst
rsq

# need more processing for this 


#Support vector regression
set.seed(508)
svr_datasort<-sample(1:nrow(work_df),nrow(work_df)*0.8)
train_svr<-work_df[svr_datasort,]
test_svr<-work_df[-svr_datasort,]
train_svr_d<-data.frame(train_svr)
descriptors_train_svr<-train_svr[,! names(train_svr) %in% c("grade")]
descriptors_test_svr<-test_svr[,! names(test_svr) %in% c("grade")]
descriptors_train_svr<-as.matrix(descriptors_train_svr)
descriptors_test_svr<-as.data.frame(descriptors_test_svr)
prop_train_svr<-train_svr$grade
prop_test_svr<-test_svr$grade
mdl_svr<-tune(svm,prop_train_svr~descriptors_train_svr,ranges=list(epsilon=seq(0,1,0.1),cost=1:10))
BstModel<-mdl_svr$best.model
summary(BstModel)
#Update the regression model with the selections from BstModel (kernel, cost, gamma, epsilon)
svmfit <- svm(train_svr$grade ~., data = train_svr, method="eps-regression",kernel = 'radial', cost = 3, gamma=0.1,epsilon=.3,scale=FALSE)
pred_train_svr<-predict(svmfit, data=descriptors_train_svr)
pred_test_svr<-predict(svmfit,newdata=descriptors_test_svr)
rmse_SVR_train<-rmse(pred_train_svr,prop_train_svr)
rmse_SVR_test<-rmse(pred_test_svr,prop_test_svr)
rmse_SVR_train
rmse_SVR_test
sst<-sum((train_svr$mpg-mean(train_svr$mpg))^2)
sse<-sum((pred_train_svr-train_svr$mpg)^2)
rsq<-1-sse/sst
rsq

#Gaussian Process Regression
set.seed(508)
datasort<-sample(1:nrow(work_df),nrow(work_df)*0.8)
train_gpr<-work_df[datasort,]
test_gpr<-work_df[-datasort,]
descriptors_train_gpr<-train_gpr[,! names(train_gpr) %in% c("grade")]
descriptors_test_gpr<-test_gpr[,! names(test_gpr) %in% c("grade")]
mdl_gpr<-gausspr(descriptors_train_gpr,train_gpr$grade)
pred_train_gpr<-predict(mdl_gpr,descriptors_train_gpr)
pred_test_gpr<-predict(mdl_gpr,descriptors_test_gpr)
rmse_gpr_train<-rmse(pred_train_gpr,as.matrix(train_gpr$grade))
rmse_gpr_test<-rmse(pred_test_gpr,as.matrix(test_gpr$grade))
rmse_gpr_train
rmse_gpr_test
sst<-sum((train_gpr$grade-mean(train_gpr$grade))^2)
sse<-sum((pred_train_gpr-train_gpr$grade)^2)
rsq<-1-sse/sst
rsq


#Random Forest Regression
set.seed(508)
datasort<-sample(1:nrow(work_df),nrow(work_df)*0.8)
train_rf<-work_df[datasort,]
test_rf<-work_df[-datasort,]
model_rf<-randomForest(train_rf$grade~.,data=train_rf,mtry=3,importance=TRUE,na.action=na.omit)
pred_train_rf<-predict(model_rf,train_rf)
pred_test_rf<-predict(model_rf,newdata=test_rf)
rmse_rf_train<-rmse(pred_train_rf,train_rf$grade)
rmse_rf_test<-rmse(pred_test_rf,test_rf$grade)
rmse_rf_train
rmse_rf_test
sst<-sum((train_rf$grade-mean(train_rf$grade))^2)
sse<-sum((pred_train_rf-train_rf$grade)^2)
rsq<-1-sse/sst
rsq
plot(model_rf)
