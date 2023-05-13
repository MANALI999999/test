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
# library(caret)
library(rpart)

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
levels(L)
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
levels(L)
K = as.numeric(L)
clean_df$semester = K
# "Fall"   "Spring" "Summer"

# LoE_DI
L = factor(clean_df$LoE_DI, levels = c("Less than Secondary","Secondary" ,"Bachelor's","Master's","Doctorate"))
levels(L)
K = as.numeric(L)
clean_df$LoE_DI = K

# gender
# L = factor(clean_df$gender)
# levels(L)
# K = as.numeric(L)
# clean_df$gender = K
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

# #kendall_correlation
# 
# cor_work_data_Kendall = cor(work_df,method = 'kendall')
# 
# #plotting heat map
# uppertri_mean_kendall = cor_work_data_Kendall
# uppertri_mean_kendall[lower.tri(cor_work_data)] =NA 
# uppertri_melted = melt(uppertri_mean_kendall, na.rm = TRUE)
# 
# # corrplot(cor_work_data,method = "number")
# 
# corrplot(cor_work_data_Kendall, method = "circle", addCoef.col = "black", 
#          type = "lower",  diag = FALSE ,tl.col ="black" ,tl.cex = 0.7,
#          title = "Kendall Correlation matrix changed",number.cex = 0.5,
#          mar=c(0,0,1,0))


#Pearson_correlation

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

names(new_df)


#Linear Regression
work_df = clean_df[,!(names(new_df) %in% c("userid_DI","start_time_DI","last_event_DI",'incomplete_flag'))]
set.seed(508)
work_df$grade = 100*work_df$grade
work_df = as.data.frame(scale(new_df[,(names(new_df) %in% c("nevents","nplay_video"))]))
# names(work_df) ,"institute"
mooc_datasort_mlr<-sample(1:nrow(work_df),nrow(work_df)*.8)
train_mlr<-work_df[mooc_datasort_mlr,]
test_mlr<-work_df[-mooc_datasort_mlr,]
mdl_mlr<-lm(nevents~.,data=train_mlr)
pred_train_mlr<-predict(mdl_mlr,train_mlr)
pred_test_mlr<-predict(mdl_mlr,test_mlr)
#Get RMSE values
rmse_mlr_train<-rmse(pred_train_mlr,train_mlr$nevents)
rmse_mlr_train
rmse_mlr_test<-rmse(pred_test_mlr,test_mlr$nevents)
rmse_mlr_test
#R2 value for training data
sst<-sum((train_mlr$nevents-mean(train_mlr$nevents))^2,na.rm=TRUE)
sse<-sum((pred_train_mlr-train_mlr$nevents)^2,na.rm=TRUE)
rsq<-1-sse/sst
rsq
sst<-sum((test_mlr$nevents-mean(test_mlr$nevents))^2,na.rm=TRUE)
sse<-sum((pred_test_mlr-test_mlr$nevents)^2,na.rm=TRUE)
rsq_test<-1-sse/sst
lr_matrix=matrix(0,1,6)
lr_matrix[1,]=c(sed,rmse_mlr_train,rmse_mlr_test,rsq,rsq_test,features)
hd = names(work_df)
plot(train_mlr$nevents,pred_train_mlr,xlab="Actual",ylab="Predicted",main = hd)
abline(a=0,b=1,col = 'red')
K_L = as.data.frame(round(mdl_mlr$coefficients,6))
K_L['seed'] = sed
K_L
summary(mdl_mlr)


# dummy_model <- dummyVars(~., data = work_df[,2:4])
# data_transformed <- predict(dummy_model, newdata = data[,2:4])
# data_final <- cbind(data[1,],data_transformed, data[,5:7])

# Define a list of elements
my_list <- list("A", "B", "C")

# Generate all possible combinations of the list elements
combos <- expand.grid(my_list)

# Convert the resulting data frame to a list
combo_list <- as.list(as.matrix(combos))

combo_list

# Flatten the resulting list
combo_list_flat <- unlist(combo_list, recursive = FALSE)

combo_list_flat


ALL_Features = list("course_id","institute","year",  "semester", "viewed","explored", "certified","final_cc_cname_DI" ,"LoE_DI","grade", "nevents",  "ndays_act","nplay_video" ,"nforum_posts","age","n_days")
length(ALL_Features)
df_compiled_mlr <- data.frame('seed'= numeric(),'Rmse_train'= numeric(),'Rmse_test'= numeric(),'r_squared_train'=numeric(),'r_squared_test'=numeric(),'features'=character())
coef_compiled_mlr= data.frame('coefficients'= numeric(),'seed'=numeric())
combos = combn(ALL_Features,2)
for (i in 1:ncol(combos)){
feature_list = list('nchapters',paste(combos[1,i]),paste(combos[2,i]))
#Multiple Linear Regression
# work_df = clean_df[,!(names(new_df) %in% c("userid_DI","start_time_DI","last_event_DI",'incomplete_flag'))]
# feature_list = list('grade','viewed','explored','certified','nevents','ndays_act','ndays','nchapters','nplay_video')
# feature_list = list('nevents','ndays_act','ndays','nchapters','nplay_video','nforum_posts' )
# df_compiled_mlr <- data.frame('seed'= numeric(),'Rmse_train'= numeric(),'Rmse_test'= numeric(),'r_squared_train'=numeric(),'r_squared_test'=numeric(),'features'=character())
# coef_compiled_mlr= data.frame('coefficients'= numeric(),'seed'=numeric())
seed_list = list(508,600,200,400,510)
for (sed in seed_list){
  features = paste(feature_list, collapse = ",")
  set.seed(sed)
  work_df = clean_df[,!(names(new_df) %in% c("userid_DI","start_time_DI","last_event_DI",'incomplete_flag'))]
  work_df = work_df[,(names(work_df) %in% feature_list)]
  mooc_datasort_mlr<-sample(1:nrow(work_df),nrow(work_df)*.8)
  train_mlr<-work_df[mooc_datasort_mlr,]
  test_mlr<-work_df[-mooc_datasort_mlr,]
  mdl_mlr<-lm(nchapters~.,data=train_mlr)
  pred_train_mlr<-predict(mdl_mlr,train_mlr)
  pred_test_mlr<-predict(mdl_mlr,test_mlr)
  #Get RMSE values
  rmse_mlr_train<-rmse(pred_train_mlr,train_mlr$nchapters)
  rmse_mlr_train
  rmse_mlr_test<-rmse(pred_test_mlr,test_mlr$nchapters)
  rmse_mlr_test
  #R2 value for training data
  sst<-sum((train_mlr$nchapters-mean(train_mlr$nchapters))^2,na.rm=TRUE)
  sst
  sse<-sum((pred_train_mlr-train_mlr$nchapters)^2,na.rm=TRUE)
  sse
  rsq<-1-sse/sst
  rsq
  sst<-sum((test_mlr$nchapters-mean(test_mlr$nchapters))^2,na.rm=TRUE)
  sse<-sum((pred_test_mlr-test_mlr$nchapters)^2,na.rm=TRUE)
  rsq_test<-1-sse/sst
  rsq_test
  mlr_matrix=matrix(0,1,6)
  mlr_matrix[1,]=c(sed,rmse_mlr_train,rmse_mlr_test,rsq,rsq_test,features)
  df_compiled_mlr = rbind(df_compiled_mlr,mlr_matrix)
  hd = paste(features,'_',as.character(sed))
  plot(train_mlr$nchapters,pred_train_mlr,xlab="Actual",ylab="Predicted",main = hd)
  abline(a=0,b=1,col = 'red')
  K = as.data.frame(round(mdl_mlr$coefficients,6))
  K['seed'] = sed
  coef_compiled_mlr = rbind(coef_compiled_mlr,K)
}
}
names(df_compiled_mlr) <- c('seed','Rmse_train','Rmse_test','r_squared_train','r_squared_test','features')
names(coef_compiled_mlr) = c('coefficients','seed')

write.csv(df_compiled_mlr,'df_compiled_mlr.csv')
write.csv(coef_compiled_mlr,'coef_compiled_mlr.csv')



#Ridge Regression

# feature_list = list('grade','viewed','explored','certified','nevents','ndays_act','ndays','nchapters','nplay_video' )
feature_list = list('nevents','ndays_act','ndays','nchapters','nplay_video','nforum_posts' )
df_compiled_ridge <- data.frame('seed'= numeric(),'Rmse_train'= numeric(),'Rmse_test'= numeric(),'r_squared_train'=numeric(),'r_squared_test'=numeric(),'features'=character())
coef_compiled_ridge = data.frame('coefficient'=numeric(),seed=numeric())
# n_cols <- length(feature_list)
# col_names <- paste0("Feature_", 1:n_cols)
# coef_compiled_ridge <- data.frame(matrix(nrow = 0, ncol = n_cols, dimnames = list(NULL, col_names)))
seed_list = list(508,600,200)
work_df = clean_df[,!(names(new_df) %in% c("userid_DI","start_time_DI","last_event_DI",'incomplete_flag'))]
work_df = work_df[,(names(work_df) %in% feature_list)]
model_list = list()
for (sed in seed_list){
  features = paste(feature_list, collapse = ",")
  set.seed(sed)
  datasort<-sample(1:nrow(work_df),nrow(work_df)*0.8)
  train_ridge<-work_df[datasort,]
  test_ridge<-work_df[-datasort,]
  descriptors_train_ridge<-train_ridge[,! names(train_ridge) %in% c("nevents")]
  descriptors_test_ridge<-test_ridge[,! names(test_ridge) %in% c("nevents")]
  descriptors_train_ridge<-as.matrix(descriptors_train_ridge)
  descriptors_test_ridge<-as.matrix(descriptors_test_ridge)
  mdl_ridge<-glmnet(descriptors_train_ridge,train_ridge$nevents,alpha=0)
  mdl_ridge_cv<-cv.glmnet(descriptors_train_ridge,train_ridge$nevents,alpha=0)
  best_lambda<-mdl_ridge_cv$lambda.min
  mdl_ridge_best<-glmnet(descriptors_train_ridge,train_ridge$nevents,alpha=0,lambda=best_lambda)
  coef(mdl_ridge_best)
  model_list = append(model_list,mdl_ridge_best)
  pred_train_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_train_ridge)
  pred_test_ridge<-predict(mdl_ridge,s=best_lambda,newx=descriptors_test_ridge)
  pred_train_ridge<-as.data.frame(pred_train_ridge)
  pred_test_ridge<-as.data.frame(pred_test_ridge)
  rmse_ridge_train<-rmse(pred_train_ridge,train_ridge$nevents)
  rmse_ridge_test<-rmse(pred_test_ridge,test_ridge$nevents)
  rmse_ridge_train
  rmse_ridge_test
  sst<-sum((train_ridge$nevents-mean(train_ridge$nevents))^2)
  sse<-sum((pred_train_ridge-train_ridge$nevents)^2)
  rsq<-1-sse/sst
  rsq
  sst<-sum((test_ridge$nevents-mean(test_ridge$nevents))^2)
  sse<-sum((pred_test_ridge-test_ridge$nevents)^2)
  rsq_test<-1-sse/sst
  rsq_test
  ridge_matrix=matrix(0,1,6)
  ridge_matrix[1,]=c(sed,rmse_mlr_train,rmse_mlr_test,rsq,rsq_test,features)
  df_compiled_ridge = rbind(df_compiled_ridge,ridge_matrix)
  coef_vec <- as.vector(coef(mdl_ridge_best)[-1])
  names(coef_vec) <- paste("Feature", seq_along(coef_vec))
  coef_df <- as.data.frame(t(coef_vec))
  coef_df = data.frame(t(coef_df))
  coef_df['seed'] = sed
  coef_compiled_ridge = rbind(coef_compiled_ridge,coef_df)
}
names(df_compiled_ridge) <- c('seed','Rmse_train','Rmse_test','r_squared_train','r_squared_test','features')
names(coef_compiled_ridge) = c('coefficients','seed')
write.csv(df_compiled_ridge,'df_compiled_ridge.csv')
write.csv(coef_compiled_ridge,'coef_compiled_ridge.csv')


model_list

for (i in model_list){
  type(i)
  coef(i)
}




mdl_ridge_best

list(rownames(coef(mdl_ridge_best)))

rownames_to_column(var = rownames(coef(mdl_ridge_best)))

K = as.data.frame(coef(mdl_ridge_best)[-1])
K['Feature'] = c(rownames(coef(mdl_ridge_best)))
K

paste(coef(mdl_ridge_best)[1])

#Lasso Regression
work_df = clean_df[,!(names(new_df) %in% c("userid_DI","start_time_DI","last_event_DI",'incomplete_flag'))]
work_df = work_df[,(names(work_df) %in% feature_list)]
df_compiled_lasso <- data.frame('seed'= numeric(),'Rmse_train'= numeric(),'Rmse_test'= numeric(),'r_squared_train'=numeric(),'r_squared_test'=numeric(),'features'=character())
coef_compiled_lasso = data.frame('coefficient'=numeric(),seed=numeric())
# n_cols <- length(feature_list)
# col_names <- paste0("Feature_", 1:n_cols)
# coef_compiled_lasso <- data.frame(matrix(nrow = 0, ncol = n_cols, dimnames = list(NULL, col_names)))
seed_list = list(508,600,200)
for (sed in seed_list){
  features = paste(feature_list, collapse = ",")
  set.seed(sed)
  datasort<-sample(1:nrow(work_df),nrow(work_df)*0.8)
  train_lasso<-work_df[datasort,]
  test_lasso<-work_df[-datasort,]
  descriptors_train_lasso<-train_lasso[,! names(train_lasso) %in% c("nevents")]
  descriptors_test_lasso<-test_lasso[,! names(test_lasso) %in% c("nevents")]
  descriptors_train_lasso<-as.matrix(descriptors_train_lasso)
  descriptors_test_lasso<-as.matrix(descriptors_test_lasso)
  mdl_lasso<-glmnet(descriptors_train_lasso,train_lasso$nevents,alpha=1)
  mdl_lasso_cv<-cv.glmnet(descriptors_train_lasso,train_lasso$nevents,alpha=1)
  best_lambda<-mdl_lasso_cv$lambda.min
  mdl_lasso_best<-glmnet(descriptors_train_lasso,train_lasso$nevents,alpha=1,lambda=best_lambda)
  coef(mdl_lasso_best)
  pred_train_lasso<-predict(mdl_lasso,s=best_lambda,newx=descriptors_train_lasso)
  pred_test_lasso<-predict(mdl_lasso,s=best_lambda,newx=descriptors_test_lasso)
  pred_train_lasso<-as.data.frame(pred_train_lasso)
  pred_test_lasso<-as.data.frame(pred_test_lasso)
  rmse_lasso_train<-rmse(pred_train_lasso,train_lasso$nevents)
  rmse_lasso_test<-rmse(pred_test_lasso,test_lasso$nevents)
  rmse_lasso_train
  rmse_lasso_test
  sst<-sum((train_lasso$nevents-mean(train_lasso$nevents))^2)
  sse<-sum((pred_train_lasso-train_lasso$nevents)^2)
  rsq<-1-sse/sst
  rsq
  sst<-sum((test_lasso$nevents-mean(test_lasso$nevents))^2)
  sse<-sum((pred_test_lasso-test_lasso$nevents)^2)
  rsq_test<-1-sse/sst
  rsq_test
  ridge_matrix=matrix(0,1,6)
  ridge_matrix[1,]=c(sed,rmse_mlr_train,rmse_mlr_test,rsq,rsq_test,features)
  df_compiled_lasso = rbind(df_compiled_lasso,ridge_matrix)
  coef_vec <- as.vector(coef(mdl_lasso_best)[-1])
  names(coef_vec) <- paste0("Feature", seq_along(coef_vec))
  coef_df <- as.data.frame(t(coef_vec))
  coef_df = data.frame(t(coef_df))
  coef_df['seed'] = sed
  coef_compiled_lasso = rbind(coef_compiled_lasso,coef_df)
}
names(df_compiled_lasso) <- c('seed','Rmse_train','Rmse_test','r_squared_train','r_squared_test','features')
names(coef_compiled_lasso) = c('coefficients','seed')
write.csv(df_compiled_lasso,'df_compiled_lasso.csv')
write.csv(coef_compiled_lasso,'coef_compiled_lasso.csv')
df_compiled_lasso





coef(mdl_lasso_best)

# need more processing for this 

# #Support vector regression
# set.seed(508)
# work_df = as.data.frame(scale(new_df[,(names(new_df) %in% feature_list)]))
# svr_datasort<-sample(1:nrow(work_df),nrow(work_df)*0.8)
# train_svr<-work_df[svr_datasort,]
# test_svr<-work_df[-svr_datasort,]
# train_svr_d<-data.frame(train_svr)
# descriptors_train_svr<-train_svr[,! names(train_svr) %in% c("grade")]
# descriptors_test_svr<-test_svr[,! names(test_svr) %in% c("grade")]
# descriptors_train_svr<-as.matrix(descriptors_train_svr)
# descriptors_test_svr<-as.data.frame(descriptors_test_svr)
# prop_train_svr<-train_svr$grade
# prop_test_svr<-test_svr$grade
# mdl_svr<-tune(svm,prop_train_svr~descriptors_train_svr,ranges=list(epsilon=seq(0,1,0.1),cost=1:10))
# BstModel<-mdl_svr$best.model
# summary(BstModel)
# #Update the regression model with the selections from BstModel (kernel, cost, gamma, epsilon)
# svmfit <- svm(train_svr$grade ~., data = train_svr, method="eps-regression",kernel = 'radial', cost = 3, gamma=0.1,epsilon=.3,scale=FALSE)
# pred_train_svr<-predict(svmfit, data=descriptors_train_svr)
# pred_test_svr<-predict(svmfit,newdata=descriptors_test_svr)
# rmse_SVR_train<-rmse(pred_train_svr,prop_train_svr)
# rmse_SVR_test<-rmse(pred_test_svr,prop_test_svr)
# rmse_SVR_train
# rmse_SVR_test
# sst<-sum((train_svr$mpg-mean(train_svr$mpg))^2)
# sse<-sum((pred_train_svr-train_svr$mpg)^2)
# rsq<-1-sse/sst
# rsq
# 
# #Gaussian Process Regression
# set.seed(508)
# datasort<-sample(1:nrow(work_df),nrow(work_df)*0.8)
# train_gpr<-work_df[datasort,]
# test_gpr<-work_df[-datasort,]
# descriptors_train_gpr<-train_gpr[,! names(train_gpr) %in% c("grade")]
# descriptors_test_gpr<-test_gpr[,! names(test_gpr) %in% c("grade")]
# mdl_gpr<-gausspr(descriptors_train_gpr,train_gpr$grade)
# pred_train_gpr<-predict(mdl_gpr,descriptors_train_gpr)
# pred_test_gpr<-predict(mdl_gpr,descriptors_test_gpr)
# rmse_gpr_train<-rmse(pred_train_gpr,as.matrix(train_gpr$grade))
# rmse_gpr_test<-rmse(pred_test_gpr,as.matrix(test_gpr$grade))
# rmse_gpr_train
# rmse_gpr_test
# sst<-sum((train_gpr$grade-mean(train_gpr$grade))^2)
# sse<-sum((pred_train_gpr-train_gpr$grade)^2)
# rsq<-1-sse/sst
# rsq


#Random Forest Regression
work_df = as.data.frame(new_df[,(names(new_df) %in% feature_list)])
empty_df_rf <- data.frame('data percent'= numeric(),'Rmse_train'= numeric(),'Rmse_test'= numeric(),'r_squared'=numeric())
# data_percent = seq(0.1,1,0.1)
data_p = 0.1
# for (data_p in data_percent){
  set.seed(508)
  datasort<-sample(1:nrow(work_df),nrow(work_df)*data_p)
  work_df_rf = work_df[datasort,]
  set.seed(508)
  datasort<-sample(1:nrow(work_df_rf),nrow(work_df_rf)*0.8)
  train_rf<-work_df_rf[datasort,]
  test_rf<-work_df_rf[-datasort,]
  model_rf<-randomForest(train_rf$nevents~.,data=train_rf,mtry=3,importance=TRUE,na.action=na.omit)
  # model_rf = readRDS("model_rf.rds")
  pred_train_rf<-predict(model_rf,train_rf)
  pred_test_rf<-predict(model_rf,newdata=test_rf)
  rmse_rf_train<-rmse(pred_train_rf,train_rf$nevents)
  rmse_rf_test<-rmse(pred_test_rf,test_rf$nevents)
  rmse_rf_train
  rmse_rf_test
  sst<-sum((train_rf$nevents-mean(train_rf$nevents))^2)
  sse<-sum((pred_train_rf-train_rf$nevents)^2)
  rsq<-1-sse/sst
  rsq
  sst_test<-sum((test_rf$nevents-mean(test_rf$nevents))^2)
  sse_test<-sum((pred_test_rf-test_rf$nevents)^2)
  rsq_test<-1-sse_test/sst_test
  rsq_test
  train_matrix=matrix(0,1,4)
  train_matrix[1,]=c(data_p,rmse_rf_train,rmse_rf_test,rsq)
  plot(model_rf,main=paste('RF',as.character(data_p)))
  empty_df_rf = rbind(empty_df_rf,train_matrix)
# }
# plot(model_rf)
write.csv(empty_df_rf,'rf_compiled_508.csv')
# saveRDS(model_rf, file = "model_rf.rds")
# model_rf

var_importance <- importance(model_rf)
var_importance

partialPlot(rf_model, iris, Petal.Width)

tree <- getTree(model_rf, k = 1)

# Plot first tree
plot(tree)

library(partykit)

party_model <- as.party(model_rf)

# Plot the trees of the party object
plot(party_model, type = "simple")



####Logistic Regression
prob_list = seq(0,1,0.1)
prob_list

for (prob in prob_list) { 
lim_fea_data=work_df[,c('certified','explored','ndays_act','nchapters','nevents','n_days','viewed')]
set.seed(508)
sort<-sample(1:nrow(lim_fea_data),nrow(lim_fea_data)*0.8)
train_logit<-lim_fea_data[sort,]
test_logit<-lim_fea_data[-sort,]
#Develop Logistic Regression Model on the Training Data
glm.fit <- glm(train_logit$certified ~ ., data = train_logit, family = binomial)
glm.probs_train <- predict(glm.fit,train_logit,type = "response")
glm.pred_train <- ifelse(glm.probs_train > prob, "1", "0")
#matrix
train_matrix=matrix(0,2,4)
rownames(train_matrix)=c(paste('Train','_',as.character(prob)), paste('Test','_',as.character(prob)))
colnames(train_matrix)=c('Accuracy','Sensitivity','Specificity','Probability')
#Assess prediction result as table as yes / no accuracy
misclass_train <- as.data.frame.matrix(table(glm.pred_train, truth = train_logit$certified))
train_matrix[1,1]=(misclass_train[1,1]+ misclass_train[2,2])/sum(misclass_train)
train_matrix[1,3]=misclass_train[2,2]/(misclass_train[2,2]+misclass_train[1,2])
train_matrix[1,2]=misclass_train[1,1]/(misclass_train[1,1]+misclass_train[2,1])
train_matrix[1,4]= prob 
#Apply model and repeat on training data
glm.probs_test <- predict(glm.fit,test_logit,type = "response")
glm.pred_test <- ifelse(glm.probs_test > prob, "1", "0")
misclass_test <- as.data.frame.matrix(table(glm.pred_test, truth = test_logit$certified))
train_matrix[2,1]=(misclass_test[1,1]+ misclass_test[2,2])/sum(misclass_test)
train_matrix[2,3]=misclass_test[2,2]/(misclass_test[2,2]+misclass_test[1,2])
train_matrix[2,2]=misclass_test[1,1]/(misclass_test[1,1]+misclass_test[2,1])
train_matrix[2,4]= prob
#coeff
round(coefficients(glm.fit),6)
empty_df = rbind(empty_df,train_matrix)
}

# empty_df <- data.frame('Accuracy'= numeric(),'Sensitivity'= numeric(),'Specificity'= numeric(),'probability'=numeric())

empty_df

