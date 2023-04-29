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
total_df = total_df[,!(names(total_df) %in% c("...1"))]
# total_df.drop("...1")
new_df = total_df
#outliers removal 
clean_df = total_df
clean_df = drop_na(clean_df)
#incomplete_flag
clean_df = clean_df[clean_df$incomplete_flag==0 ,]
#gender - o is being removed as we have only 4 observations
clean_df = subset(clean_df, clean_df$gender %in% c("m", "f"))
# viewed = 0 , explored = 1 
clean_df = clean_df[!(clean_df$viewed == 0 & clean_df$explored == 1),]
#Explored , n_chapters
# Assumpotion - atleast one of students accessed all chapters in coursework
Agg = aggregate(nchapters ~ course_id, data = clean_df, FUN = max)
Agg$chapter_limit = round(Agg$nchapters/2,0)
Agg[Agg$course_id==1,2]/2
Agg = Agg[,!(names(Agg) %in% c('nchapters'))]
clean_df = merge(clean_df,Agg, by = 'course_id')
clean_df =  clean_df[(clean_df$explored == 1 & clean_df$nchapters > clean_df$chapter_limit) | (clean_df$explored == 0 & clean_df$nchapters < clean_df$chapter_limit), ]
clean_df = clean_df[,!(names(clean_df) %in% c('chapter_limit'))]
#date 
clean_df = clean_df[(clean_df$last_event_DI >= clean_df$start_time_DI),]
#Grade, certified 
Agg1 = aggregate(grade ~ course_id + certified, data = clean_df, FUN = max )
Agg2 = aggregate(grade ~ course_id + certified, data = clean_df, FUN = min )
Agg_m = merge(Agg1,Agg2, by = c('course_id','certified') )
Agg_f = Agg_m[Agg_m$certified == 0 ,c('course_id','grade.x')]
clean_df = merge(clean_df,Agg_f, by = 'course_id')
clean_df = clean_df[(clean_df$certified == 0 & clean_df$grade <= clean_df$grade.x) | (clean_df$certified == 1 & clean_df$grade >= clean_df$grade.x),]
clean_df = clean_df[,!(names(clean_df) %in% c('grade.x'))]
#age >= 9  - can be implemented
# adding days_diff
clean_df$n_days = as.numeric(difftime(clean_df$last_event_DI, clean_df$start_time_DI, units = "days"))



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
