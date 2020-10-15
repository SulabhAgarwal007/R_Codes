library(car)
library(ggplot2)
##--------------read data
df = data.frame(read.csv('census_income_modified.csv'))
df = select(df, -c(capital.gain, capital.loss))

##--------------split data into train and test
split = 0.75

set.seed(123)
r = sample(1:nrow(df), split*nrow(df))

df_train = df[r,]
df_test = df[-r,]

##------------Remove multicolinearity

lm_model = lm(Y~.-wc_4-relationship_Husband-edu_4-race_White-nc_2-ms_2, df_train)

sort(vif(lm_model), decreasing = T)[1:10]

log_model = glm(Y~.-wc_4-relationship_Husband-edu_4-race_White-nc_2-ms_2, df_train, family = 'binomial')

log_model = step(log_model)

summary(log_model)

log_model = glm(formula = Y ~ age + fnlwgt + education.num + hours.per.week + relationship_Wife + 
                  relationship_Unmarried + relationship_Own_child + relationship_Not_in_family + 
                  race_Amer_Indian_Eskimo + race_Asian_Pac_Islander + race_Black + 
                  sex_Male + wc_1 + wc_2 + wc_3 + wc_5 + edu_2 + edu_6 + ms_1 + 
                  occ_1 + occ_2 + occ_3 + occ_4 + occ_5 + nc_3 + nc_4 + cg_flag + 
                  cl_flag, family = "binomial", data = df_train)


summary(log_model)

df_train$score = predict(log_model, df_train, type='response')

ggplot(df_train, aes(x=score, y=Y, color=factor(Y)))+geom_point()+geom_jitter()

##---------------metrics calculations
cutoff = 0.3

TP = sum(as.numeric(df_train$Y==1 & df_train$score>=cutoff))
TN = sum(as.numeric(df_train$Y==0 & df_train$score<cutoff))
FP = sum(as.numeric(df_train$Y==0 & df_train$score>=cutoff))
FN = sum(as.numeric(df_train$Y==1 & df_train$score<cutoff))

Accuracy = (TP+TN)/(TP+TN+FP+FN)
Sn = TP/(TP+FN)
Sp = TN/(TN+FP)

Dist_var = sqrt((1-Sn)**2+(1-Sp)**2)

KS = Sn - FP/(TN+FP)
M = (9*FN+0.6*FP)/(1.9*(TP+TN+FP+FN))


       