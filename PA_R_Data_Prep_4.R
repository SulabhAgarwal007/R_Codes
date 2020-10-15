

df = data.frame(read.csv("datasets/Data/Data/census_income.csv", stringsAsFactors = F))
View(df)

col_name = names(df)[-length(names(df))]
dummy_var_list = c()

for (c in col_name) {
  if (typeof(df[,c]) == "character" ){
    print(paste0("Number of Categories in ",c," : ", length(unique(df[,c]))))
    if(length(unique(df[,c]))<=6){
      dummy_var_list = append(dummy_var_list, c) # list of cat var with 6 or less categories
    }
  }
}

##---------------------Create Dummies

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}


for (var in dummy_var_list){
  df = CreateDummies(df, var)
}

  
##--------------Grouping of categories

# workclass
# round(prop.table(table(df$workclass, df$Y),1),1)
df$workclass = gsub(" ","",df$workclass)

df = df %>% mutate(wc_1 = as.numeric(df$workclass =='Self-emp-inc'),
              wc_2 = as.numeric(df$workclass =='Federal-gov'),
              wc_3 = as.numeric(df$workclass %in% c('Local-gov','Self-emp-not-inc','State-gov')),
              wc_4 = as.numeric(df$workclass =='Private'),
              wc_5 = as.numeric(df$workclass =='?')
              ) %>% select(-workclass)

# education
# round(prop.table(table(df$education, df$Y),1),1)
df$education = gsub(" ","",df$education)

df = df %>% mutate(edu_1 = as.numeric(df$education =='Masters'),
                   edu_2 = as.numeric(df$education =='Bachelors'),
                   edu_3 = as.numeric(df$education == 'Assoc-voc'),
                   edu_4 = as.numeric(df$education %in% c('Assoc-acdm','HS-grad','Some-college')),
                   edu_5 = as.numeric(df$education %in% c('10th','11th','12th','7th-8th','9th')),
                   edu_6 = as.numeric(df$education %in% c('1st-4th','5th-6th','Preschool'))
) %>% select(-education)

# marital.status
# round(prop.table(table(df$marital.status, df$Y),1),1)
df$marital.status = gsub(" ","",df$marital.status)
df = df %>% mutate(ms_1=as.numeric(df$marital.status %in% c('Divorced','Married-spouse-absent','Separated','Widowed')),
                   ms_2=as.numeric(df$marital.status %in% c('Never-married'))
                   ) %>% select(-marital.status)

# occupation
round(prop.table(table(df$occupation, df$Y),1),1)
df$occupation = gsub(" ","",df$occupation)

df = df %>% mutate(occ_1 = as.numeric(df$occupation =='Prof-specialty'),
                   occ_2 = as.numeric(df$occupation %in% c('Protective-serv','Sales','Tech-support')),
                   occ_3 = as.numeric(df$occupation %in% c('Craft-repair','Transport-moving')),
                   occ_4 = as.numeric(df$occupation %in% c('?','Adm-clerical','Armed-Forces','Farming-fishing','Handlers-cleaners','Machine-op-inspct')),
                   occ_5 = as.numeric(df$occupation %in% c('Other-service','Priv-house-serv'))
) %>% select(-occupation)


# native.country
round(prop.table(table(df$native.country, df$Y),1),1)
df$native.country = gsub(" ","",df$native.country)
df = df %>% mutate(nc_1 = as.numeric(df$native.country %in% c('?','Canada','China','Cuba','England','Germany','Greece','Hong','Italy','Philippines')),
                   nc_2 = as.numeric(df$native.country %in% c('Hungary','Ireland','Poland','Scotland','South','Thailand','United-States')),
                   nc_3 = as.numeric(df$native.country %in% c('Ecuador','El-Salvador','Haiti','Honduras','Jamaica','Laos','Mexico','Nicaragua','Peru','Portugal','Puerto-Rico','Trinadad&Tobago','Vietnam')),
                   nc_4 = as.numeric(df$native.country %in% c('Columbia','Dominican-Republic','Guatemala','Holand-Netherlands','Outlying-US(Guam-USVI-etc)'))
                   ) %>% select(-native.country)

##-----------------Flag Variables
# to convert those variable into flags which have high number of same values

sum(df$capital.gain==0)/nrow(df)
sum(df$capital.loss==0)/nrow(df)
sum(df$hours.per.week==40)/nrow(df)

df = df %>% mutate(cg_flag = as.numeric(capital.gain==0),
                   cl_flag = as.numeric(capital.loss==0),
                   Y = as.numeric(Y == ' >50K'))

write.csv(df, "census_income_modified.csv")
