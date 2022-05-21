library(dplyr)
library(fastDummies)
library(tidymodels)
library(tidyr)

data_train<-read.csv("train.csv")
data_test<-read.csv("test.csv")

# preprocessing steps
preprocess=function(data){
data=data %>%
  mutate_at(
            vars(Alley,
                 BsmtQual,
                 BsmtCond,
                 BsmtExposure,
                 BsmtFinType2,
                 FireplaceQu,
                 GarageType,
                 GarageFinish,
                 GarageQual,
                 GarageCond,
                 PoolQC,
                 Fence,
                 MiscFeature),
      
            replace_na,replace="NON" ) %>%
  mutate_at(
             vars(MasVnrType,BsmtFinType1,Electrical),
             ~replace_na(.,mode(.)) ) %>%
  mutate_if(
             is.numeric,
             function(x){ 
               x=ifelse(is.na(x),replace(x,values=mean(x,na.rm=T)),x) #fill NA with mean
               x=(x-mean(x))/sd(x)  # normalization
              }
             ) %>%
  
  dummy_cols(remove_selected_columns = T)
  
  return(data)
}

#cleaning data_train
data_train=cbind(
                Id=data_train$Id, 
                preprocess(select(data_train,-c(SalePrice,Id))),
                SalePrice=data_train$SalePrice
                ) 
#cleaning data test
data_test=cbind(
                Id=data_test$Id,
                preprocess(select(data_test,-Id))
                )

#select common featurs in both data train and test
featursName=intersect(colnames(data_train),colnames(data_test))
data_train= cbind(data_train[featursName],SalePrice=data_train$SalePrice)



#removing outlires

model<-linear_reg() %>% set_engine("lm") %>% set_mode("regression")
dataModel<-data_train
Rsquared=0
movedFeaturs<-c()

while(TRUE){
  lm_fit<-fit(model,SalePrice~.-Id,data=dataModel)
  NewRsquared<-glance(lm_fit) %>% select(r.squared)
  NewRsquared<-as.numeric(NewRsquared[1])
  featurs<-tidy(lm_fit) %>% arrange(desc(p.value))
  if( NewRsquared>= Rsquared ){
    dataModel<-dataModel %>% select(-as.character(featurs[1,1]))
    movedFeaturs<-c(movedFeaturs,as.character(featurs[1,1]))
    Rsquared<-NewRsquared
    print(Rsquared)
  }else{
    if(length(movedFeaturs)>1){
      dataModel<-cbind(dataModel, select(data_train,as.character(movedFeaturs[length(movedFeaturs)])))
      movedFeaturs<-movedFeaturs[-1]
    }
    break
  }
}

which(is.na(data_test))
data_test=mutate_all(data_test,function(.){.=ifelse(is.na(.),mean(.,na.rm=T),.)})

result = predict(lm_fit, new_data =data_test) 
result=cbind(Id=data_test$Id,SalePrice=result)

View(result)



