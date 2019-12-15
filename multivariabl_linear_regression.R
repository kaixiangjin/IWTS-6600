library(readxl)
brooklyn<-read_excel('E:/R/data/rollingsales_brooklyn.xls',skip=4)
a<-which(brooklyn$`LAND SQUARE FEET`!=0 & brooklyn$`GROSS SQUARE FEET`!=0 & brooklyn$`SALE
PRICE`!=0 & brooklyn$`SALE
PRICE`< 3000000)
new_data<-brooklyn[a,]
set.seed(1234)
index<-sample(1:nrow(new_data),100)
new<-new_data[index,]
fit<-lm(new$`SALE
PRICE`~new$`GROSS SQUARE FEET`+new$BLOCK)
plot(fit)
predict<-coef(fit)[2]*4000+coef(fit)[3]*5994+coef(fit)[1]
