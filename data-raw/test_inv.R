mydata <- inv_getMaterial()
View(mydata)



mydata2 <- inv_getRpt()
View(mydata2)



mydata3 <- inv_getRpt_filtered()
View(mydata3)

options(digits=15)
options(scipen=200)
res2 <- reshape2::dcast(data = mydata3,formula = FItemNumber+FItemName+FItemModel+FProdSpec+FHelpCode+FItemDesc+FStockUnitName+FSafeStockQty~FStockName,
                        fun.aggregate = sum,value.var = 'FInvQty' )
View(res2)



mydata4 <- inv_getRpt_cast()

View(mydata4)



mydata5 <- inv_getPOQty()
names(mydata5) <-c('物料编码','采购在途量')
mydata[ ,'采购在途量'] <- tsdo::na_replace(mydata[ ,'采购在途量'],0)
View(mydata5)

mydata6<- dplyr::left_join(mydata4,mydata5,by='物料编码')

View(mydata6)
