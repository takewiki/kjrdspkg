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



mydata5 <- mydata4[,9:12]

mydata5 <- as.matrix(mydata5)



mydata6 <-addmargins(mydata5)
mydata6 <- as.data.frame(mydata6)

View(mydata6)
