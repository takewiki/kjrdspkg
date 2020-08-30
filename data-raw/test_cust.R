library(readxl)
cust_fp_type<- read_excel("data-raw/专普票区分表-828-V2.xls")
#字段筛选
cust_fp_type <-cust_fp_type[,c( "名称", "票类", "开票要求")]
#去重
cust_fp_type <- unique(cust_fp_type)
View(cust_fp_type)


mydata <- cust_getFpType(lang = 'en')
View(mydata)


cust_bakFpType()
cust_uploadFpType()
cust_updateFpType()

cust_syncFpType()
