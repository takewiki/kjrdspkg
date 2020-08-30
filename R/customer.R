#' 更新发票信息
#'
#' @param conn 连接
#' @param phone 电话
#' @param custName 客户名称
#' @param bank  银行
#' @param acctNumber 账号
#' @param addr 地址
#' @param taxNo 税号
#' @param multiple  是否显示相似记录
#'
#' @return 返回值
#' @export
#'
#' @examples
#' fp_update()
fp_update <- function(conn=tsda::conn_rds('kj613'),
                      custName='北京健辰医疗器械有限公司',
                      bank='bankName',
                      acctNumber='acctNumber',
                      phone='phone',
                      taxNo='taxNo',
                      addr='addr',multiple=FALSE){
  custName <- tsdo::na_replace(custName,'')
  bank <-tsdo::na_replace(bank,'')
  acctNumber <- tsdo::na_replace(acctNumber,'')
  phone <-tsdo::na_replace(phone,'')
  taxNo <- tsdo::na_replace(taxNo,'')
  addr <- tsdo::na_replace(addr,'')
  if(multiple){
    #显示模拟匹配
    sql_1 <- paste0("
update a set  a.FINVOICETITLE ='",custName,"' ,a.FINVOICEBANKNAME ='",bank,"' ,a.FINVOICEBANKACCOUNT='",acctNumber,"',a.FINVOICETEL='",phone,"' from T_BD_CUSTOMER_F a
inner join  T_BD_CUSTOMER b
on a.fcustid = b.fcustid
inner join t_bd_customer_L c
on b.fcustid=c.fcustid
where c.fname like '",custName,"%'")
  }else{
    #显示精确匹配
    sql_1 <- paste0("
update a set  a.FINVOICETITLE ='",custName,"' ,a.FINVOICEBANKNAME ='",bank,"' ,a.FINVOICEBANKACCOUNT='",acctNumber,"',a.FINVOICETEL='",phone,"' from T_BD_CUSTOMER_F a
inner join  T_BD_CUSTOMER b
on a.fcustid = b.fcustid
inner join t_bd_customer_L c
on b.fcustid=c.fcustid
where c.fname ='",custName,"'")
  }

  #可以会出现，因此需要设置
  try({
    tsda::sql_update(conn,sql_1)
  })
  if(multiple){
    #模糊匹配
    sql2 <- paste0("update b set  b.FTAXREGISTERCODE='",taxNo,"',b.FINVOICEADDRESS ='",addr,"' from T_BD_CUSTOMER_F a
inner join  T_BD_CUSTOMER b
on a.fcustid = b.fcustid
inner join t_bd_customer_L c
on b.fcustid=c.fcustid
where c.fname like '",custName,"%'")
  }else{
    #精确确认
    sql2 <- paste0("update b set  b.FTAXREGISTERCODE='",taxNo,"',b.FINVOICEADDRESS ='",addr,"' from T_BD_CUSTOMER_F a
inner join  T_BD_CUSTOMER b
on a.fcustid = b.fcustid
inner join t_bd_customer_L c
on b.fcustid=c.fcustid
where c.fname ='",custName,"'")
  }


  try({
    tsda::sql_update(conn,sql2)
  })




}


#' 发票批理更新
#'
#' @param conn 连接
#' @param data 数据
#' @param show_process 是否显示进度条
#' @param multiple 发票是否模糊匹配
#'
#' @return 返回值
#' @export
#'
#' @examples
#' fp_updateBatch()
fp_updateBatch <- function(conn=tsda::conn_rds('kj613'),data,show_process =FALSE,multiple=FALSE) {
  ncount <- nrow(data)
  if(ncount>0){
    if(show_process){
      #显示进度条
      withProgress(message = '客户开票信息更新中', value = 0, {

        lapply(1:ncount, function(i){
          item <- data[i,]
          custName <- item$custName
          taxNo <- item$taxNo
          addr <- item$addr
          phone<-item$phone
          bank <- item$bank
          acctNumber <- item$acctNumber
          fp_update(conn=conn,
                    custName=custName,bank = bank,acctNumber = acctNumber,phone = phone,taxNo = taxNo,addr = addr,multiple = multiple
                    )
          incProgress(1/ncount, detail = paste("(",i,"/",ncount,")..."))



        })


      })
    }else{
      #不显示进度条
      lapply(1:ncount, function(i){
        item <- data[i,]
        custName <- item$custName
        taxNo <- item$taxNo
        addr <- item$addr
        phone<-item$phone
        bank <- item$bank
        acctNumber <- item$acctNumber
        fp_update(conn=conn,
                  custName=custName,bank = bank,acctNumber = acctNumber,phone = phone,taxNo = taxNo,addr = addr,multiple = multiple
        )


      })
    }

  }

}



#' 读取数据信息
#'
#' @param file 文件名
#' @param sheet 页签
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cust_getFpType()
cust_getFpType <- function(file="data-raw/专普票区分表-828-V2.xls",sheet = "专普",lang='cn'){
  #library(readxl)
  cust_fp_type<- readxl::read_excel(path = file,sheet = sheet)
  #字段筛选
  cust_fp_type <-cust_fp_type[,c( "名称", "票类", "开票要求")]
  #去重
  cust_fp_type <- unique(cust_fp_type)
  if(lang =='en'){
    names(cust_fp_type) <-c('FName','FFpType','FFpRequire')
  }
  return(cust_fp_type)


}

#' 上传数据
#'
#' @param conn 连接名
#' @param file 文件
#' @param sheet 页答
#'
#' @return 返回值
#' @export
#'
#' @examples
#' cust_uploadFpType()
cust_uploadFpType <-function(conn=conn_kjrds(),file="data-raw/专普票区分表-828-V2.xls",sheet = "专普"){

  data <- cust_getFpType(file=file,sheet = sheet,lang = 'en')
  ncount <-nrow(data)
  if(ncount>0){
    data$FUser <- 'RDS'
    data$FDate <-  as.character(Sys.Date())
    #print(head(data,10))
    #上传数据
    tsda::db_writeTable(conn=conn,table_name = 'rds_cust_fptype',r_object = data,append = T)
  }
}


#' 删除数据
#'
#' @param conn 连接信息
#'
#' @return 无返回值
#' @export
#'
#' @examples
#' cust_bakFpType()
cust_bakFpType <-function(conn=conn_kjrds()){
  #检查数据
  sql_sel <- paste0("select 1 from rds_cust_fptype")
  r1 <- tsda::sql_select(conn,sql_sel)
  ncount1 <- nrow(r1)
  if(ncount1 >0){
    #针对有数据的情况
    #备份旧的数据
    sql_bak <- paste0("insert into rds_cust_fptypeDel
select * from  rds_cust_fptype")
    tsda::sql_update(conn,sql_bak)
    #删除数据
    sql_del <-paste0("truncate table rds_cust_fptype")
    tsda::sql_update(conn,sql_del)
  }

}

#' 更新发票数据
#'
#' @param conn 连接
#'
#' @return 无返回值
#' @export
#'
#' @examples
#' cust_updateFpType()
cust_updateFpType <-function(conn=conn_kjrds()){
  sql_update <- paste0("update a set a.F_NLJ_BILLINGTYPE=isnull(c.FFpType,''),a.F_NLJ_BILLINGREQUIREMENT=isnull(c.FFpRequire,'')  from  T_BD_CUSTOMER a
inner join T_BD_CUSTOMER_L b
on a.FCUSTID = b.FCUSTID
inner join rds_cust_fptype  c
on b.FNAME = c.FName
where b.FLOCALEID =2052")
  tsda::sql_update(conn,sql_update)
}


#' 同步客户的发票数据
#'
#' @param conn 连接
#' @param file 文件
#' @param sheet 页答
#'
#' @return 无返回值
#' @export
#'
#' @examples cust_syncFpType()
cust_syncFpType <- function(conn=conn_kjrds(),file="data-raw/专普票区分表-828-V2.xls",sheet = "专普"){
  #删除原有数据
  cust_bakFpType(conn=conn)
  #上传新的数据
  cust_uploadFpType(conn=conn,file = file,sheet = sheet)
  #更新数据
  cust_updateFpType(conn=conn)

}





