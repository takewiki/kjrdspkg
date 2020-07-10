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
