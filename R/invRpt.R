#' 查询物料安全库存
#'
#' @param conn 连接
#' @param FCompanyId  公司内码
#'
#' @return 返回值
#' @export
#'
#' @examples
#' inv_getMaterial()
inv_getMaterial <- function(conn=conn_kjrds(),FCompanyId=100202){
  options(digits=15)
  options(scipen=200)


  sql <- paste0("select  FMasterId,FItemNumber,FItemName,FItemModel,FProdSpec,FHelpCode,FItemDesc,FStockUnitName,FSafeStockQty from rds_md_material
where FCompanyId =",FCompanyId)
  res <- tsda::sql_select(conn,sql)
  if(nrow(res)>0){
    res$FSafeStockQty <- round(FSafeStockQty,2)
  }
  return(res)
}




#' 获取即时库存报表
#'
#' @param conn 连接
#' @param FCompanyId 公司名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' inv_getRpt()
inv_getRpt <- function(conn=conn_kjrds(),FCompanyId=100202){
  options(digits=15)
  options(scipen=200)


  sql <- paste0("select  FItemNumber,FItemName,FItemModel,FProdSpec,FHelpCode,FItemDesc,FStockUnitName,FSafeStockQty,FStockName,FInvQty from  rds_stk_invRpt
where FCompanyId =",FCompanyId)
  res <- tsda::sql_select(conn,sql)
  return(res)
}


#' 针对即时库存报表按物料与仓库进行过滤
#'
#' @param conn 连接
#' @param FCompanyId 公司代码
#' @param filter_ItemNumber 物料代码向量
#' @param filter_stockName 仓库名称向量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' inv_getRpt_filtered()
inv_getRpt_filtered <- function(conn=conn_kjrds(),
                                FCompanyId=100202,
                                filter_ItemNumber=c('2.01.01.0001',
                                                    '2.01.01.0008',
                                                    '3.01.01.0001',
                                                    '3.01.01.0003',
                                                    '3.01.04.0001',
                                                    '3.01.04.0002',
                                                    '3.01.04.0003',
                                                    '3.01.04.0004',
                                                    '3.01.04.0005'
                                ),
                                filter_stockName=c('半成品仓库',
                                                   '原材料仓库',
                                                   '采血管车间库',
                                                   '辅料仓库'
                                )){
  r <- inv_getRpt(conn=conn,FCompanyId = FCompanyId)
  #print('1')
  #print(head(r))
  ncount <- nrow(r)
  if (ncount>0){
    #针对物料名称进行过滤
    r$FInvQty <- round(r$FInvQty,2)
    r <- r[r$FItemNumber %in% filter_ItemNumber, ]
    #print('2')
    #print(r)
    ncount <- nrow(r)
    if(ncount>0){
      #针对仓库进行过滤
      r<- r[r$FStockName %in% filter_stockName, ]
      #print('3')
      #print(head(r))
    }
  }
  return(r)
}



#' 针对即时库存报表进行处理
#'
#' @param conn 连接
#' @param FCompanyId 公司ID
#' @param filter_ItemNumber 物料代码
#' @param filter_stockName  物料名称
#'
#' @return 返回值
#' @export
#'
#' @examples
#' inv_getRpt_cast()
inv_getRpt_cast <- function(conn=conn_kjrds(),
                                FCompanyId=100202,
                                filter_ItemNumber=c('2.01.01.0001',
                                                    '2.01.01.0008',
                                                    '3.01.01.0001',
                                                    '3.01.01.0003',
                                                    '3.01.04.0001',
                                                    '3.01.04.0002',
                                                    '3.01.04.0003',
                                                    '3.01.04.0004',
                                                    '3.01.04.0005'
                                ),
                                filter_stockName=c('半成品仓库',
                                                   '原材料仓库',
                                                   '采血管车间库',
                                                   '辅料仓库'
                                )){
  data <- inv_getRpt_filtered(conn=conn,
                              FCompanyId = FCompanyId,
                              filter_ItemNumber = filter_ItemNumber,
                              filter_stockName = filter_stockName)
  ncount <- nrow(data)
  if(ncount>0){

    data <- reshape2::dcast(data = data,formula = FItemNumber+FItemName+FItemModel+FProdSpec+FHelpCode+FItemDesc+FStockUnitName+FSafeStockQty~FStockName,
                            fun.aggregate = sum,value.var = 'FInvQty' )
    data_cols <- names(data)
    col_fixed <-c('物料编码','物料名称','规格型号','产品要求','助记码','描述','单位','安全库存')
    col_var <-data_cols[9:length(data_cols)]
    col_all <- c(col_fixed,col_var)
    names(data) <- col_all
    data_inv <- data[,col_var]
    data_inv_mx <- as.matrix(data_inv)
    data_inv_mx<- addmargins(data_inv_mx)
    data_inv2 <- as.data.frame(data_inv_mx)
    inv_count <- nrow(data_inv2)-1
    sum_qty <-data_inv2$Sum[1:inv_count]
    data[,'指定仓库合计库存'] <- sum_qty
    data[,'库存差异值'] <- data[,'指定仓库合计库存']-data[,'安全库存']





  }

return(data)
}

