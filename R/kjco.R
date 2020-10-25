#' 获取数据库链接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' conn_kjco()
conn_kjco <- function() {
  res <-tsda::conn_rds(db_name = 'kjco')
  return(res)

}



#' 生产成本数据
#'
#' @param conn  连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_prdCost_3Compare()
kjco_prdCost_3Compare <- function(conn=conn_kjco()){
  sql <- paste0("select * from rds_prdCost_compare3All
order by Fperiod,FLevel,FPrdItemNo,FMO_No,FMO_Entry")
  res <- tsda::sql_select(conn,sql)
  return(res)

}

#' 计算成本差异中文版
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_prdCost_3Compare_cn()
kjco_prdCost_3Compare_cn <- function(conn=conn_kjco()){

  data <- kjco_prdCost_3Compare(conn = conn)
  names(data) <-c('年月','生产订单号','生产订单行号',
                  '产品代码','材料成本-BOM口径','材料成本-生产用料清单口径','材料成本-生产领料单口径',
                  '材料成本差异-用料差异','材料成本差异-领料差异','当月产量','人工及制费金额',
                  '生产总成本-BOM口径','生产总成本-生产用料清单口径','生产总成本-生产领料单口径',
                  '生产总成本差异-用料差异','生产总成本差异-领料差异',
                  '单位生产成本-BOM口径','单位生产成本-生产用料清单口径','单位生产成本-生产领料口径',
                  '单位成本差异-用料差异','单位成本差异-领料差异','成本计算步骤'
                  )
  return(data)
}



#' 材料成本按生产领料单口径
#'
#' @param conn  连接
#' @param FPeriod 期间
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_materialCost_pick()
kjco_materialCost_pick <- function(conn=conn_kjco(),FPeriod='2020-03') {
  sql <- paste0("SELECT *  FROM [rds_deal_directMaterialAmt_vw]
  where FPeriod='",FPeriod,"'
order by Fperiod,FPrdLowCode,FMO_No,FMO_Entry")
  res <- tsda::sql_select(conn,sql)
  return(res)

}


#' 材料成本生产领料口径中文标题
#'
#' @param conn 连接
#' @param FPeriod 期间
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_materialCost_pick_cn()
kjco_materialCost_pick_cn <- function(conn=conn_kjco(),FPeriod='2020-03') {
  data <- kjco_materialCost_pick(conn = conn,FPeriod = FPeriod)
  names(data) <-c('年月','生产订单单号','生产订单行号',
                  '子项物料代码','领料数量','子项计算步骤','出库单价','领料金额','子项物料属性',
                  '父项物料代码','BOM版本','父项计算步骤')
  return(data)

}



#' 材料成本按BOM口径
#'
#' @param conn  连接
#' @param FPeriod 年月
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_materialCost_bom()
kjco_materialCost_bom <- function(conn=conn_kjco(),FPeriod='2020-03') {
  sql <- paste0("SELECT Fperiod,FMO_No,FMO_Entry,FSubItemNo,FPickQty,FLowCode,FPrice,FPickAmt,FItemProperty,FPrdItemNo,FBom_ver,FPrdLowCode
FROM [rds_deal_directMaterialAmt_bom_vw]
where Fperiod ='",FPeriod,"'
order by Fperiod,FPrdLowCode,FMO_No,FMO_Entry")
  res <- tsda::sql_select(conn,sql)
  return(res)

}


#' 材料成本生产领料口径中文标题
#'
#' @param conn 连接
#' @param FPeriod 年月
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_materialCost_bom_cn()
kjco_materialCost_bom_cn <- function(conn=conn_kjco(),FPeriod='2020-03') {
  data <- kjco_materialCost_bom(conn = conn,FPeriod = FPeriod)
  names(data) <-c('年月','生产订单单号','生产订单行号',
                  '子项物料代码','领料数量','子项计算步骤','出库单价','领料金额','子项物料属性',
                  '父项物料代码','BOM版本','父项计算步骤')
  return(data)

}

#' 材料成本按生产用料清单口径
#'
#' @param conn  连接
#' @param FPeriod 年月
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_materialCost_ppbom()
kjco_materialCost_ppbom <- function(conn=conn_kjco(),FPeriod='2020-03') {
  sql <- paste0("SELECT Fperiod,FMO_No,FMO_Entry,FSubItemNo,FPickQty,FLowCode,FPrice,FPickAmt,FItemProperty,FPrdItemNo,FBom_ver,FPrdLowCode
FROM [rds_deal_directMaterialAmt_ppbom_vw]
where Fperiod ='",FPeriod,"'
order by Fperiod,FPrdLowCode,FMO_No,FMO_Entry")
  res <- tsda::sql_select(conn,sql)
  return(res)

}


#' 材料成本按生产用料清单
#'
#' @param conn 连接
#' @param FPeriod 年月
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_materialCost_ppbom_cn()
kjco_materialCost_ppbom_cn <- function(conn=conn_kjco(),FPeriod='2020-03') {
  data <- kjco_materialCost_ppbom(conn = conn,FPeriod = FPeriod)
  names(data) <-c('年月','生产订单单号','生产订单行号',
                  '子项物料代码','领料数量','子项计算步骤','出库单价','领料金额','子项物料属性',
                  '父项物料代码','BOM版本','父项计算步骤')
  return(data)

}


#' 人工及制造费用
#'
#' @param conn 连接
#' @param FPeriod 期间
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_HumanManufacture_cost()
kjco_HumanManufacture_cost <- function(conn=conn_kjco(),FPeriod='2020-03') {
  sql <- paste0("SELECT   [Fperiod]
      ,[FWorkShop]
      ,[FMO_No]
      ,[FMO_Entry]
      ,[FBom_ver]
      ,[FItemNo]
      ,[FItemName]
      ,[FStockInQty]
      ,[FAcctName]
      ,[FFeeRate]
      ,[FFeeAmt]
  FROM [kjco].[dbo].[rds_deal_FeeAmt]
  where Fperiod ='",FPeriod,"'
  order by FWorkShop,FMO_No,FMO_Entry,FAcctName")
  res <- tsda::sql_select(conn,sql)
  return(res)

}

#' 人工及制造费用明细
#'
#' @param conn 连接
#' @param FPeriod 期间
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_HumanManufacture_cost_cn()
kjco_HumanManufacture_cost_cn <- function(conn=conn_kjco(),FPeriod='2020-03') {

  data <- kjco_HumanManufacture_cost(conn = conn,FPeriod = FPeriod)
  names(data) <-c('年月','车间','生产订单单号','生产订单行号',
                  'BOM版本','父项物料代码','父项物料名称','当月产量','工费科目明细','分配率',
                  '工费金额')
  return(data)
}




#' 材料出库成本按生产领料
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_deal_price_pick()
kjco_deal_price_pick <- function(conn=conn_kjco()) {
  sql <-paste0("select * from rds_deal_Price")
  res <- tsda::sql_select(conn,sql)
  names(res) <- c('年月','产品代码','物料属性','单位','出库单价')
  res_d <- reshape2::dcast(data = res,formula = '`产品代码`+`物料属性`+`单位`~`年月`',value.var = '出库单价')
  return(res_d)

}

#' 材料出库成本按BOM
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_deal_price_bom()
kjco_deal_price_bom <- function(conn=conn_kjco()) {
  sql <-paste0("select * from rds_deal_Price_bom")
  res <- tsda::sql_select(conn,sql)
  names(res) <- c('年月','产品代码','物料属性','单位','出库单价')
  res_d <- reshape2::dcast(data = res,formula = '`产品代码`+`物料属性`+`单位`~`年月`',value.var = '出库单价')
  return(res_d)

}

#' 材料出库成本按生产用料清单
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_deal_price_ppbom()
kjco_deal_price_ppbom <- function(conn=conn_kjco()) {
  sql <-paste0("select * from rds_deal_Price_ppbom")
  res <- tsda::sql_select(conn,sql)
  names(res) <- c('年月','产品代码','物料属性','单位','出库单价')
  res_d <- reshape2::dcast(data = res,formula = '`产品代码`+`物料属性`+`单位`~`年月`',value.var = '出库单价')
  return(res_d)

}

#' 材料成本计算单价
#'
#' @param conn 连接
#' @param method 方法
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_deal_price()
kjco_deal_price <-function(conn=conn_kjco(),method='生产领料单'){
  if (method == '生产领料单'){
    res <- kjco_deal_price_pick(conn = conn)
  }
  if (method == 'BOM'){
    res <- kjco_deal_price_bom(conn=conn)
  }
  if(method == '生产用料清单'){
    res <- kjco_deal_price_ppbom(conn=conn)
  }
  return(res)
}


#' 产品计算步骤异常查询
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' kjco_item_lowCode_error()
kjco_item_lowCode_error <- function(conn=conn_kjco()){
  sql <- paste0("select
FItemNo 产品代码,FItemProperty 物料属性,FUnit 计量单位
from rds_md_item_low_code
where FLowCode =0")
res <- tsda::sql_select(conn,sql)
return(res)
}
