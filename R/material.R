#' 读取物料的开票名称及开票规格型号
#'
#' @param file 文件
#' @param sheet 页签
#' @param lang 语言
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mtrl_getFpName()
mtrl_getFpName <- function(file="data-raw/2019年新K3开票名称-V3.XLS",sheet = "开票用最新",lang='cn'){
  #library(readxl)
  mtrl_fp_info <- readxl::read_excel(path = file,
                             sheet = sheet)
  mtrl_fp_info <- mtrl_fp_info[,c("新代码","开票名称","开票规格型号")]
  mtrl_fp_info <- unique(mtrl_fp_info)
  if(lang == 'en'){
    names(mtrl_fp_info) <-c('FNewNumber','FFpName','FFpModel')
  }
  return(mtrl_fp_info)

}

#' 物料的开票名称上传
#'
#' @param conn 连接
#' @param file 文件
#' @param sheet 页签
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mtrl_uploadFpName()
mtrl_uploadFpName <- function(conn=conn_kjrds(),file="data-raw/2019年新K3开票名称-V3.XLS",sheet = "开票用最新") {
   data <- mtrl_getFpName(file=file,sheet = sheet,lang = 'en')
   ncount <- nrow(data)
   if (ncount >0){
     data$FUser <- 'RDS'
     data$FDate <-  as.character(Sys.Date())
     #print(head(data,10))
     #上传数据
     tsda::db_writeTable(conn=conn,table_name = 'rds_mtrl_fpname',r_object = data,append = T)
   }

}

#' 备份物料上的开票物料名称及规格型号
#'
#' @param conn 连接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mtrl_bakFpName()
mtrl_bakFpName <-function(conn=conn_kjrds()){
  #检查数据
  sql_sel <- paste0("select 1 from rds_mtrl_fpname")
  r1 <- tsda::sql_select(conn,sql_sel)
  ncount1 <- nrow(r1)
  if(ncount1 >0){
    #针对有数据的情况
    #备份旧的数据
    sql_bak <- paste0("insert into rds_mtrl_fpnameDel
select * from  rds_mtrl_fpname")
    tsda::sql_update(conn,sql_bak)
    #删除数据
    sql_del <-paste0("truncate table rds_mtrl_fpname")
    tsda::sql_update(conn,sql_del)
  }

}

#' 更新物料发票信息
#'
#' @param conn 连接
#'
#' @return 无返回值
#' @export
#'
#' @examples
#' mtrl_updateFpName()
mtrl_updateFpName <-function(conn=conn_kjrds()){

  sql_update <- paste0(" update a set

F_NLJ_FFPNAME=isnull(b.FFpName,''),
F_NLJ_FFPMODEL = isnull(b.FFpModel,'')
from
T_BD_MATERIAL a
inner join rds_mtrl_fpname  b
on a.FNUMBER = b.FNewNumber")
  tsda::sql_update(conn,sql_update)
}


#' 物料同步
#'
#' @param conn 连接
#' @param file 文件
#' @param sheet 页签
#'
#' @return 返回值
#' @export
#'
#' @examples
#' mtrl_syncFpName()
mtrl_syncFpName <- function(conn=conn_kjrds(),file="data-raw/2019年新K3开票名称-V3.XLS",sheet = "开票用最新"){
  #删除原有数据
  mtrl_bakFpName(conn = conn)
  #上传新的数据
 mtrl_uploadFpName(conn = conn,file = file,sheet = sheet)
  #更新数据
 mtrl_updateFpName(conn = conn)
}

