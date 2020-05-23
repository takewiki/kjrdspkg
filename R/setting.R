#' get the conn
#'
#' @return conn
#'
#' @examples
#' get_conn()
get_conn <- function() {
  #library(tsda)
  conn <- tsda::sql_conn_common(ip = "rm-uf6e13noj5yiye7s92o.sqlserver.rds.aliyuncs.com",
                                port = 3433,
                                user_name = "kangjian",
                                password ="kangjian@2019" ,
                                db_name = "AIS20191122134531")
  return(conn)


}
#' 获取数据库链接
#'
#' @return 返回值
#' @export
#'
#' @examples
#' conn_kjrds()
conn_kjrds <- function(){
  res <- get_conn()
  return(res)

}
