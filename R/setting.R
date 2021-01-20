#' get the conn
#'
#' @return conn
#'
#' @examples
#' get_conn()
get_conn <- function() {
  #library(tsda)
  conn <- tsda::sql_conn_common(ip = "47.103.221.12",
                                port = 1433,
                                user_name = "sa",
                                password ="kangjian@2019" ,
                                db_name = "KJ2020")
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
