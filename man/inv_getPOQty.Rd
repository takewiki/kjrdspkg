% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/invRpt.R
\name{inv_getPOQty}
\alias{inv_getPOQty}
\title{获取采购订单已审核未关闭未业务关闭的未入库数量}
\usage{
inv_getPOQty(conn = conn_kjrds(), FCompanyId = 100202)
}
\arguments{
\item{conn}{连接}

\item{FCompanyId}{公司代码}
}
\value{
返回值
}
\description{
获取采购订单已审核未关闭未业务关闭的未入库数量
}
\examples{
inv_getPOQty()
}
