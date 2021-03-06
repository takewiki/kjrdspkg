% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/customer.R
\name{fp_update}
\alias{fp_update}
\title{更新发票信息}
\usage{
fp_update(
  conn = tsda::conn_rds("kj613"),
  custName = "北京健辰医疗器械有限公司",
  bank = "bankName",
  acctNumber = "acctNumber",
  phone = "phone",
  taxNo = "taxNo",
  addr = "addr",
  multiple = FALSE
)
}
\arguments{
\item{conn}{连接}

\item{custName}{客户名称}

\item{bank}{银行}

\item{acctNumber}{账号}

\item{phone}{电话}

\item{taxNo}{税号}

\item{addr}{地址}

\item{multiple}{是否显示相似记录}
}
\value{
返回值
}
\description{
更新发票信息
}
\examples{
fp_update()
}
