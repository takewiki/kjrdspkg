library(readxl)
mtrl_fp_info <- read_excel("data-raw/2019年新K3开票名称-V3.XLS",
                               sheet = "开票用最新")
mtrl_fp_info <- mtrl_fp_info[,c("新代码","开票名称","开票规格型号")]
mtrl_fp_info <- unique(mtrl_fp_info)
View(mtrl_fp_info)



data_mt <- mtrl_getFpName(lang = 'en')
View(data_mt)


mtrl_uploadFpName()

mtrl_bakFpName()


mtrl_syncFpName()
