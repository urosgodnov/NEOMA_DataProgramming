library(RSelenium)
library(rvest)
library(data.table)

rmDr<-try(rsDriver(
  port = 4568L, browser="chrome",
  chromever = "109.0.5414.74"

), silent=TRUE)

remDr <- rmDr[["client"]]

getData <- function(year) {
  link <-
    paste0("https://www.boxofficemojo.com/year/",
           as.character(year),
           "/?ref_=bo_yl_table_2")
  remDr$navigate(link)
  currentPage <- remDr$getCurrentUrl()[[1]]
  
  #getting data
  data <- currentPage %>% read_html() %>%
    html_table() 
}



dataList=list()
i<-1
for (year in 1978:1992){
    #getting to a data page

    df<-
    try(dataList[i]<-getData(year) , silent=TRUE)
    
    i<-i+1
}

finalDf<-rbindlist(dataList, fill = TRUE)
