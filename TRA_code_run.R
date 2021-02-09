rm(list=ls())
library(dplyr);library(httr);library(jsonlite);library(formattable)
library(rJava);library(RSelenium);library(stringr);library(rvest);library(data.table)
library(purrr)

source("TRA_function.R")

remDr$closeall()
remDr <- remoteDriver(remoteServerAddr="localhost",port=4445L,browserName="chrome") 
remDr$open()

#feedernet 접속
#feedernet<-"https://feedernet.com/member/login"
#api_navigate(remDr,feedernet)

#login(feedernet 접속)
#input
login_id<-'tech_support@evidnet.com'
login_pw<-'dpqlemspt!12' 
#login
login(remDr,login_id,login_pw)

#pop_up
# pop_up_remove(remDr)
# accept_remove(remDr)

#atlas_open
atlas_open(remDr,site_n=31)

#CC_import
#input
api<-read.csv(file="api.csv", header=T)
#select api
api<-api %>% filter(HCOs==1)
# api<-api %>% filter(no%in% c(17,18,33))
# api<-api %>% filter(!(no %in% c(18,21,29,30,33)))
api$no1<-as.numeric(rownames(api))+1

json_cc <- readLines("./input/json_cc.txt", encoding = "UTF-8" )
id_tmp<-CC_import(remDr,json_cc,api,n_cd=1)


#id
id<-get_id(remDr,api,n_cd=1)
name<-'T2DM_Test_210209'
saveRDS(id, paste0('./result_CC/id_',name,'.rds'))


#subgroup 
subgroup(remDr,api,id)

#CD_excution
CD_excution(remDr,api,id,n_cd=1)

#CC_excution
CC_excution(remDr,api,id)

#CD_view & merge 
result_list<-CD_view(remDr,api,id_tmp,name='PRS] T2DM')
result<-CD_merge(result_list,txt_person=9012799,txt_hospitals=9)

#CC_download
CC_download(remDr,api,id,delete=FALSE)

#CD_import
json_cd <- readLines("./input/json_cd.txt", encoding = "UTF-8" )
CD_import(remDr,json_cd,api,id,name='O_Any cancer_test')


#switch
# switch_navi(remDr,api,id)

#nave
# navi(remDr,api)


#####################
#CD_people
result1<-CD_people(remDr,api,id,name='ENCOVER - ENCOVER SOLUTION')
result2<-CD_people(remDr,api,id,name='HARMONILAN')
result3<-CD_people(remDr,api,id,name='Total Parenteral Nutrition')

result<-data.frame(result1$HCO_name,result1$`ENCOVER - ENCOVER SOLUTION`,result2$`HARMONILAN`,result3$`Total Parenteral Nutrition`)
write.csv(result,'./result.csv')



####################
#IR 
json_ir <- readLines("./input/json_ir.txt", encoding = "UTF-8" )

#IR_import
IR_id_tmp<-IR_import(remDr,api,json_ir)

#IR_get_id
IR_id<-IR_get_id(remDr,api)
name<-'T2DM_Test_210209'
saveRDS(IR_id, paste0('./result_IR/IR_id_',name,'.rds'))


#IR_generation
IR_generation(remDr,api,IR_id)

#IR_result
#input
api<-read.csv(file="api.csv", header=T)
#select api
api<-api %>% filter(HCOs==1)
api<-api %>% filter(!(no %in% c(29,30,33)))
api$no1<-as.numeric(rownames(api))+1



IR_id_2<-readRDS('./input/IR_id_2.rds')
IR_id_2<-IR_id_2[-c(16,19)]
IR_result(remDr,api,IR_id_2)



