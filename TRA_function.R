###############################################################################

###login 
login <- function(remDr, login_id, login_pw){
  if(length(remDr$getSessions())==0){
    remDr$open(silent=FALSE)
  }
  
  remDr$navigate("https://feedernet.com/")
  Sys.sleep(1)
  
  #id input
  txt_login_id <- remDr$findElement(using='xpath', value='//*[@id="app"]/div/div[3]/div/div/form/div[2]/div/div[2]/div/div[2]/div[2]/div[1]/div  /input')
  txt_login_id$clickElement()
  writeClipboard(login_id)
  txt_login_id$sendKeysToElement(list(key = "control", "v"))
  
  #pw input
  txt_prs_password <- remDr$findElement(using="xpath",value='//*[@id="app"]/div/div[3]/div/div/form/div[2]/div/div[2]/div/div[2]/div[2]/div[2]/div/input')
  txt_prs_password$clickElement()
  writeClipboard(login_pw)
  txt_prs_password$sendKeysToElement(list(key = "control", "v"))
  
  #click
  tryCatch({remDr$findElement(using='xpath', value='//*[@id="app"]/div/div[3]/div/div/form/div[2]/div/div[2]/div/div[2]/div[2]/button')$clickElement()}, 
           error = function(e) print("error"))
  
  
  #pop_up_remove
  Sys.sleep(1)
  check<-tryCatch({remDr$findElement(using='xpath', value='/html/body/div[2]/div[2]/div/div/div[2]/div[3]/label')}, 
           error = function(e) print(NULL))
  if(!is.null(check)){check$clickElement()}
  check<-tryCatch({remDr$findElement(using='xpath', value='/html/body/div[2]/div[2]/div/div/div[3]/button')}, 
           error = function(e) print(NULL))
  if(!is.null(check)){check$clickElement()}
  
  #1개(에비드넷)의 아틀라스 접속
  remDr$navigate('https://api.feedernet.co.kr/atlas/v2.7.6/58/91/0/#/home')
  
  #accept
  btn(remDr,'/html/body/terms-and-conditions/atlas-modal/div/div/div/div[2]/div/div/div[3]/button[2]')$clickElement()
  remDr$goBack()
  remDr$refresh()
  
}


###pop_up_remove
pop_up_remove<-function(remDr){
  remDr$findElement(using='xpath',value='/html/body/div[2]/div[2]/div/div/div[2]/div[3]/label')$clickElement()
  remDr$findElement(using="xpath",value='/html/body/div[2]/div[2]/div/div/div[3]/button')$clickElement()
}


### xpath_btn function
btn <- function(remDr,xpath){
  btn_accept <- NULL
  while(is.null(btn_accept)){
    btn_accept <- tryCatch({remDr$findElement(using = "xpath", value= xpath)},
                           error=function(e){NULL})
    if(is.null(btn_accept)){Sys.sleep(1)}
  }
  return(btn_accept)
}


### xpath_btn function
btn2 <- function(remDr,xpath1,xpath2){
  btn_accept <- NULL
  while(is.null(btn_accept)){
    btn_accept <- tryCatch({remDr$findElement(using = "xpath", value= xpath1)},
                           error=function(e){NULL})
    if(is.null(btn_accept)){
      btn_accept <- tryCatch({remDr$findElement(using = "xpath", value= xpath2)},
                             error=function(e){NULL})}
    if(is.null(btn_accept)){Sys.sleep(1)}
  }
  return(btn_accept)
}


#accept_remove
accept_remove <- function(remDr){
  #1개(에비드넷)의 아틀라스 접속
  remDr$navigate('https://api.feedernet.co.kr/atlas/v2.7.6/58/91/0/#/home')
  
  #accept
  btn(remDr,'/html/body/terms-and-conditions/atlas-modal/div/div/div/div[2]/div/div/div[3]/button[2]')$clickElement()
  remDr$goBack()
  remDr$refresh()
}


###atlas_open
atlas_open<-function(remDr,site_n=2){
  #atlas 이동
  remDr$navigate('https://feedernet.com/Atlas')
  Sys.sleep(1)
  
  #n개 아틀라스 접속
  for(i in 1:site_n){
    btn(remDr, paste0('//*[@id="app"]/div/div[3]/div[2]/div/div/div[3]/div/div/div/div/div/div[2]/div[2]/div/div/div/table/tbody/tr[1]/td[5]/a/button'))$clickElement()
  }
  
}


### myswitch function
myswitch <- function (remDr, windowId){
  qpath <- sprintf("%s/session/%s/window", remDr$serverURL, 
                   remDr$sessionInfo[["id"]])
  remDr$queryRD(qpath, "POST", qdata = list(handle = windowId))
}



###characterization import
CC_import<-function(remDr,json_cc,api,n_cd=1){
  id<-data.frame();
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  #characterization > utilities
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    remDr$navigate(paste0(api$api[i-1],'/cc/characterizations/0/utilities'))
  }
  
  #characterization import 
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    
    HCO<-i-1
    CC_id<-c(); CD_id<-c();CD_name<-c();
    
    if(api$source_name[i-1]=='KBNUH_5.3.0_02'){
      CC_id <-append(CC_id, CC_id_tmp)
      if(n_cd==1){
        CD_id <-append(CD_id, CD_id_tmp1)
        CD_name <-append(CD_name, CD_name_tmp1)
      }else{
        CD_id <-append(CD_id, CD_id_tmp1)
        CD_id <-append(CD_id, CD_id_tmp2)
        CD_name <-append(CD_name, CD_name_tmp1)
        CD_name <-append(CD_name, CD_name_tmp2)
      }
      id_tmp<-data.frame(HCO,CC_id,CD_id,CD_name)
      id<-rbind(id,id_tmp)  
    }else{
    btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div[2]/ul/li[2]')$clickElement()
    txt_area <- btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div[2]/div[2]/import/div/div/textarea')
    txt_area$clearElement()
    txt_area$click()
    writeClipboard(json_cc)
    txt_area$sendKeysToElement(list(key = "control", "v"))
    remDr$findElement(using="xpath",value='//*[@id="currentComponent"]/div/tabs/div/div[2]/div[1]/div/div[2]/div[2]/import/div/div/button')$clickElement()
    Sys.sleep(2)
    


    check <- NULL
    count <- 0
    while(is.null(check) || count>25){
      Sys.sleep(1)
      count<-count+1
      Tbody<-remDr$getPageSource()[[1]]
      Tbody<-Tbody %>% read_html()
      
      tmp<-Tbody %>%
        html_nodes("heading-title") %>%
        html_text()
      
      ##데이터 전처리
      tmp<-str_replace_all(tmp,"\n","")
      tmp<-str_replace_all(tmp,"\t","")
      
      tmp<-strsplit(tmp,'#')[[1]][1]
      if(tmp=='Characterization '){check=1}
      
      if(count==10){remDr$refresh()}
    }
    
  
    Sys.sleep(1)
    body<-remDr$getPageSource()[[1]]
    body<-body %>% read_html()
    
    CC_id_tmp<-body %>%
      html_nodes("heading-title") %>%
      html_text()
    
    
    ##데이터 전처리
    CC_id_tmp<-str_replace_all(CC_id_tmp,"\n","")
    CC_id_tmp<-str_replace_all(CC_id_tmp,"\t","")
    
    
    CC_id_tmp<-strsplit(CC_id_tmp,'#')[[1]][2]
    
    if (length(CC_id_tmp) == 0) {
      CC_id <-append(CC_id, "check")
    } else {
      CC_id <-append(CC_id, CC_id_tmp)
    }
    
    
    
    if(n_cd==1){
      CD_id_tmp1<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.odd")%>% 
        html_nodes("td:nth-child(1)")%>%
        html_text()  
      
      if (length(CD_id_tmp1) == 0) {
        CD_id <-append(CD_id, "check")
      } else {
        CD_id <-append(CD_id, CD_id_tmp1)}  
      
      
      CD_name_tmp1<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.odd")%>% 
        html_nodes("td:nth-child(2)")%>%
        html_text() 
      
      if (length(CD_name_tmp1) == 0) {
        CD_name <-append(CD_name, "check")
      } else {
        CD_name <-append(CD_name, CD_name_tmp1)}  
      
      
    } else{ 
      CD_id_tmp1<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.odd")%>% 
        html_nodes("td:nth-child(1)")%>%
        html_text() 
      
      CD_id_tmp2<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.even")%>%
        html_nodes("td:nth-child(1)")%>%
        html_text() 
      
      if (length(CD_id_tmp1) == 0) {
        CD_id <-append(CD_id, "odd.check")
      } else {
        CD_id <-append(CD_id, CD_id_tmp1)}  
      
      if (length(CD_id_tmp2) == 0) {
        CD_id <-append(CD_id, "even.check")
      } else {
        CD_id <-append(CD_id, CD_id_tmp2)}   
      
      CD_name_tmp1<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.odd")%>% 
        html_nodes("td:nth-child(2)")%>%
        html_text() 
      
      CD_name_tmp2<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.even")%>%
        html_nodes("td:nth-child(2)")%>%
        html_text() 
      
      if (length(CD_name_tmp1) == 0) {
        CD_name <-append(CD_name, "odd.check")
      } else {
        CD_name <-append(CD_name, CD_name_tmp1)}  
      
      if (length(CD_name_tmp2) == 0) {
        CD_name <-append(CD_name, "even.check")
      } else {
        CD_name <-append(CD_name, CD_name_tmp2)}   
      
    }
    
    id_tmp<-data.frame(HCO,CC_id,CD_id,CD_name)
    id<-rbind(id,id_tmp)  
    }
  }
  id<-id %>% arrange(HCO,CD_name)
  saveRDS(id, './input/id.rds')
  return(id)
  
}

###get_id 
get_id<-function(remDr,api,n_cd=1){
  id<-data.frame();
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }

  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    
    HCO<-i-1
    CC_id<-c(); CD_id<-c();CD_name<-c();
   
    Sys.sleep(2)
    if(api$source_name[i-1]=='KBNUH_5.3.0_02'){
      CC_id <-append(CC_id, CC_id_tmp)
      if(n_cd==1){
        CD_id <-append(CD_id, CD_id_tmp1)
        CD_name <-append(CD_name, CD_name_tmp1)
        }else{
        CD_id <-append(CD_id, CD_id_tmp1)
        CD_id <-append(CD_id, CD_id_tmp2)
        CD_name <-append(CD_name, CD_name_tmp1)
        CD_name <-append(CD_name, CD_name_tmp2)
        }
      id_tmp<-data.frame(HCO,CC_id,CD_id,CD_name)
      id<-rbind(id,id_tmp) 
    }else{
    body<-remDr$getPageSource()[[1]]
    body<-body %>% read_html()
    
    CC_id_tmp<-body %>%
      html_nodes("heading-title") %>%
      html_nodes("div") %>%
      html_nodes("span") %>%
      html_text()
    
    CC_id_tmp<-strsplit(CC_id_tmp,'#')[[1]][2]
    
    if (length(CC_id_tmp) == 0) {
      CC_id <-append(CC_id, "check")
    } else {
      CC_id <-append(CC_id, CC_id_tmp)
    }
    
    
    
    if(n_cd==1){
      CD_id_tmp1<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.odd")%>% 
        html_nodes("td:nth-child(1)")%>%
        html_text()  
      
      if (length(CD_id_tmp1) == 0) {
        CD_id <-append(CD_id, "check")
      } else {
        CD_id <-append(CD_id, CD_id_tmp1)}  
      
      
      CD_name_tmp1<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.odd")%>% 
        html_nodes("td:nth-child(2)")%>%
        html_text() 
      
      if (length(CD_name_tmp1) == 0) {
        CD_name <-append(CD_name, "check")
      } else {
        CD_name <-append(CD_name, CD_name_tmp1)}  
      
      
    } else{ 
      CD_id_tmp1<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.odd")%>% 
        html_nodes("td:nth-child(1)")%>%
        html_text() 
      
      CD_id_tmp2<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.even")%>%
        html_nodes("td:nth-child(1)")%>%
        html_text() 
      
      if (length(CD_id_tmp1) == 0) {
        CD_id <-append(CD_id, "odd.check")
      } else {
        CD_id <-append(CD_id, CD_id_tmp1)}  
      
      if (length(CD_id_tmp2) == 0) {
        CD_id <-append(CD_id, "even.check")
      } else {
        CD_id <-append(CD_id, CD_id_tmp2)}   
      
      CD_name_tmp1<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.odd")%>% 
        html_nodes("td:nth-child(2)")%>%
        html_text() 
      
      CD_name_tmp2<-body %>%
        html_nodes("linked-cohort-list")%>% 
        html_nodes("tr.even")%>%
        html_nodes("td:nth-child(2)")%>%
        html_text() 
      
      if (length(CD_name_tmp1) == 0) {
        CD_name <-append(CD_name, "odd.check")
      } else {
        CD_name <-append(CD_name, CD_name_tmp1)}  
      
      if (length(CD_name_tmp2) == 0) {
        CD_name <-append(CD_name, "even.check")
      } else {
        CD_name <-append(CD_name, CD_name_tmp2)}   
      
      
    }
    
    id_tmp<-data.frame(HCO,CC_id,CD_id,CD_name)
    id<-rbind(id,id_tmp)  
    }
  }
  id<-id %>% arrange(HCO,CD_name)
  saveRDS(id, './input/id.rds')
  return(id)
  
}



###subgroup

subgroup<-function(remDr,api,id){
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  
  cc_id<-id %>% 
    group_by(HCO) %>% 
    filter(row_number()==1) 
  
  for(i in api$no1){
    
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    if(api$source_name[i-1]!='KBNUH_5.3.0_02')
      {
    remDr$navigate(paste0(api$api[i-1],'/cc/characterizations/',cc_id$CC_id[i-1]))
    
    #new subgroup
    a<-btn2(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/button','//*[@id="currentComponent"]/div/tabs/div/div[2]/div[2]/div/div/div[3]/button')
    a$clickElement()  
    a<-btn2(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/button','//*[@id="currentComponent"]/div/tabs/div/div[2]/div[2]/div/div/div[3]/button')
    a$clickElement()  

  
    
    ##Female
    # name
    txt <- remDr$findElement(using='xpath', value='//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[1]/div[1]/input')
    txt$clickElement()
    txt$clearElement()
    writeClipboard('Female')
    txt$sendKeysToElement(list(key = "control", "v"))
    
    # add criteria to group
    a<-btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[1]/div[4]/criteria-group/div/div[1]/table/tbody/tr/td[2]/drop-down-menu/div/button/span[2]')
    a$clickElement()
    
    
    # add demographic
    a<-btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[1]/div[4]/criteria-group/div/div[1]/table/tbody/tr/td[2]/drop-down-menu/div/ul/li[1]/a/div[2]')
    a$clickElement()
    
    
    # add attribute
    a<-btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[1]/div[4]/criteria-group/div/div[2]/div/table/tbody/tr/td[1]/div/div[2]/demographic-criteria/div/div/table/tbody/tr[1]/td/div/button')
    a$clickElement()
    
    # add gender criteria
    a<-btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[1]/div[4]/criteria-group/div/div[2]/div/table/tbody/tr/td[1]/div/div[2]/demographic-criteria/div/div/table/tbody/tr[1]/td/div/ul/li[2]/a/div[2]')
    a$clickElement()
    
    # Import
    a<-btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[1]/div[4]/criteria-group/div/div[2]/div/table/tbody/tr/td[1]/div/div[2]/demographic-criteria/div/div/table/tbody/tr[3]/td[2]/div/concept-list/concept-picker/button[2]')
    a$clickElement()  
    
    value <- vector()
    for(j in 1:99999){
      tryCatch(remDr$findElement(using="xpath",value=paste0('//*[@id="ui-id-',j,'"]/textarea')), error =function(e){print(j)})
      if(is.null(remDr$errorDetails()$message)) {value <- j ; break;}
    }
    
    import_concept <- remDr$findElement(using="xpath",value=paste0('//*[@id="ui-id-',value,'"]/textarea'))
    import_concept$clickElement()
    import_concept$sendKeysToElement(list("8532")) ; Sys.sleep(1)
    remDr$findElement(using="xpath",value=paste0('//*[@id="ui-id-',value,'"]/button'))$clickElement()
    Sys.sleep(1)  
    
    
    ##Male
    # name
    txt <- remDr$findElement(using='xpath', value='//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[2]/div[1]/input')
    txt$clickElement()
    txt$clearElement()
    writeClipboard('Male')
    txt$sendKeysToElement(list(key = "control", "v"))
    
    # add criteria to group
    a<-btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[2]/div[4]/criteria-group/div/div[1]/table/tbody/tr/td[2]/drop-down-menu/div/button/span[2]')
    a$clickElement()
    
    # add demographic
    a<-btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[2]/div[4]/criteria-group/div/div[1]/table/tbody/tr/td[2]/drop-down-menu/div/ul/li[1]/a/div[2]')
    a$clickElement()
    
    # add attribute
    a<-btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[2]/div[4]/criteria-group/div/div[2]/div/table/tbody/tr/td[1]/div/div[2]/demographic-criteria/div/div/table/tbody/tr[1]/td/div/button')
    a$clickElement()
    
    # add gender criteria
    a<-btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[2]/div[4]/criteria-group/div/div[2]/div/table/tbody/tr/td[1]/div/div[2]/demographic-criteria/div/div/table/tbody/tr[1]/td/div/ul/li[2]/a/div[2]')
    a$clickElement()
    
    # Import
    a<-btn(remDr,'//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div[3]/div[2]/div[2]/div[4]/criteria-group/div/div[2]/div/table/tbody/tr/td[1]/div/div[2]/demographic-criteria/div/div/table/tbody/tr[3]/td[2]/div/concept-list/concept-picker/button[2]')
    a$clickElement()  
    
    value <- vector()
    for(j in 1:99999){
      tryCatch(remDr$findElement(using="xpath",value=paste0('//*[@id="ui-id-',j,'"]/textarea')), error =function(e){print(j)})
      if(is.null(remDr$errorDetails()$message)) {value <- j ; break;}
    }
    
    import_concept <- remDr$findElement(using="xpath",value=paste0('//*[@id="ui-id-',value,'"]/textarea'))
    import_concept$clickElement()
    import_concept$sendKeysToElement(list("8507")) ; Sys.sleep(1)
    remDr$findElement(using="xpath",value=paste0('//*[@id="ui-id-',value,'"]/button'))$clickElement()
    Sys.sleep(1)   
    
    
    #save
    btn(remDr,'//*[@id="currentComponent"]/div/div/div[1]/div/button[1]')$clickElement()  
    Sys.sleep(1)
  }
  }
}



###CC_excution
CC_excution<-function(remDr,api,id){
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  cc_id<-id %>% 
    group_by(HCO) %>% 
    filter(row_number()==1) 
  
  #characterization > executions
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    remDr$navigate(paste0(api$api[i-1],'/cc/characterizations/',cc_id$CC_id[i-1],'/executions'))
    
    Sys.sleep(3)
    a<-btn(remDr,paste0('//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div/div/div/div/div/div[',api$db_no[i-1],']/div/div/ul/li[1]/button/span'))
    a$clickElement()
    Sys.sleep(2)
  }
  
}






###CD_excution
CD_excution<-function(remDr,api,id,n_cd=1){
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  for(k in 1:n_cd){
    
    cd_id<-id %>% 
      group_by(HCO) %>% 
      filter(row_number()==k)
    
    #cohort definition > executions
    for(i in api$no1){
      
      if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
      remDr$navigate(paste0(api$api[i-1],'/cohortdefinition/',cd_id$CD_id[i-1]))
      
      Sys.sleep(2)
      a<-btn(remDr,'//*[@id="currentComponent"]/div/div/ul/li[3]')
      a$clickElement()
      Sys.sleep(1)
      a<-btn(remDr,paste0('//*[@id="currentComponent"]/div/div/div[2]/div[4]/table/tbody/tr[',api$db_no[i-1],']/td[1]/span/span/button'))
      a$clickElement()
      
      Sys.sleep(2)
    }
  } 
}


###CD_import

CD_import<-function(remDr,json_cc,api,id,name){
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  cd_id<-id[grep(name,id$CD_name),]
  
  #cohort definition > export
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    remDr$navigate(paste0(api$api[i-1],'/cohortdefinition/',cd_id$CD_id[i-1]))
    
    a<-btn(remDr,'//*[@id="currentComponent"]/div/div/ul/li[5]')
    a$clickElement()
    a<-btn(remDr,'//*[@id="currentComponent"]/div/div/div[2]/div[3]/ul/li[3]')
    a$clickElement()
    
    
    txt <- remDr$findElement(using='xpath', value='//*[@id="cohortExpressionJSON"]')
    txt$clickElement()
    txt$clearElement()
    writeClipboard(json_cc)
    txt$sendKeysToElement(list(key = "control", "v"))
    
    #reload
    btn(remDr,'//*[@id="currentComponent"]/div/div/div[2]/div[3]/div/div[3]/div/div[2]/button')$clickElement()
    #save
    btn(remDr,'//*[@id="currentComponent"]/div/div/div[1]/div[1]/div/button[1]')$clickElement()
  }
}




###CD_view
CD_view<-function(remDr,api,id,name){
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  cd_id<-id[grep(name,id$CD_name),]
  result_list <-list()
  
  #cohort definition > generation
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    remDr$navigate(paste0(api$api[i-1],'/cohortdefinition/',cd_id$CD_id[i-1]))
    
    #generation
    a<-btn(remDr,'//*[@id="currentComponent"]/div/div/ul/li[3]')
    a$clickElement()
    
    btn(remDr,paste0('//*[@id="currentComponent"]/div/div/div[2]/div[4]/table/tbody/tr[',api$db_no[i-1],']/td[8]/div'))$clickElement()
    
    
    
    #button view
    view_xpath <- paste0('//*[@id="currentComponent"]/div/div/div[2]/div[4]/table/tbody/tr[',api$db_no[i-1],']/td[8]/div')
    btn_view <- btn(remDr,view_xpath)
    status_xpath<-paste0('//*[@id="currentComponent"]/div/div/div[2]/div[4]/table/tbody/tr[',api$db_no[i-1],']/td[3]/span')
    status <- btn(remDr,status_xpath)
    
    
    if(status$getElementText()[[1]]=='COMPLETE'){
      while(btn_view$isElementDisplayed()[[1]]==FALSE){}
      btn_view$clickElement()
      
      btn(remDr,'//*[@id="currentComponent"]/div/div/div[2]/div[4]/div[3]/div/div/ul/li[1]')$clickElement()
      Sys.sleep(1);
      
      #btn_switch
      btn_switch <- btn(remDr,'//*[@id="currentComponent"]/div/div/div[2]/div[4]/div[3]/div/div/div/div[2]/feasibility-report-viewer/div[1]/span')
      remDr$mouseMoveToLocation(webElement = btn_switch) # move to the required element
      if(btn_switch$getElementText()[[1]] != "Switch to intersect view"){
        remDr$click(0)
        Sys.sleep(1);
      }
      
      #table scrolling
      source <- remDr$getPageSource()
      html_source  <- source[[1]] %>% read_html() 
      table1 <- html_nodes(html_source, xpath = '//*[@id="currentComponent"]/div/div/div[2]/div[4]/div[3]/div/div/div/div[2]/feasibility-report-viewer/div[3]/feasibility-attrition-report/table/tbody/tr/td[1]/div/table') %>% html_table() %>% data.frame() %>% select(Matches, Total.Events)
      table2 <- html_nodes(html_source, xpath = '//*[@id="currentComponent"]/div/div/div[2]/div[4]/div[3]/div/div/div/div[2]/feasibility-report-viewer/div[3]/feasibility-attrition-report/div/table') %>% html_table() %>% data.frame() %>% select(Inclusion.Rule, N)
      table1 <- data.frame(Rule = names(table1), N = t(table1))
      colnames(table2) <- c("Rule", "N")
      table1$N <- as.character(table1$N)
      table2$N <- as.character(table2$N)
      
      
      if(nrow(table2)==0){
        table <- bind_rows(table1[2,], table1[1,])
      }else{
        table <- bind_rows(table1[2,], table2, table1[1,])
      }
      table[1,1] <- "Base population"
      result_table <- data.frame(no = seq(1,nrow(table),1), table)
      colnames(result_table) <- c("no","Rule",name)
      result_table$no <- as.numeric(result_table$no)
      result_table$Rule <- as.character(result_table$Rule)
      result_table[,3] <- as.character(result_table[,3])
      result_list[[i-1]] <- result_table
    }
  }
  return(result_list)
}


CD_merge<-function(result_list,txt_person=9012799,txt_hospitals=9){
  
  # site별 table merge
  not_null <- !unlist(lapply(result_list,is.null))
  result_list <- result_list[not_null] 
  data <- purrr::reduce(result_list, function(x,y){
    left_join(x,y,by=c("no","Rule"))
  })
  
  for(i in 3:ncol(data)) {
    data[,i] <- gsub(",","",as.character(data[,i])) %>% as.numeric()
  }
  Total <- data.frame(Total = apply(data[,3:ncol(data)],1,sum))
  HCOs  <- apply(data[,3:ncol(data)],1,function(x){sum(x>0)})
  result <- cbind(data,Total,HCOs) # %>% arrange(no, desc(Rule))
  last_result      <- result %>% select(no,Rule, Total, HCOs) 
  last_result$Rule <- last_result$Rule %>% as.character()
  last_result$Total <- last_result$Total %>% as.integer()
  last_result$HCOs <- last_result$HCOs %>% as.integer()
  
  first_result     <- data.frame(no=0, Rule = 'Network', Total = as.integer(txt_person), HCOs = as.integer(txt_hospitals))
  
  result <- rbind(first_result,last_result) %>% arrange(no)
  
  value <- vector()
  var <- result$Total
  for(i in 1:(nrow(result)-1)){
    value[i]<-(var[i+1]-var[i])/var[i]*100
  }
  Reduction_Rate <- value %>% as.character() %>% substr(1,6) %>% paste0("%")
  Reduction_Rate <- data.frame(Rate = c("",Reduction_Rate))
  funnel_table   <- data.frame(result[,-4], Reduction_Rate, HOCs=result[,4])
  funnel_table$Total <- as.integer(funnel_table$Total)
  funnel_table$Total <- comma(funnel_table$Total,digits = 0)
  funnel_table[1,"HOCs"] <- txt_hospitals
  result <-list()
  result[[1]]<-data
  result[[2]]<-funnel_table
  # result[[1]]<-DT::datatable(data)
  # result[[2]]<-DT::datatable(funnel_table)
  return(result)
  
}    




##characterization download

CC_download<-function(remDr,api,id,delete=True){
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  cc_id<-id %>% 
    group_by(HCO) %>% 
    filter(row_number()==1) 
  
  
  for(i in api$no1){
    
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    remDr$navigate(paste0(api$api[i-1],'/cc/characterizations/',cc_id$CC_id[i-1],'/executions'))
    
    #view latest result
    a<-btn(remDr,paste0('//*[@id="currentComponent"]/div/tabs/div/div[2]/div/div/div/div/div/div/div/div/div[',api$db_no[i-1],']/div/div/ul/li[2]'))
    a$clickElement()
    
  }
  
  # characterization - download
  Sys.sleep(2)
  
  root <- "Rtemp_CC"
  if(delete==True){unlink(root, recursive = TRUE)}
  
  # file.remove(root)
  if(!dir.exists(root)) dir.create(root)
  download_path <- file.path("C:", "Users", Sys.info()[["user"]], "Downloads")
  
  #down
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    
    source <- remDr$getPageSource()[[1]]
    export_table  <-   source %>% 
      read_html(encoding = "UTF-8") %>%
      html_nodes('button') %>% as.character()
    export_table  <- export_table[grep("DataTables_Table", export_table)] %>% as.list()
    
    list_name <- lapply(export_table,
                        function(x){
                          start_end <- str_locate(x,'DataTables_Table_\\d{1,10}')[1,]
                          name <- substr(x, start_end[1], start_end[2])
                        }
    )
    
    xpath <- paste0('//*[@id="',list_name,'_wrapper"]/div[1]/button')
    root1 <- paste(root, api$name[i-1], sep="/")
    if(!dir.exists(root1)) dir.create(root1)
    
    
    temp_folder <- root1
    for(j in 1:length(xpath)){
      btn_export <- btn(remDr,xpath[j])
      # btn_export <- remDr$findElement(using="xpath",value=)
      btn_export$clickElement()
      Sys.sleep(1)
      
      file_name <- list.files(download_path, pattern = "data \\W(\\d{1,10}\\W)")
      file_no <- sapply(file_name, function(x){
        x <- strsplit(x, split="\\)")[[1]][1]
        x <- strsplit(x, split="\\(")[[1]][2]
        return(x)
      })
      file_no <- as.numeric(file_no)
      max_no  <- max(file_no)
      temp_file <- paste0(download_path,"/data (",max_no,").csv")
      file.copy(temp_file,temp_folder)
      unlink(temp_file,recursive=TRUE)
      # file.remove(temp_file)
      file.rename(paste0(temp_folder,"/data (",max_no,").csv"), 
                  paste0(temp_folder,"/data (",max_no,")_",j,".csv")
      )
    }
  }
}



Rtemp_merge <- function(txt_title){
  path <- "./Rtemp_CC/"
  list_path <- as.list(list.dirs(path)[-1])
  result <-lapply(list_path, function(set_path){
    char <- strsplit(set_path, split="/")[[1]]
    dir_name <- char[length(char)]
    hospital_name <- dir_name
    file_name     <- paste0(set_path,"/",list.files(set_path))
    data          <- lapply(file_name, fread)
    change_data   <- lapply(data, function(Unit_data){
      Unit_data   <- Unit_data %>% mutate(Hospitals = hospital_name)
      if(any(colnames(Unit_data)=='Min')){
        Unit_data <- Unit_data %>% select('Hospitals', 'Cohort name', 'Analysis name', 'Covariate name', 'Strata name', 'Covariate ID', 'Count', 'Avg', 'StdDev', 'Min', 'P10', 'P25', 'Median', 'P75', 'P90', 'Max') %>% mutate(Percent = NA)
      }else{
        Unit_data <- Unit_data %>% select('Hospitals', 'Cohort name', 'Analysis name', 'Covariate name', 'Strata name', 'Covariate ID', 'Count','Percent') %>% mutate(Avg = NA, StdDev=NA,Min=NA, P10=NA,P25=NA,Median=NA,P75=NA,P90=NA,Max=NA)
      }
      return(Unit_data)
    })
    clean_data   <- lapply(change_data, function(Unit_data){
      Unit_data$`Analysis name` <- gsub('Demographics',"",Unit_data$`Analysis name`)
      Unit_data$`Analysis name` <- gsub('Long Term',"",Unit_data$`Analysis name`)
      Unit_data$`Analysis name` <- gsub('Medium Term',"",Unit_data$`Analysis name`)
      Unit_data$`Analysis name` <- gsub('Short Term',"",Unit_data$`Analysis name`)
      Unit_data$`Analysis name` <- trimws(Unit_data$`Analysis name`)
      Unit_data$`Cohort name`   <- substr(Unit_data$`Cohort name`,7,nchar(Unit_data$`Cohort name`))
      if(sum(grep("relative to index:",Unit_data$`Covariate name`[1]))==1){
        Unit_data <- tidyr::separate(data = Unit_data, col = `Covariate name`, sep="relative to index:", into = c("Period", "Concept Name"))
      }else{
        Unit_data <- Unit_data %>% mutate(Period = NA) %>% rename(`Concept Name` = `Covariate name`)
      }
      Unit_data$`Covariate ID` <- as.character(Unit_data$`Covariate ID`)
      Unit_data$`Covariate ID` <- substr(start = 1, stop = nchar(Unit_data$`Covariate ID`)-2.5, x= Unit_data$`Covariate ID`)
      Unit_data$`Concept Name` <- trimws(Unit_data$`Concept Name`)
      Unit_data <- Unit_data %>% rename(`Concept ID` = `Covariate ID`, Gender = `Strata name`, `Study Title` = `Cohort name`) %>%
        select('Hospitals','Study Title','Analysis name','Period','Gender','Concept ID','Concept Name', 'Count','Percent','Avg','StdDev','Min','P10','P25','Median','P75','P90','Max') %>%
        arrange(Hospitals, `Analysis name`, `Study Title`, Gender, desc(Count))
      # Unit_data$Percent <- as.double(Unit_data$Percent)
      # Unit_data$Percent <- if(!is.na(Unit_data$Percent)) round(Unit_data$Percent/100,2.5)
    })
  })
  data       <- reduce(result, bind_rows)
  # colnames(data) <- c("Hospitals","Study Title","Analysis name","Period","Gender","Concept ID","Concept Name","Count","Percent","Avg","StdDev","Min","P10","P25","Median","P75","P90","Max")
  root1 <- "./result_CC"
  root2 <- paste0(root1,"/",txt_title,gsub("-","",as.character(Sys.Date())),".csv")
  if(!dir.exists(root1)){dir.create(root1)}
  # data$Hospitals <- gsub("\\[DQM\\]","",data$Hospitals)
  # data$`Study Title` <- gsub(" \\([0-9]\\)","",data$`Study Title`)
  # # data_id<-read.csv("list.csv")
  # name_list <-unique(data$`Study Title`)
  # id_list <-c("000005","000001","000003","000006","000004","000002")
  # data_id <- data.frame(name = name_list, id= id_list) %>% arrange(id)
  # data_2 <- left_join(data, data_id, by=c("Study Title" = "name"))
  # data_3 <- data_2[,c(1,19,2:18)]
  
  # write.csv(data.frame(list = unique(data_3$Hospitals)),"list.csv")
  
  write.csv(data, root2, row.names = FALSE, na = "")
  
  # data_3 %>% select(Hospitals, id, Gender) %>% unique() %>% group_by(Hospitals, Gender) %>% count(n=n()) %>% data.frame()
  
  return(data)
}




##################################

###CD_people###
CD_people<-function(remDr,api,id,name){
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  cd_id<-id[grep(name,id$CD_name),]
  result<-c()
  
  
  #cohort definition > generation
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    remDr$navigate(paste0(api$api[i-1],'/cohortdefinition/',cd_id$CD_id[i-1]))
    
    #generation
    a<-btn(remDr,'//*[@id="currentComponent"]/div/div/ul/li[3]')
    a$clickElement()
    
    #button view
    people_xpath<-paste0('//*[@id="currentComponent"]/div/div/div[2]/div[4]/table/tbody/tr[',api$db_no[i-1],']/td[4]')
    people <- btn(remDr,people_xpath)
    status_xpath<-paste0('//*[@id="currentComponent"]/div/div/div[2]/div[4]/table/tbody/tr[',api$db_no[i-1],']/td[3]/span')
    status <- btn(remDr,status_xpath)
    
    if(status$getElementText()[[1]]=='COMPLETE'){
      result_tmp<-people$getElementText()[[1]]} else{ 
        result_tmp<-status$getElementText()[[1]] 
      }
    
    
    if (length(result_tmp) == 0) {
      result <-append(result, "check")
    } else {
      result <-append(result, result_tmp)
    }
    
    
  }
  f_result<-data.frame(api$name,result)
  colnames(f_result)<-c('HCO_name',name)
  return(f_result)
}



##################################

###Incidence rate###

IR_import<-function(remDr,api,json_ir){
  IR_id<-c()
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  
  #Incidence rate > utilities
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    remDr$navigate(paste0(api$api[i-1],'/iranalysis/new'))
  }
  
  #Incidence rate import 
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    if(api$source_name[i-1]=='KBNUH_5.3.0_02'){IR_id <-append(IR_id, IR_id_tmp)
    }else{
    
    btn(remDr,'//*[@id="currentComponent"]/div[1]/div[1]/ul/li[4]')$clickElement()
    txt_area <- btn(remDr,'//*[@id="currentComponent"]/div[1]/div[1]/div[2]/div[4]/div/div[1]/import/div/div/textarea')
    txt_area$clearElement()
    txt_area$click()
    writeClipboard(json_ir)
    txt_area$sendKeysToElement(list(key = "control", "v"))
    remDr$findElement(using="xpath",value='//*[@id="currentComponent"]/div[1]/div[1]/div[2]/div[4]/div/div[1]/import/div/div/button')$clickElement()
    Sys.sleep(2)
    
    HCO<-i-1
    
    check <- NULL
    count <- 0
    while(is.null(check) || count>25){
      Sys.sleep(1)
      count<-count+1
      Tbody<-remDr$getPageSource()[[1]]
      Tbody<-Tbody %>% read_html()
      
      tmp<-Tbody %>%
        html_nodes("heading-title") %>%
        html_text()
      
      ##데이터 전처리
      tmp<-str_replace_all(tmp,"\n","")
      tmp<-str_replace_all(tmp,"\t","")
      
      tmp<-strsplit(tmp,'#')[[1]][1]
      if(tmp=='Incidence Rate Analysis '){check=1}
    
      if(count==10){remDr$refresh()}
    }
    
    if(is.null(check)){IR_id <-append(IR_id, "check")
    }else{
    
    body<-remDr$getPageSource()[[1]]
    body<-body %>% read_html()
    
    IR_id_tmp<-body %>%
      html_nodes("heading-title") %>% 
      html_text()
    
    ##데이터 전처리
    IR_id_tmp<-str_replace_all(IR_id_tmp,"\n","")
    IR_id_tmp<-str_replace_all(IR_id_tmp,"\t","")
    
    
    IR_id_tmp<-strsplit(IR_id_tmp,'#')[[1]][2]
    
    if (length(IR_id_tmp) == 0) {
      IR_id <-append(IR_id, "check")
    } else {
      IR_id <-append(IR_id, IR_id_tmp)
    }
    }
    }
  }
  saveRDS(IR_id, './input/IR_id.rds')
  return(IR_id)
  
}


IR_get_id<-function(remDr,api){
  
  IR_id<-c()
  IR_target_cohort<-list()
  IR_outcome_cohort<-list()
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  
  #Incidence rate > utilities
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
  
  HCO<-i-1
  if(api$source_name[i-1]=='KBNUH_5.3.0_02'){IR_id <-append(IR_id, IR_id_tmp)}else{
    
  # body<-remDr$getPageSource()[[1]]
  # body<-body %>% read_html()
  # 
  # IR_id_tmp<-body %>%
  #   html_nodes("heading-title") %>% 
  #   html_text()
  # 
  # ##데이터 전처리
  # IR_id_tmp<-str_replace_all(IR_id_tmp,"\n","")
  # IR_id_tmp<-str_replace_all(IR_id_tmp,"\t","")
  
  test <- remDr$findElement(using="xpath",value='//*[@id="currentComponent"]/heading-title/div/span')
  IR_id_tmp<-unlist(test$getElementText())
    

  IR_id_tmp<-strsplit(IR_id_tmp,'#')[[1]][2]
  
  if (length(IR_id_tmp) == 0) {
    IR_id <-append(IR_id, "check")
  } else {
    IR_id <-append(IR_id, IR_id_tmp)
  }
  
  #target or outcome name 
  td_1 <- remDr$findElement(using="xpath",value='//*[@id="currentComponent"]/div[1]/div[1]/div[2]/div[1]/div/ir-analysis-editor/div/div[1]/table/tbody/tr[2]/td[1]/table')
  td_2 <- remDr$findElement(using="xpath",value='//*[@id="currentComponent"]/div[1]/div[1]/div[2]/div[1]/div/ir-analysis-editor/div/div[1]/table/tbody/tr[2]/td[2]/table')
  ir_target_name<-str_split(td_1$getElementText()[[1]],pattern = '\n')[[1]]
  ir_outcome_name<-str_split(td_2$getElementText()[[1]],pattern = '\n')[[1]]  
  
  IR_target_cohort[[i-1]]<-ir_target_name
  IR_outcome_cohort[[i-1]]<-ir_outcome_name
  
  }
  }

  saveRDS(IR_id, './input/IR_id.rds')
  saveRDS(IR_target_cohort,'./input/IR_target_cohort.rds' )
  saveRDS(IR_outcome_cohort,'./input/IR_outcome_cohort.rds' )

}





IR_generation<-function(remDr,api,IR_id){
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  #Incidence rate > utilities
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    remDr$navigate(paste0(api$api[i-1],'/iranalysis/',IR_id[i-1]))
    Sys.sleep(1)
    
    a<-btn(remDr,'//*[@id="currentComponent"]/div[1]/div[1]/ul/li[3]')
    Sys.sleep(1)
    a$clickElement()
    
    if(api$db_no[i-1]==1){
    btn(remDr,'//*[@id="currentComponent"]/div[1]/div[1]/div[2]/div[3]/div/ir-analysis-results/div[1]/div[2]/select-sources-btn/span/button/span')$clickElement()
    }else{
    btn(remDr,'//*[@id="currentComponent"]/div[1]/div[1]/div[2]/div[3]/div/ir-analysis-results/div[1]/div[2]/select-sources-btn/span/button/span')$clickElement()
      

    value <- vector()
    for(j in 1:99999){
      tryCatch(remDr$
                 findElement(using="xpath",value=paste0('//*[@id="DataTables_Table_',j,'"]/tbody/tr[2]/td[2]')), error =function(e){print(j)})
      if(is.null(remDr$errorDetails()$message)) {value <- j ; break;}
    }
    
    xpath <- remDr$findElement(using="xpath",value=paste0('//*[@id="DataTables_Table_',value,'"]/tbody/tr[',api$db_no[i-1],']/td[2]'))
    xpath$clickElement()
    
    
    btn(remDr,'//*[@id="currentComponent"]/div[1]/div[1]/div[2]/div[3]/div/ir-analysis-results/div[1]/div[2]/select-sources-btn/select-sources-popup/atlas-modal/div/div/div/div[2]/div/button')$clickElement()
    
  }
  }

  }  




##Incidence 
IR_down<-function(remDr,api,IR_id){
  # result_IR<-data.frame()
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  
  root <- "Rtemp_IR"
  unlink(root, recursive = TRUE)
  # file.remove(root)
  if(!dir.exists(root)) dir.create(root)
  download_path <- file.path("C:", "Users", Sys.info()[["user"]], "Downloads")
  
  #down
  for(i in api$no1){
    #Incidence rate > generation > export 
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    if(api$source_name[i-1]!='KBNUH_5.3.0_02'){
    remDr$navigate(paste0(api$api[i-1],'/iranalysis/',IR_id[i-1]))
    Sys.sleep(1)
    
    
    a<-btn(remDr,'//*[@id="currentComponent"]/div[1]/div[1]/ul/li[3]')
    Sys.sleep(1)
    a$clickElement()
    
    root1 <- paste(root, api$name[i-1], sep="/")
    unlink(root1, recursive = TRUE)
    if(!dir.exists(root1)) dir.create(root1)
    
    
    
    temp_folder <- root1
    check<-tryCatch({remDr$findElement(using='xpath', value='//*[@id="currentComponent"]/div[1]/div[1]/div[2]/div[3]/div/ir-analysis-results/div[1]/div[2]/button')}, 
                    error = function(e) print(NULL))
    
    if(!is.null(check)){
      check$clickElement()
    
    # btn(remDr,'//*[@id="currentComponent"]/div[1]/div[1]/div[2]/div[3]/div/ir-analysis-results/div[1]/div[2]/button')$clickElement()
    Sys.sleep(2)
    
    file_name <- list.files(download_path, pattern = paste0('incidence-rate-',IR_id[i-1]))
    
    file_no <- sapply(file_name, function(x){
      x <- strsplit(x, split="\\)")[[1]][1]
      x <- strsplit(x, split="\\(")[[1]][2]
      return(x)
    })
    
    if(length(file_no)==1){temp_file<-paste0(download_path,'/incidence-rate-',IR_id[i-1],".zip")}else{
      file_no <- as.numeric(file_no)
      max_no  <- max(file_no,na.rm=TRUE)
      temp_file <- paste0(download_path,'/incidence-rate-',IR_id[i-1]," (", max_no,").zip")
    }
    
    
    file.copy(temp_file,temp_folder)
    unlink(temp_file,recursive=TRUE)
    # file.remove(temp_file)
    if(length(file_no)>1){file.rename(paste0(temp_folder,'/incidence-rate-',IR_id[i-1]," (", max_no,").zip"), 
                                      paste0(temp_folder,'/incidence-rate-',IR_id[i-1],".zip"))}
    
    zipdata<-paste0(temp_folder,'/incidence-rate-',IR_id[i-1],".zip")
    unzip(zipdata,exdir=paste0("./",temp_folder))
    # data_tmp<-read.csv(file=paste0("./",temp_folder,'/ir_summary.csv'))
    # data_tmp<-data_tmp%>% mutate(HCOs=api$name[i-1], proportion = cases/total, rate=cases/timeAtRisk)
    # result_IR<-rbind(result_IR,data_tmp)
  }
  }
  }
  # write.csv(result_IR,'./result_IR/result_IR.csv')
  # return(result_IR)
}



IR_result<-function(api,IR_id,IR_target_cohort,IR_outcome_cohort){
  result_IR<-data.frame()
    
  for(i in api$no1){
    if(api$source_name[i-1]!='KBNUH_5.3.0_02'){
    root1 <- paste("Rtemp_IR", api$name[i-1], sep="/")
    # zipdata<-paste0(root1,'/incidence-rate-',IR_id[i-1],".zip")
    # unzip(zipdata,exdir=paste0("./",root1))
    data_tmp<-read.csv(file=paste0("./",root1,'/ir_summary.csv'))
    data_tmp<-data_tmp%>% mutate(HCOs=api$name[i-1], proportion = cases/total, rate=cases/timeAtRisk)
    
    #target_name
    ir_target_name<-IR_target_cohort[[i-1]]
    ir_target_name<-str_split(ir_target_name,pattern = ": ")
    
    a<-as.numeric(unlist(lapply(ir_target_name_id,function(x) gsub("#","",x[1]))))
    b<-unlist(lapply(ir_target_name_id,function(x)x[2]))
    ir_target_name<-data.frame(target_id=a,target_name=b)
    
    data_tmp<-left_join(data_tmp,ir_target_name,by=c('targetId'='target_id'))
    
    #outcome_name
    ir_outcome_name<-IR_outcome_cohort[[i-1]]
    ir_outcome_name<-str_split(ir_outcome_name,pattern = ": ")
    
    c<-as.numeric(unlist(lapply(ir_outcome_name,function(x) gsub("#","",x[1]))))
    d<-unlist(lapply(ir_outcome_name,function(x)x[2]))
    ir_outcome_name<-data.frame(outcome_id=c,outcome_name=d)

    data_tmp<-left_join(data_tmp,ir_target_name,by=c('outcomeId'='outcome_id'))
    
    
    result_IR<-rbind(result_IR,data_tmp)
    
  }
  }
  write.csv(result_IR,'./result_IR/result_IR.csv')
  return(result_IR)
  
}  




############################################################################
##switch_navi
switch_navi<-function(remDr,api,id){
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  cc_id<-id %>% 
    group_by(HCO) %>% 
    filter(row_number()==1) 
  
  #characterization > utilities
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    remDr$navigate(paste0(api$api[i-1],'/cc/characterizations/',cc_id$CC_id[i-1]))
  }
  
}


##navi
navi<-function(remDr,api,IR_id){
  
  # try to switch to new window
  check_handle <- FALSE
  count <- 0
  while(!check_handle || count > 20){
    count <- count + 1
    windows_handles <- remDr$getWindowHandles()
    if(length(windows_handles) < 2){
      Sys.sleep(1)
    }else{
      check_handle <- TRUE
    }
  }
  
  #characterization
  for(i in api$no1){
    if(check_handle==TRUE){myswitch(remDr,windows_handles[[i]])}else{print('error')}
    remDr$navigate(paste0(api$api[i-1],'/iranalysis/',IR_id[i-1]))
    
  }
  
}



