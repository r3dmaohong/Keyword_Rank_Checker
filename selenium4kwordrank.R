#'
#' @title Checking keyword and ad rankings positions in yahoo and google
#' @author r3dmaohong
#' @keywords Rselenium

#' @note It's just for 'ASSISTING' manual processing. 
#'       Maybe it will be detected as bot.
#'       Therefore, I add many inefficient code blocks for google checker...
#'       Nevertheless, I still can't guarantee that google won't detected as BOT.

## Run selenium server
# cmd
# java -jar selenium-server-standalone-2.53.1.jar
library(RSelenium)
library(rvest)
library(httr)
library(tcltk)

setwd("git/keyword_rank_checker")

#' Settings for browser
######################## 
remDr <- remoteDriver(remoteServerAddr = "localhost"
                      , port = 4444
                      , browserName ="firefox"
)
#remDr$open() #open browser
#remDr$getStatus()#check the status of browser

#https://stackoverflow.com/questions/40317920/how-to-clear-history-in-rseleniums-internal-web-browser
#https://stackoverflow.com/questions/33224070/how-to-open-incognito-private-window-with-selenium-wd-for-different-browser-type
fprof <- makeFirefoxProfile(
  list(
    "browser.cache.disk.enable" = FALSE,
    "browser.cache.memory.enable" = FALSE,
    "browser.cache.offline.enable" = FALSE,
    "network.http.use-cache" = FALSE,
    "browser.privatebrowsing.autostart" = TRUE
    
  )
)
remDr <- remoteDriver(extraCapabilities = fprof)
remDr$open()
########################

#' Import Keywords
dat <- readClipboard()

#' Website's url
grepltext = "hs.1111"

#' YAHOO Checker
###########
yahoo_start = Sys.time()

output_yahoo <- data.frame('query' = dat, 'ad' = "NA", 'rank' = "NA", stringsAsFactors = F)
# Yahoo's ad got three places.
ad_place = c("上", "右", "下")

for(i in 1:length(dat)){
  page  = 1
  query = dat[i]
  url   = paste0("https://tw.search.yahoo.com/search?fr=yfp-search-sb&p=", iconv(query, to="UTF-8"))
  
  remDr$navigate(url)
  status = T
  
  #remDr$deleteAllCookies()
  
  while(status){
    status = F
    page_source <- remDr$getPageSource()
    html_source <- read_html(page_source[[1]])
    
    # Yahoo is updating their website these days...
    #links <- html_source %>% html_nodes(".wr-bw") %>%  html_text()
    #links <- html_source %>% html_nodes(".fw-m") %>%  html_text()
    #links <- html_source %>% html_nodes(".compTitle") %>%  html_text()
    links <- html_source %>% html_nodes("ol li span") %>%  html_text()
    links <- links[grepl("[0-9a-zA-Z]+[.][0-9a-zA-Z]+", links)]
    
    if(length(links)!=10){
      tkmessageBox(title = "Warning!",
                   message = "Check the CSS, maybe it's updated!", 
                   icon = "warning", 
                   type = "ok") 
    }
    
    top    <- html_source %>% html_nodes(".searchCenterTopAds .layoutMiddle") %>%  html_text() # .compText
    right  <- html_source %>% html_nodes(".searchRightBottomAds .layoutMiddle") %>%  html_text() #.layoutCenter
    bottom <- html_source %>% html_nodes(".searchCenterBottomAds .layoutMiddle") %>%  html_text()
    
    rank_links    <- which(grepl(grepltext, links, fixed = T))
    rank_topad    <- which(grepl(grepltext, top, fixed = T)) %>% toString() %>% as.numeric()
    rank_rightad  <- which(grepl(grepltext, right, fixed = T)) %>% toString() %>% as.numeric()
    rank_bottomad <- which(grepl(grepltext, bottom, fixed = T)) %>% toString() %>% as.numeric()
    
    ad_pos <- c(rank_topad, rank_rightad, rank_bottomad)
    
    pos <- which(ad_pos == min(c(rank_topad, rank_rightad, rank_bottomad), na.rm = T))
    output_yahoo$query[i] = query
    
    if(grepl("NA", toString(output_yahoo$ad[i])) | grepl("Inf", toString(output_yahoo$ad[i]))){
      output_yahoo$ad[i] = paste0(page, "-", ad_place[pos][1], "-", ad_pos[pos[1]])
    }
    if(grepl("NA", toString(output_yahoo$rank[i]), fixed = T)){
      output_yahoo$rank[i] = paste0(page, "-", rank_links[1])
    }
    Sys.sleep(runif(1,3,6))
    
    if(page < 3){#(toString(rank_links)=="" & page<=6){
      status = T
      webElem <- remDr$findElement("css selector", ".next")
      remDr$mouseMoveToLocation(webElement = webElem) # move to the required element
      remDr$click() # left mouse button click
      page = page + 1 
    }else{
      status = F
    }
  }
  print(i)
  print(query)
  print(output_yahoo$ad[i])
  print(output_yahoo$rank[i])
  Sys.sleep(runif(1,5,10))
  
  #remDr$quit()
  #remDr$open()
}
output_yahoo$ad[grepl("NA", output_yahoo$ad, fixed = T)]      <- "x"
output_yahoo$rank[grepl("NA", output_yahoo$rank, fixed = T)]  <- "x"
output_yahoo$ad[grepl("Inf", output_yahoo$ad, fixed = T)]     <- "x"
output_yahoo$rank[grepl("Inf", output_yahoo$rank, fixed = T)] <- "x"

#' Making output as string when exporting to excel.
output_yahoo$rank <- paste0("'", output_yahoo$rank)
output_yahoo$ad   <- paste0("'", output_yahoo$ad)
write.csv(output_yahoo, 
          paste0("output/yahoo_", format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".csv"), 
          row.names = F)

yahoo_end = Sys.time()

yahoo_end - yahoo_start
###########



#' GOOGLE Checker
###########
google_start = Sys.time()

output_google <- data.frame('query' = dat, 'ad' = "NA", 'rank' = "NA", stringsAsFactors = F)

#' Stop the program when been detected as bot by google.
sorry_break <- FALSE

#' Main program
if(TRUE){
  for(i in 1:length(dat)){
    page  = 1
    query = dat[i]
    
    if(TRUE){#i == 1){
      remDr$navigate("https://www.google.com.tw/")
    }
    webElem_sch <- remDr$findElement(using = 'css',
                                     value = '#lst-ib')
    webElem_sch$clearElement()
    Sys.sleep(runif(1, 2, 5))
    webElem_sch$sendKeysToElement(list(query))
    Sys.sleep(runif(1, 1, 3))
    webElem_sch$sendKeysToElement(list(key = 'enter'))
    
    #url = paste0("https://www.google.com.tw/search?q=", iconv(query, to="UTF-8"))
    #remDr$navigate(url)# website to crawl
    status = T
    remDr$deleteAllCookies()
    
    while(status){
      #' Bot verification. Stop!
      if(grepl("sorry", unlist(remDr$getCurrentUrl()))){
        sorry_break = TRUE
        break
      }
      
      webElem <- remDr$findElement("css", "body")
      
      # I hate been detected as BOT...
      #rnd_down_times <- floor(runif(1, 2, 4))
      for(i_down_times in 1:3){#rnd_down_times){
        Sys.sleep(runif(1, 0.5, 5))
        webElem$sendKeysToElement(list(key = "page_down"))
      }
      
      status = F
      page_source <- remDr$getPageSource()
      html_source <- read_html(page_source[[1]])
      
      links <- html_source %>% html_nodes(".srg ._Rm") %>%  html_text()
      ad    <- html_source %>% html_nodes(".ads-ad") %>%  html_text() # .compText
      
      rank_links <- which(grepl(grepltext, links, fixed = T))
      rank_ad    <- which(grepl(grepltext, ad, fixed = T))
      
      output_google$query[i] = query
      
      if(grepl("NA", toString(output_google$ad[i])) | grepl("Inf", toString(output_google$ad[i]))){
        output_google$ad[i] = paste0(page, "-", min(rank_ad, na.rm = T))
      }
      if(grepl("NA", toString(output_google$rank[i]), fixed = T)){
        output_google$rank[i] = paste0(page, "-", rank_links[1])
      }
      Sys.sleep(runif(1, 5, 10))
      
      if(page < 3){#(toString(rank_links)=="" & page<=6){
        status = T
        webElem <- remDr$findElement("css selector", "#pnnext")
        remDr$mouseMoveToLocation(webElement = webElem) # move to the required element
        remDr$click() # left mouse button click
        page = page + 1 
      }else{
        status = F
        webElem$sendKeysToElement(list(key = "home"))
      }
    }
    if(sorry_break==TRUE){
      print("STOP! Been detected as BOT!")
      break
    }
    print(i)
    print(query)
    print(output_google$ad[i])
    print(output_google$rank[i])
    Sys.sleep(runif(1, 10, 23))
    
    remDr$quit()
    remDr$open()
  }
  
  
  ## Alert
  if(sorry_break==TRUE){
    tkmessageBox(title = "STOP!",
                 message = "Been detected as BOT!", 
                 icon = "warning", 
                 type = "ok")
    print(sprintf("i = %s", i))
  }
}

output_google$ad[grepl("NA", output_google$ad, fixed = T)]      <- "x"
output_google$rank[grepl("NA", output_google$rank, fixed = T)]  <- "x"
output_google$ad[grepl("Inf", output_google$ad, fixed = T)]     <- "x"
output_google$rank[grepl("Inf", output_google$rank, fixed = T)] <- "x"
output_google$rank <- paste0("'", output_google$rank)
output_google$ad   <- paste0("'", output_google$ad)
write.csv(output_google, 
          paste0("output/google_", format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".csv"), 
          row.names = F)

google_end = Sys.time()
google_end - google_start
###########
