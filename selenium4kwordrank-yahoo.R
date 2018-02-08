library(RSelenium)
library(rvest)
library(httr)
library(gmailr)
library(tcltk)
library(clipr)


#' Import Keywords
( dat <- read_clip() )
#' Website's url
grepltext = ""

mail_from = '' 
mail_to = ''

#' YAHOO Checker
###########
yahoo_start = Sys.time()

system('docker run -d -p 4446:4444 selenium/standalone-chrome')
remDr <- remoteDriver(browserName = "chrome",
                      remoteServerAddr = "localhost",
                      port = 4446L
)

yahoo_keyword_ad_checker <- function(remDr, dat, grepltext, ads, mail_from, mail_to){
  print('Start')
  
  output_yahoo <- data.frame('query' = dat, 'ad' = "NA", 'rank' = "NA", stringsAsFactors = F)
  
  # Yahoo's ad got three places.
  ad_place = c("上", "右", "下")
  
  for(i in 1:length(dat)){
    remDr$open(silent = TRUE)
    remDr$deleteAllCookies()
    
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
      #links <- html_source %>% html_nodes("ol li .algo-sr span") %>%  html_text()
      links <- html_source %>% html_nodes(".options-toggle") %>%  html_text()
      
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
    tmp <<- output_yahoo
    
    print(i)
    print(query)
    print(output_yahoo$ad[i])
    print(output_yahoo$rank[i])
    Sys.sleep(runif(1,5,10))
    
    tryCatch({
      remDr$quit()
    }, error = function(e) {
      print("The browser is closed.")
    })
  }
  
  output_yahoo$ad[grepl("NA", output_yahoo$ad, fixed = T)]      <- "x"
  output_yahoo$rank[grepl("NA", output_yahoo$rank, fixed = T)]  <- "x"
  output_yahoo$ad[grepl("Inf", output_yahoo$ad, fixed = T)]     <- "x"
  output_yahoo$rank[grepl("Inf", output_yahoo$rank, fixed = T)] <- "x"
  
  
  
  dir.create('output', showWarnings = FALSE)
  yahoo_fn <- paste0("output/yahoo_", format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".csv")
  write.csv(output_yahoo, 
            yahoo_fn, 
            row.names = F)
  
  yahoo_end <- Sys.time()
  print(yahoo_end - yahoo_start)
  
  html_msg <- mime() %>%
    to(mail_to) %>%
    from(mail_from) %>%
    html_body("")
  file_attachment <- html_msg %>%
    subject("[noreply] Keyword Ranking Result in Yahoo") %>%
    attach_file(yahoo_fn)
  
  send_message(file_attachment)
}

## scheduled
times <- seq(as.POSIXct(paste0(Sys.Date(), " 08:50:00")), as.POSIXct(paste0(Sys.Date(), " 16:50:00")), by="hour")

x <- 1
while(x<=length(times)){
  if(Sys.time()>=times[x]){
    print( paste0(rep("===", 5), collapse = "") )
    print(paste0(x, '. Start at ', Sys.time()))
    result <- yahoo_keyword_ad_checker(remDr, dat, grepltext, 
                                        ads = TRUE,
                                        mail_from, 
                                        mail_to)
    print( paste0(rep("===", 5), collapse = "") )
    x <- x + 1
  }else{
    sleep_time <- ceiling((as.numeric(times[x] - Sys.time())*60))
    
    print( paste0(rep("===", 5), collapse = "") )
    print( paste0('Now: ', format(Sys.time(), '%Y-%m-%d %H:%M:%S')) )
    print( paste0('Wait for next schedule: ', format(times[x], '%Y-%m-%d %H:%M:%S')) )
    print(paste0('Sleep for about ', sleep_time, ' secs (', 
                 format(round(sleep_time/60, 2), nsmall = 2), ' mins).'))
    print( paste0(rep("===", 5), collapse = "") )
    Sys.sleep(sleep_time)
  }
}

## or 
result <- yahoo_keyword_ad_checker(remDr, dat, grepltext, 
                                    ads = TRUE,
                                    mail_from, 
                                    mail_to)
