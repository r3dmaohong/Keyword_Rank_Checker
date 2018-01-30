#'
#' @title Checking keyword and ad rankings positions in google
#' @author r3dmaohong
#' @keywords Rselenium, docker

#' @note It's just for 'ASSISTING' manual processing. 
#'       Maybe it will be detected as BOT.
#'       Therefore, I add many inefficient code blocks in the code...
#'       Nevertheless, I still can't guarantee that google won't detected it as BOT.

library(RSelenium)
library(rvest)
library(httr)
library(gmailr)
library(tcltk)
library(clipr)

setwd("workspace/datascience/r/crawler/keyword_rank_checker")

#########################
## machine name: default
#########################
#' docker-machine start default
#' docker run -d -p 4445:4444 selenium/standalone-chrome
#' docker ps
#' docker-machine ip
#' 

#' Settings for browser
######################## 
system('docker run -d -p 4445:4444 selenium/standalone-chrome')
remDr <- remoteDriver(browserName = "chrome",
                      remoteServerAddr = "localhost",
                      port = 4445L
                      )
########################

google_keyword_ad_checker <- function(remDr, dat, grepltext, ads, mail_from, mail_to){
  print('Start')
  google_start = Sys.time()
  
  output_google <- data.frame('query' = dat, 'ad' = "NA", 'rank' = "NA", stringsAsFactors = F)
  
  #' Stop the program when been detected as BOT by google.
  sorry_break <- FALSE
  
  #' Main program
  if(TRUE){
    for(i in 1:length(dat)){
      page  = 1
      query = dat[i]
      
      remDr$open(silent = TRUE)
      
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
        
        #rnd_down_times <- floor(runif(1, 2, 4))
        for(i_down_times in 1:3){#rnd_down_times){
          Sys.sleep(runif(1, 0.5, 5))
          webElem$sendKeysToElement(list(key = "page_down"))
        }
        
        status = F
        page_source <- remDr$getPageSource()
        html_source <- read_html(page_source[[1]])
        
        links <- html_source %>% html_nodes("._Rm") %>%  html_text()# %>% html_nodes(".srg ._Rm") %>%  html_text()
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
        
        # max pages
        if((toString(rank_links)=="" & page<3) | (ads==TRUE & page<3)){#page < 3){
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
      
      ##
      tmp <<- output_google
      
      Sys.sleep(runif(1, 10, 23))
      
      tryCatch({
        remDr$quit()
      }, error = function(e) {
        print("The browser is closed.")
      })
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
  
  dir.create('output', showWarnings = FALSE)
  google_fn <- paste0("output/google_", format(Sys.time(), "%Y_%m_%d_%H%M%S"), ".csv")
  write.csv(output_google, 
            google_fn, 
            row.names = F)
  
  google_end <- Sys.time()
  print(google_end - google_start)
  
  html_msg <- mime() %>%
    to(mail_to) %>%
    from(mail_from) %>%
    html_body("")
  file_attachment <- html_msg %>%
    subject("[noreply] Keyword Ranking Result in Google") %>%
    attach_file(google_fn)
  
  send_message(file_attachment)
  
  return(output_google)
}
###########

#' Import Keywords
( dat <- read_clip() )
#' Website's url
grepltext = ""

mail_from = '' 
mail_to = ''


## scheduled
times <- seq(as.POSIXct(paste0(Sys.Date(), " 08:50:00")), as.POSIXct(paste0(Sys.Date(), " 16:50:00")), by="hour")

x <- 1
while(x<=length(times)){
  if(Sys.time()>=times[x]){
    print( paste0(rep("===", 5), collapse = "") )
    print(paste0(x, '. Start at ', Sys.time()))
    result <- google_keyword_ad_checker(remDr, dat, grepltext, 
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
result <- google_keyword_ad_checker(remDr, dat, grepltext, 
                                    ads = TRUE,
                                    mail_from, 
                                    mail_to)

#' docker-machine stop default
#' docker-machine ls