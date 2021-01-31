# General-purpose data wrangling
library(tidyverse)  
# Parsing of HTML/XML files  
library(rvest)    
# String manipulation
library(stringr)   
# Verbose regular expressions
library(rebus)     
# Eases DateTime manipulation
library(lubridate)
# Read/Write Excel
library(xlsx)



getURL <- function(year,page)
{
  return(paste0("http://www.cballtimeresults.org/performances?division=Overall+Women&page=",page,"&section=10M&sex=W&utf8=✓&year=",year))
}  


getPlayerRecord <- function(tab_txt, playerRecID)
{
  x_list <- strsplit(tab_txt[playerRecID], "[ ,]")[[1]]
  
  tokens <- c()
  token_i = 1
  for (i in 1:length(x_list))
  {
    if (x_list[i]=="")
    {
      if(x_list[i-1] !="")
        token_i = token_i + 1
    }
    else
    {
      if(i==1)
        tokens[token_i] = x_list[i]
      else
      if(x_list[i-1] == "")
          tokens[token_i] = x_list[i]
      else
       tokens[token_i] = paste(tokens[token_i],x_list[i])
    }
  } 
  
  new_row = c()
  
  r =1
  for (i in 1:length(tokens))
  {
     if (i==1)
     {
        x_list <- strsplit(tokens[i],"[ ]")[[1]]
        for (j in 1:length(x_list))
        {
           new_row[r] = x_list[j]
           r =  r + 1
        }
     }
     else
     if (i >= 9)   
     {
       if(is.na(new_row[r]) == TRUE)
         new_row[r] = tokens[i]
       else  
         new_row[r] = paste0(new_row[r],', ',tokens[i])
     }
     else
     {   
       new_row[r] = tokens[i]
       r = r + 1
     }
  }
   return (new_row)
}

loadDF <- function(year)
{  

  df <- data.frame(year=numeric(),race_category=character(),name=character(),age=numeric(),time=character(),Pace=character(),PiSTiS=character(),
                   Division=character(),PiDTiD=character(),Hometown=character(), page=numeric(),stringsAsFactors = FALSE)  
  page = 1
  while (1)
  {  
      simple  <- read_html(getURL(year,page))
      tab_txt <- simple %>% html_nodes("tr") %>% html_text() %>%  unlist() %>% 
                 str_replace_all("[\n]" , " ")  %>% str_replace_all("[ ]{2}+", " ")  %>% 
                 str_replace_all("^[ ]|[ ]$", "")
      
      if(length(tab_txt) < 2)
      {    
        print(paste0('Year ',year,' Pages ',page,' Total Records ',nrow(df)))
        break   
      }
      
      for (i in 2:length(tab_txt))
      {  
         new_row <- getPlayerRecord(tab_txt,i)
         df[nrow(df) + 1,] <- c(new_row,page)
      }         
         
      
      page  = page + 1
  }

  return(df)
}

web_scrape <- function()
{
   i = 1
   dfex <- data.frame()

   for(year in 1999:2020)
   { 
      print(paste(Sys.time(),' Loading Data for Year ',year ))
      df <- loadDF(year)
      write.table(df,"data/cherry_blossom_2020.csv",sep = ';',append=TRUE,col.names = if (i==1) T else F)
      dfex  <- rbind(dfex,df)
      i = i + 1
   }

   #print('Writing to Excel ',nrow(df))
   #write.xlsx(df, "data/cherry_blossom.xlsx", sheetName='cs02',row.names = F, append = TRUE)
   print('Process Completed ')
}


#library(writexl)
#write.xlsx(dfex,'data/cherry_blossom_2020.xlsx')

format_df <- function(df)
{  
  for (i in 1:nrow(df))
  {
    df$time[i] <- gsub("[^:0-9]","",df$time[i])
    split_time <- unlist(strsplit(df$time[i],":"))
    
    if(length(split_time)==3) 
    {
      df$mins[i] <- as.numeric(split_time[1])*60+as.numeric(split_time[2])+ as.numeric(split_time[3])/60
    }
    
    if(length(split_time)==2) 
    {
      df$mins[i] <- as.numeric(split_time[1])+ as.numeric(split_time[2])/60
    }   
    
    if(length(split_time)==1) 
    {
      df$mins[i] <- as.numeric(split_time[1])/60
    } 
    
    split_hometown <- unlist(strsplit(df$Hometown[i],","))
    
    if(length(split_hometown) > 1)
    {
      df$country[i] <- "USA"
    }
    else
    {
      if(nchar(split_hometown[1])>2)
      {    
        df$country[i] <- df$Hometown[i]
      }
      else
      {
        df$country[i] <- "USA"
      }
    }
    
    split_PiSTiS <- unlist(strsplit(df$PiSTiS[i],"/"))
    
    if(length(split_PiSTiS) == 2)
    {
       df$rankWomens[i]  <- split_PiSTiS[1]
       df$TotalWomens[i] <- split_PiSTiS[2]
    }
    else
    {
      df$rankWomens[i]  <- NA
      df$TotalWomens[i] <- NA
    }

    split_PiDTiD <- unlist(strsplit(df$PiDTiD[i],"/"))
    
    if(length(split_PiDTiD) == 2)
    {
      df$rankDivision[i]   <- split_PiDTiD[1]
      df$TotalDivisions[i] <- split_PiDTiD[2]
    }
    else
    {
      df$rankDivision[i]   <- NA
      df$TotalDivisions[i] <- NA
    }
    
  }  
  
  return (df)
}

process_df <- function(filename)
{
  
  df <- read.csv(filename,sep=';',row.names=NULL,stringsAsFactors = FALSE,header = T)  
  df <- format_df(df)
  
  return (df)
}  



#web_scrape()

#df <- read.csv('data/cherry_blossom_2020.csv',sep=';',row.names=NULL,stringsAsFactors = FALSE) 
#df <- format_df(df)
# write.table(df[,-c(1)],"data/cherry_blossom01_2020.csv",sep = ';',append=FALSE,col.names = T,row.names = F)

#df1 <- data.frame(A=1:10, B=LETTERS[1:10], D=rnorm(10) )
#df2 <- data.frame(A=11:20, D=rnorm(10), E=letters[1:10] )
#df3 <- data.frame(A=21:30, D=rnorm(10), E=letters[1:10] )


