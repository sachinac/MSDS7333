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

options(java.parameters = "- Xmx2048m")
i = 1

print(paste(Sys.time(),' Loading Data for Year ',' 1999'  ))

for(year in 1999:2013)
{ 
  print(paste(Sys.time(),' Loading Data for Year ',year ))
  df <- loadDF(year)
  write.table(df,"data/cherry_blossom.csv",sep = ';',append=TRUE)
#  df1 <- loadDF(year)
#  df  <- rbind(df,df1)
}

#print('Writing to Excel ',nrow(df))
#write.xlsx(df, "data/cherry_blossom.xlsx", sheetName='cs02',row.names = F, append = TRUE)
print('Process Completed ')




