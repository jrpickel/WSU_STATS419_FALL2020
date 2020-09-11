readFile = function(file)
  {
  df = read.table(file, 
                  header = TRUE,
                  sep = "|")
  df
  }


cleanData = function(df){
  df$V00 = NULL
  df$year = strftime(as.Date(df$date_test, format="%m/%d/%Y %H:%M"),format="%Y")
  df$week = strftime(as.Date(df$date_test, format="%m/%d/%Y %H:%M"),format="%V")
  df.order = df[order(df$year,df$week,decreasing = T),]
  dfClean = df.order[match(unique(df.order$md5_email),df.order$md5_email),]
  dfClean
}
