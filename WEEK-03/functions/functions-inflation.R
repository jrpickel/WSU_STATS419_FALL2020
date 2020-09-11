inflation.data = function()
{
  # data from 1920 to 2020 ... 101 years ...
  infl = "https://www.officialdata.org/us/inflation/2000?endYear=1920&amount=1000000";
  # bigger dollar gives a more accurate percent ...
  
  # read the values in "year"/"dollars" using rvest ...
  library(rvest);	
  
  infl.html = read_html(infl);
  
  infl.table = infl.html %>%
    html_node(".expand-table-parent") %>%
    html_node("table")%>%
    html_table(header = TRUE);
  
  dollar = gsub('$','',infl.table$`Dollar Value`,fixed=T);
  dollar = gsub(',','',dollar,fixed=T);
  infl.table$`Dollar Value` = round(as.numeric(dollar),0)
  
  infl.rate = gsub('%','',infl.table$`Inflation Rate`,fixed=T);
  infl.table$`Inflation Rate` = infl.rate
  infl.table;
}


convertMillions = function(actor,baseYear)
{
  infl.table = inflation.data()
  index = match(actor$movies.50$year,infl.table$Year);
  CPI = infl.table[index,2];
  baseYearCPI = infl.table[which(infl.table$Year == as.numeric(baseYear)),2];
  new_dollar = (baseYearCPI/CPI)*actor$movies.50$millions;
  converted.year = as.character(baseYear)
  actor$movies.50$millions.2000 = new_dollar
  actor;
}