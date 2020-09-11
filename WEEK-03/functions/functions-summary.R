doSummary = function(x)
{
  length_ = length(x);
  countNA = sum(is.na(x))
  mean_ = mean(x)
  median_ = median(x)
  NaiveVar = doSampleVariance(x,"naive")
  twoPassVar = doSampleVariance(x,"twoPass")
  stDev = sd(x)
  mode_ = doMode(x)
  result = data.frame(length = length_,countNA = countNA,mean=mean_,median=median_,NaiveVar = NaiveVar,twoPassVar=twoPassVar,stDev=stDev,mode=mode_)
  result;
}

doSampleVariance = function(x,method)
{
  if(method=="naive")
  {
    n = 0
    sum_ = 0
    sumSq = 0
    for(i in x){
      n = n + 1
      sum_ = sum_ + i
      sumSq = sumSq + i * i
    }
    variance = (sumSq-(sum_*sum_)/n)/(n-1)
    result = data.frame(sum_=sum_,sumSq=sumSq,variance=variance)
  }
  else
  {
    # two-pass algorithm
    n = 0
    sum1 = 0
    sum2 = 0
    for(i in x)
    {
      n = n + 1
      sum1 = sum1 + i
    }
    mean_ = sum1/n
    for(i in x)
    {
      sum2 = sum2 + (i-mean_)*(i-mean_)
    }
    variance = sum2 / (n-1)
    result = data.frame(sum = sum1,sum2 = sum2,variance = variance);
  }
  result;      
}

doMode = function(x)
{
  df = data.frame(table(x))
  df_sorted = df[order(df$Freq,decreasing = T),]
  most = df_sorted$Freq[which.max(df_sorted$Freq)]
  number = as.numeric(levels(df_sorted$x))[df_sorted$x]
  result = c()
  count_ = 0
  for (i in df_sorted$Freq)
  {
    count_ = count_ + 1
    if(i == most){
      result = append(result,number[count_])
    }
  }
  result;
}

retrieveTopRecord = function(df)
{
  dfMonte = df[which(df$md5_email=="b62c73cdaf59e0a13de495b84030734e"),]
  dataValues = as.numeric(dfMonte[1,-c(1,2,63,64)])
  dataValues
}