blockFun=function(x) as.integer(x[1])
trialFun=function(x) as.integer(x[2])

extractData=function(d){
  r=d$sender=="Resp"
  dat0=data.frame(d$observation,d$timestamp,d$sender_id,d$top_length,d$bottom_length,d$response,round(d$duration))
  colnames(dat0)=c("sid","timestamp","sender_id","top_length","bottom_length","resp","rt")
  datR=dat0[r,]
  good=!duplicated(cbind(datR$timestamp,datR$sid))
  dat=datR[good,]
  index0=strsplit(as.character(dat$sender_id),"_")
  dat$block=unlist(lapply(index0,blockFun))
  dat$trial=unlist(lapply(index0,trialFun))+1
  return(dat)
}

