blockFun=function(x) as.integer(x[1])
trialFun=function(x) as.integer(x[2])

extractData=function(d){
  resp=d$sender=="Resp"
  dat0=data.frame(d$observation,d$timestamp,d$sender_id,d$targ_level,d$back_level,d$response,round(d$duration))
  colnames(dat0)=c("sid","timestamp","sender_id","targLevel","backLevel","resp","rt")
  datR=dat0[resp,]
  good=!duplicated(cbind(datR$timestamp,datR$sid))
  dat=datR[good,]
  index0=strsplit(as.character(dat$sender_id),"_")
  dat$block=unlist(lapply(index0,blockFun))
  dat$trial=unlist(lapply(index0,trialFun))+1
  return(dat)
}

