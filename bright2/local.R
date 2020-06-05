blockFun=function(x) ifelse (length(x)==5,as.integer(x[2]),-1)
trialFun=function(x) as.integer(ifelse(length(x)==5,x[4],x[3]))

extractData=function(d){
  resp=d$sender=="Resp"
  dat0=data.frame(d$observation,d$timestamp,d$sender,d$sender_id,d$targ,d$back,d$morph,d$response,round(d$duration))
  colnames(dat0)=c("sid","timestamp","sender","sender_id","targ","back","morph","resp","rt")
  datR=dat0[resp,]
  good=!duplicated(cbind(datR$timestamp,datR$sid))
  dat=datR[good,]
  index0=strsplit(as.character(dat$sender_id),"_")
  dat$block=unlist(lapply(index0,blockFun))+1
  dat$trial=unlist(lapply(index0,trialFun))+1
  return(dat)
}

