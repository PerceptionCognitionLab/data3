#Jeff Rouder, Local Data Extractor
extractData=function(d,code){
  stim=d$sender=="Stim"
  ntrials=table(d$observation[stim])
  codeTrials=rep(code,ntrials)
  targ=d[stim,]$targ
  back=d[stim,]$targ
  morph=d[stim,]$morph
  resp=d[stim,]$response
  rt=d[stim,]$duration
  out=data.frame(codeTrials,targ,back,morph,resp,rt)
  colnames(out)=c("code","targ","background","morph","resp","rt")
  return(out)}
