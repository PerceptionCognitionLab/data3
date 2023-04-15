extractData=function(d,pid){
  stim=d$sender=="Stim"
  respEvent=d$sender=="Resp"
  sessionID=d$observation[stim]
  ntrials=table(sessionID)
  #  participantID=rep(pid,ntrials)
  targ=d[stim,]$targ
  back=d[stim,]$back
  morph=d[stim,]$morph
  resp=d[respEvent,]$response
  rt=d[respEvent,]$duration
  out=data.frame(
                 # participantID,
                 sessionID,targ,back,morph,resp,rt)
  colnames(out)=c(
                  # "participantID",
                  "sessionID",
                  "targ",
                  "background",
                  "morph",
                  "resp",
                  "rt")
  return(out)}
