extractData=function(d,pid){
  stim=d$sender=="Stim"
  respEvent=d$sender=="Resp"
  
  ## Get the participants who actually responded to every stimulus
  sessionID=d$observation[stim] # stimulus session ID
  sessionID2 <- d[respEvent,]$observation # response session ID
  stimrespmatch <- match(table(sessionID), table(sessionID2))
  goodsessID <- names(table(sessionID)[which(!is.na(stimrespmatch))])
  sessionID.common <- sessionID[sessionID %in% goodsessID]
  
  # ntrials=table(sessionID)
  # participantID=rep(pid, each = table(sessionID))
  
  # Get stimulus information
  targ=d[stim,]$targ
  back=d[stim,]$back
  morph=d[stim,]$morph
  stim.df <- data.frame(targ, back, morph)
  stim.df <- subset(stim.df, sessionID %in% goodsessID)
  
  # Get response information
  resp=d[respEvent,]$response
  rt=d[respEvent,]$duration
  resp.df <- data.frame(resp, rt)
  resp.df <- subset(resp.df, sessionID2 %in% goodsessID)
  
  out= cbind(sessionID.common, stim.df, resp.df)
  colnames(out)=c(
    #"participantID",
                  "sessionID",
                  "target",
                  "background",
                  "morph",
                  "resp",
                  "rt")
  return(out)}
