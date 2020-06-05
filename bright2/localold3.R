extractData=function(d,pid){
  stim=d$sender=="Stim"
  respEvent=d$sender=="Resp"
  
  ## new table function that does not sort
  getTable <- function(vec){
    unique_names <- unique(vec)
    table_r <- sapply(unique_names, function(x) sum(vec==x))
    names(table_r) <- unique_names
    return(table_r)
  }
  
  ## Get the participants who actually responded to every stimulus
  sessionID <- d$observation[stim] # stimulus session ID
  sessionID2 <- d[respEvent,]$observation # response session ID
  stimrespmatch <- match(getTable(sessionID), getTable(sessionID2))
  IDnotinResp <- which(is.na(stimrespmatch))
  goodsessID <- names(getTable(sessionID)[-IDnotinResp])
  sessionID.common <- sessionID[sessionID %in% goodsessID]
  
  ntrials <- getTable(sessionID.common)
  IDnotinStim <- is.na(match(pid[[2]], unique(sessionID)))
  partID <- pid[[1]][!duplicated(pid[[2]]) & !IDnotinStim] ##Remove participant ID if session ID is used double
  partID <- partID[-IDnotinResp]
  participantID <- rep(partID, ntrials)
  
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
  
  out= cbind(participantID, sessionID.common, stim.df, resp.df)
  colnames(out)=c("participantID",
                  "sessionID",
                  "target",
                  "background",
                  "morph",
                  "resp",
                  "rt")
  return(out)}
