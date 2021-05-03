extractData=function(d){
  stim=d$sender=="Screen"
  respEvent=d$sender=="Screen"
  sid=d$observation[stim]
  same=d[stim,]$same
  diff=d[stim,]$different
  resp=d[respEvent,]$response
  correct=d[respEvent,]$correct
  rt=d[respEvent,]$duration
  out=data.frame(sid,same,diff,resp,correct,rt)
  colnames(out)=c("sid",
                  "same",
                  "different",
                  "resp",
                  "correct",
                  "rt")
  return(out)}

