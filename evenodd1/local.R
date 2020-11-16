extractData=function(d){

myQ=c('shape',
  'side',
  'sym',
  'temp',
  'taste',
  'starwars',
  'gender',
  'preference')

resp=c(d$shape[d$sender=="shape"],
       d$side[d$sender=="side"],
       d$sym[d$sender=="sym"],
       d$temp[d$sender=="temp"],
       d$taste[d$sender=="taste"],
       d$starwars[d$sender=="starwars"],
       d$gender[d$sender=="gender"],
       d$preference[d$sender=="preference"])

question=d$sender[d$sender %in% myQ]
sid=d$observation[d$sender %in% myQ]
pid=d$url_pid[d$sender %in% myQ]
dat=data.frame(sid,pid,question,resp)
return(dat)
}
