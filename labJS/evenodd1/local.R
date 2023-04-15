extractData=function(d){


resp=c(d$shape[d$sender=="shape"],
       d$side[d$sender=="side"],
       d$sym[d$sender=="sym"],
       d$temp[d$sender=="temp"],
       d$taste[d$sender=="taste"],
       d$starwars[d$sender=="starwars"],
       d$gender[d$sender=="gender"],
       d$preference[d$sender=="preference"])

question=c(d$sender[d$sender=='shape'],
      d$sender[d$sender=="side"],
      d$sender[d$sender=="sym"],
      d$sender[d$sender=="temp"],
      d$sender[d$sender=="taste"],
      d$sender[d$sender=="starwars"],
      d$sender[d$sender=="gender"],
      d$sender[d$sender=="preference"])

sid=c(d$observation[d$sender=='shape'],
      d$observation[d$sender=="side"],
      d$observation[d$sender=="sym"],
      d$observation[d$sender=="temp"],
      d$observation[d$sender=="taste"],
      d$observation[d$sender=="starwars"],
      d$observation[d$sender=="gender"],
      d$observation[d$sender=="preference"])

dat=data.frame(sid,question,resp)
return(dat)
}
