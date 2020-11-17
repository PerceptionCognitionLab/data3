link='https://raw.githubusercontent.com/PerceptionCognitionLab/data3/master/evenodd1/data'
dat=read.csv(url(link),head=T)
freq=table(dat$question,dat$resp)
prop=round(table(dat$question,dat$resp)/as.vector(table(dat$question)),2)

qtext=c('-1: evens male',
        '-1: evens preferred',
        '-1: evens lumpy',
        '-1: evens left',
        '-1: evens empire',
        '-1: evens symmetric',
        '-1: evens sweet',
        '-1: evens cool')
cbind(freq,qtext)
cbind(prop,qtext)

