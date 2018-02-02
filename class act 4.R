
a<-c(1245,925,1622,2101,42)
a<-as.matrix(a)
a

Leemis<-function(a){
  first<-substr(a[,1],1,1)
  tab<-table(first)
  dat<-matrix(NA, ncol=1, nrow=9)
  for(i in 1:length(tab)){
    no<-as.numeric(tab[[i]])
    deno<-as.numeric(names(tab)[i])
    dat[i,1]<-(no/nrow(a)) - (log10(1+(1/deno)))  
    m<-max(dat[,1],na.rm=T)
  } 
  return(m)
}

Leemis(a)

cho_gains<-function(a){
  first<-substr(a[,1],1,1)
  tab<-table(first)
  dat<-matrix(0, ncol=1, nrow=9)
  for(i in 1:length(tab)){
    no<-as.numeric(tab[[i]])
    deno<-as.numeric(names(tab)[i])
    dat[i,1]<-(no/nrow(a)) - (log10(1+(1/deno))) 
    d<-sum((dat[,1])^2,na.rm=T)
  } 
  return(d)
}

cho_gains(a)



nn<-function(a,type){
  if(type=="cho_gains"){
      first<-substr(a[,1],1,1)
      tab<-table(first)
      dat<-matrix(0, ncol=1, nrow=9)
      for(i in 1:length(tab)){
        no<-as.numeric(tab[[i]])
        deno<-as.numeric(names(tab)[i])
        dat[i,1]<-(no/nrow(a)) - (log10(1+(1/deno))) 
        d<-sum((dat[,1])^2,na.rm=T)}
        return(d)
    }
  if(type=="leemis"){
    first<-substr(a[,1],1,1)
    tab<-table(first)
    dat<-matrix(NA, ncol=1, nrow=9)
    for(i in 1:length(tab)){
      no<-as.numeric(tab[[i]])
      deno<-as.numeric(names(tab)[i])
      dat[i,1]<-(no/nrow(a)) - (log10(1+(1/deno)))  
      m<-max(dat[,1],na.rm=T)} 
    return(m)
  }
  if(type=="both"){
    first<-substr(a[,1],1,1)
    tab<-table(first)
    dat<-matrix(0, ncol=2, nrow=9)
    for(i in 1:length(tab)){
      no<-as.numeric(tab[[i]])
      deno<-as.numeric(names(tab)[i])
      dat[i,2]<-(no/nrow(a)) - (log10(1+(1/deno))) 
      d<-sum((dat[,2])^2,na.rm=T)
      dat[i,1]<-(no/nrow(a)) - (log10(1+(1/deno)))  
      m<-max(dat[,1],na.rm=T)
      both<-cbind(m,d)
      colnames(both)<-c("Leemis","Cho_gains")}
    return(both)
  }
}

nn(a,"leemis")
nn(a,"cho_gains")
nn(a,"both")

print.benfords(a){
  k<-nn(a,"both")
  if (0.851 <= k[1] | k[1] < 0.967){
    k[1]<-paste(k[1],"*" )
  }
  if (k[1] <= 0.967){
    k[1]<-paste(k[1],"**" )
  }
  if (k[1] <= 1.212){
    k[1]<-paste(k[1],"***" )
  }
  if (k[2] <= 1.212){
    k[2]<-paste(k[2],"*" )
  }
  if (k[2] <= 1.330){
    k[2]<-paste(k[2],"**" )
  }
  if (k[2] <= 1.569){
    k[2]<-paste(k[2],"***" )
  }  
  return(k)
}

drfd





