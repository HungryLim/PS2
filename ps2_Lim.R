
a<-c(1245,925,1622,2101,42)
a<-as.matrix(a)


a<-c(1245,925,1622,2101,42,96541,548,69,48,874,8,9,9,9,9,999,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,99,9,9,9,9,9,9,9,9,9,2,1,1,1,2,12,12,5,33434)
a<-as.matrix(a)

a<-as.matrix(c(rep(9,10000)))

  
Leemis<-function(a){
  first<-substr(a[,1],1,1)
  tab<-table(first)
  tab<-as.data.frame(tab)
  dat<-as.data.frame(matrix(0, ncol=2, nrow=9))
  colnames(dat)<-c("first","freq")
  dat[,1]<-c(1:9)
  aa<-merge(dat,tab,by="first",all.x=T)
  aa<-subset(aa, select = -freq)
  for( i in 1:9){
    x_i<-aa[i,2]/nrow(a)
    aa[i,3]<-x_i - (log10(1+(1/i)))}  
    m<-max(aa[,3],na.rm = T)
    return(m)
}

Leemis(a)

cho_gains<-function(a){
  first<-substr(a[,1],1,1)
  tab<-table(first)
  tab<-as.data.frame(tab)
  dat<-as.data.frame(matrix(0, ncol=2, nrow=9))
  colnames(dat)<-c("first","freq")
  dat[,1]<-c(1:9)
  aa<-merge(dat,tab,by="first",all.x=T)
  aa<-subset(aa, select = -freq)
  for( i in 1:9){
    x_i<-(aa[i,2]/nrow(a))
    aa[i,3]<-x_i - (log10(1+(1/i)))}
    aa[,4]<-(aa[,3])^2
    d<-sum(aa[,4],na.rm = T)
    return(d)
}

cho_gains(a)



nn<-function(a,type){
  if(type=="cho_gains"){
    first<-substr(a[,1],1,1)
    tab<-table(first)
    tab<-as.data.frame(tab)
    dat<-as.data.frame(matrix(0, ncol=2, nrow=9))
    colnames(dat)<-c("first","freq")
    dat[,1]<-c(1:9)
    aa<-merge(dat,tab,by="first",all.x=T)
    aa<-subset(aa, select = -freq)
    for( i in 1:9){
      x_i<-(aa[i,2]/nrow(a))
      aa[i,3]<-x_i - (log10(1+(1/i)))}
    aa[,4]<-(aa[,3])^2
    d<-sum(aa[,4],na.rm = T)
    return(d)
    }
  if(type=="leemis"){
    first<-substr(a[,1],1,1)
    tab<-table(first)
    tab<-as.data.frame(tab)
    dat<-as.data.frame(matrix(0, ncol=2, nrow=9))
    colnames(dat)<-c("first","freq")
    dat[,1]<-c(1:9)
    aa<-merge(dat,tab,by="first",all.x=T)
    aa<-subset(aa, select = -freq)
    for( i in 1:9){
      x_i<-aa[i,2]/nrow(a)
      aa[i,3]<-x_i - (log10(1+(1/i)))}  
    m<-max(aa[,3],na.rm = T)
    return(m)
  }
  if(type=="both"){
    first<-substr(a[,1],1,1)
    tab<-table(first)
    tab<-as.data.frame(tab)
    dat<-as.data.frame(matrix(0, ncol=2, nrow=9))
    colnames(dat)<-c("first","freq")
    dat[,1]<-c(1:9)
    aa<-merge(dat,tab,by="first",all.x=T)
    aa<-subset(aa, select = -freq)
    for( i in 1:9){
      x_i<-aa[i,2]/nrow(a)
      aa[i,3]<-x_i - (log10(1+(1/i)))}  
    m<-max(aa[,3],na.rm = T)
    aa[,4]<-(aa[,3])^2
    d<-sum(aa[,4],na.rm = T)
    both<-cbind(m,d)
    colnames(both)<-c("Leemis","Cho_gains")
    return(both)}
}

nn(a,"leemis")
nn(a,"cho_gains")
nn(a,"both")

  
print.benfords<-function(a){
    t<-as.data.frame(nn(a,"both")) 
    k<-nn(a,"both")
    if (0.851 < k[1] & k[1] < 0.967){
      t$Leemis[1]<-paste(k[1],"*" )}
    if (0.967 <= k[1] & k[1] < 1.212){
      t$Leemis[1]<-paste(k[1],"**" )}
    if (1.212<= k[1] ){
      t$Leemis[1]<-paste(k[1],"***" )}
    if (1.212 < k[2] & k[2] <1.330){
      t$Cho_gains[1]<-paste(k[2],"*" )}
    if (1.330 <= k[2] & k[2] < 1.569){
      t$Cho_gains[1]<-paste(k[2],"**" )}
    if (1.569 <= k[2]){
      t$Cho_gains[1]<-paste(k[2],"***" )}

    print(t)
    cat("Note: P-value <0.01 ***, <0.05 **, <0.1 *")
}
print.benfords(a)


save<-function(a){
  out<-print.benfords(a)
  write.csv(out, file = "output2.csv")
}
save(a)



save<-function(a){
  sink(file="temp.csv")
  print.benfords(a)
  sink()
}
save(a)
