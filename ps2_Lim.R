#Lim
#Sorry I didn't clarify functions well.... Please let me know if I need to do a better job.
#Also please let me know if there is a better/ more efficient way to write these functions if you have a chance.
#Thanks!!!

#Make matrix
a<-as.matrix(c(1245,925,1622,2101,42,96541,548,69,48,874,8,9,9,9,9,999,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,99,9,9,9,9,9,9,9,9,9,2,1,1,1,2,12,12,5,33434))

a<-as.matrix(c(rep(9,10000)))

#1; write function
Leemis<-function(a){
  first<-substr(a[,1],1,1) # get the first number
  tab<-table(first)        # make a frequncy table
  tab<-as.data.frame(tab)  # change it as data frame to merge
  dat<-as.data.frame(matrix(0, ncol=2, nrow=9)) # make a data frame that has 1 to 9
  colnames(dat)<-c("first","freq")
  dat[,1]<-c(1:9)          # fill in numbers 
  aa<-merge(dat,tab,by="first",all.x=T) # merge the frequency table with pre-made data frame
  aa<-subset(aa, select = -freq) 
  for( i in 1:9){                       # a loop to calculate Leemis
    x_i<-aa[i,2]/nrow(a)
    aa[i,3]<-x_i - (log10(1+(1/i)))}  
    m<-max(aa[,3],na.rm = T)            #save Leemis in the third column of data frame that I made before.
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


#A function that has options
nn<-function(a,type){              # by using "type", I can choose whether I calculate Leemis, Chogains or both
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

#2; critical values
print.benfords<-function(a){                # I use if to give condition for thresholds and add * to the values in the data frame
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
    first_tab<-as.data.frame(table(substr(a[,1],1,1)))
    colnames(first_tab)<-c("number","frequency")
    print(first_tab)
    print(t)                                                # print rather than return to show note below
    cat("Note: P-value <0.01 ***, <0.05 **, <0.1 *")        # I put this note in the bottom of print (after print)
}
print.benfords(a)


#save data
save<-function(a,wd){                                          # use sink function to save outcomes as csv
  setwd(wd)                                                   # in some cases, I need to set working directory that I want to save data.
  sink(file="output.csv")
  print.benfords(a)
  sink()
}
save(a,"C:/Users/wooki/OneDrive/Music/Documents/GitHub/Class")
