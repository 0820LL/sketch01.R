args = commandArgs(TRUE)
num = length(args)
sample = list()
data = list()
myorder = list()  # sample data
mydepth=as.numeric(args[length(args)])
or = c(paste(paste(".",1:22,sep=""),".RData",sep=""),".X.RData")
for(i in 1:(num-3)){
  sample[[i]]= list.files(args[i])
  aa = list.files(args[i])
  temp=c()
  for(j in 1:length(or)){
    for(m in 1:length(aa)){
       if(length(grep(or[j],aa[m],fixed=T))==1){
          temp[j]=aa[m]
        }
     }
  }
 # print(temp)
  myorder[[i]] = temp
}
#print (myorder)

###load data
data = list()
data_num = list()
myname=c()
for(i in 1:(num-3)){
  data_num[[i]]=rep(0,23)
  for(j in 1:length(or)){
    aa = paste(args[i],myorder[[i]][j],sep="/")
    load(aa)
     if(j==1){ data[[i]] = RC;data_num[[i]][j]=length(RC) 
      myname[i]= strsplit(myorder[[i]][j], '.',fixed=T)[[1]][1]
      
     }else{ data[[i]] = c(data[[i]], RC);data_num[[i]][j]=length(RC)}
  }
  
}

### test data

###plot
name_jpeg=paste(paste(args[length(args)-2],args[length(args)-1],sep="/"),"_depth.jpeg",sep="")

jpeg(name_jpeg,width = 1280, height = 200*(num-3))
col= rainbow(num-3)
par(mar=c(5,5,3,2),font=2,cex.axis=1.2,font.lab=2,font.axis=2,cex.lab=1.2)
plot(data[[1]], pch=20, type='h', ylim=c(0,mydepth*(num-3)), col=col[1], yaxt='n', ylab='Depth (X)', xlab='Bin', xaxs='i')
for(i in 2:(num-3)){
  lines(data[[i]]+(i-1)*mydepth,col=col[i])
  
  for(j in 3:length(data[[i]])){
    polygon(x=c(j-2,j-2,j-1,j,j),y=c((i-1)*mydepth, data[[i]][j-2]+(i-1)*mydepth, data[[i]][j-1]+(i-1)*mydepth, data[[i]][j]+(i-1)*mydepth ,(i-1)*mydepth),col=col[i], border = col[i])
  }  
}
chr=rep(0,23)
for(i in 1:23){
  if(i==1){chr[i] = data_num[[1]][1]}else{ chr[i]=chr[i-1]+data_num[[1]][i] }
}
 
for(i in 1:23){
  abline(v=chr[i],lty=2)
  if(i==1){text(chr[i]/2, mydepth*(num-3.1), paste("chr: ",i,sep="") ,cex=1.3  )}else{
     text((chr[i]+chr[i-1])/2, mydepth*(num-3.1), i ,cex=1.3  )
  }
}
for(j in 1:(num-3)){
  abline(h=(j-1)*mydepth,lty=1)
}
ax=c(0,mydepth*0.25,mydepth*0.5,mydepth*0.75)
axx=c(0,mydepth*0.25,mydepth*0.5,mydepth*0.75)
for(i in 2:(num-3)){
 ax=c(ax, c(0,mydepth*0.25,mydepth*0.5,mydepth*0.75)+(i-1)*mydepth)
 axx=c(axx,c(0,mydepth*0.25,mydepth*0.5,mydepth*0.75))
}
axis(2,at=ax, axx)
text(1000,mydepth*0.7,myname[1]  )
for(i in 2:(num-3)){ 
  text(1000,ax[(i-1)*4+1]+mydepth*0.7,myname[i]  )
}
dev.off()

name_pdf=paste(paste(args[length(args)-2],args[length(args)-1],sep="/"),"_depth.pdf",sep="")

pdf(name_pdf,width = 15, height = 2.5*(num-3))
col= rainbow(num-3)
par(mar=c(5,5,3,2),font=2,cex.axis=1.2,font.lab=2,font.axis=2,cex.lab=1.2)
plot(data[[1]], pch=20, type='h', ylim=c(0,mydepth*(num-3)), col=col[1], yaxt='n', ylab='Depth (X)', xlab='Bin', xaxs='i')
for(i in 2:(num-3)){
  lines(data[[i]]+(i-1)*mydepth,col=col[i])
  
  for(j in 3:length(data[[i]])){
    polygon(x=c(j-2,j-2,j-1,j,j),y=c((i-1)*mydepth, data[[i]][j-2]+(i-1)*mydepth, data[[i]][j-1]+(i-1)*mydepth, data[[i]][j]+(i-1)*mydepth ,(i-1)*mydepth),col=col[i], border = col[i])
  }  
}
chr=rep(0,23)
for(i in 1:23){
  if(i==1){chr[i] = data_num[[1]][1]}else{ chr[i]=chr[i-1]+data_num[[1]][i] }
}
 
for(i in 1:23){
  abline(v=chr[i],lty=2)
  if(i==1){text(chr[i]/2, mydepth*(num-3.1), paste("chr: ",i,sep="") ,cex=1.3  )}else{
     text((chr[i]+chr[i-1])/2, mydepth*(num-3.1), i ,cex=1.3  )
  }
}
for(j in 1:(num-3)){
  abline(h=(j-1)*mydepth,lty=1)
}
ax=c(0,mydepth*0.25,mydepth*0.5,mydepth*0.75)
axx=c(0,mydepth*0.25,mydepth*0.5,mydepth*0.75)
for(i in 2:(num-3)){
 ax=c(ax, c(0,mydepth*0.25,mydepth*0.5,mydepth*0.75)+(i-1)*mydepth)
 axx=c(axx,c(0,mydepth*0.25,mydepth*0.5,mydepth*0.75))
}
axis(2,at=ax, axx)
text(1000,mydepth*0.7,myname[1]  )
for(i in 2:(num-3)){ 
  text(1000,ax[(i-1)*4+1]+mydepth*0.7,myname[i]  )
}
dev.off()
