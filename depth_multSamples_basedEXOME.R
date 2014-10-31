args = commandArgs(TRUE)
num = length(args)
myorder = list()  # sample data
mydepth=as.numeric(args[length(args)])
depthdata = list() #the depth data
### sort bins by chr and position 
temp1 = read.table(file=args[1],as.is=T,head=T)
chr=c()
pos=c()
for(i in 1:nrow(temp1)){
  aa = strsplit(temp1[i,1],split=":")[[1]][1]
  if(aa=="X"){ chr[i] = 23}else{
     chr[i] =as.numeric(aa)
  }
  pos[i] = as.numeric(strsplit(strsplit(temp1[i,1],split=":")[[1]][2], split="-")[[1]][1])
}
temp11 = cbind(temp1,chr,pos)
myorder = order(temp11[,6],temp11[,7]) #myorder used to order the data by chr and position
temp1 = temp11[myorder,]
# all samples depth 
depthdata = matrix(nr=nrow(temp1),ncol=num-2)
cname=c()
cname[1] = "chr"
for(i in 1:(num-3)){
  aa = strsplit(args[i],split = "/")[[1]][length(strsplit(args[i],split = "/")[[1]])]
  cname[1+i] = strsplit(aa,split="\\.")[[1]][1]
}
colnames(depthdata) = cname
depthdata[,1]=temp1[,6]
depthdata[,2]=temp1[,5]
for(i in 2:(num-3)){
  temp1 = read.table(file=args[i],as.is=T,head=T)
  depthdata[,i+1] = temp1[myorder,5]
}
##plot the depth
name_jpeg=paste(paste(args[length(args)-2],args[length(args)-1],sep="/"),"_depth.jpeg",sep="")
jpeg(name_jpeg,width = 1280, height = 200*(num-3))
col= rainbow(num-3)
par(mar=c(5,5,3,2),font=2,cex.axis=1.2,font.lab=2,font.axis=2,cex.lab=1.2)
plot(depthdata[,2], pch=20, type='h', ylim=c(0,mydepth*(num-3)), col=col[1], yaxt='n', ylab='Depth (X)', xlab='Bin', xaxs='i')
for(i in 2:(num-3)){
  lines(depthdata[,i+1]+(i-1)*mydepth,col=col[i])
  for(j in 3:length(depthdata[,i+1])){
    polygon(x=c(j-2,j-2,j-1,j,j),y=c((i-1)*mydepth, depthdata[,i+1][j-2]+(i-1)*mydepth, depthdata[,i+1][j-1]+(i-1)*mydepth, depthdata[,i+1][j]+(i-1)*mydepth ,(i-1)*mydepth),col=col[i], border = col[i])
  }  
}
chr=rep(0,23) # the position used to print the chr ID
for(i in 1:23){
  if(i==1){
    chr[i] = length(which(depthdata[,1]==i))
  }else{
    chr[i] = length(which(depthdata[,1]==i))+chr[i-1]
  }
}
for(i in 1:23){
  abline(v=chr[i],lty=2)
  if(i==1){text(chr[i]/2, mydepth*(num-3.1), paste("chr: ",i,sep="") ,cex=1.3  )}else if(i==23){
   text((chr[i]+chr[i-1])/2, mydepth*(num-3.1), "X" ,cex=1.3  )
  }else{
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
text(1000,mydepth*0.7,cname[2]  )
for(i in 2:(num-3)){ 
  text(1000,ax[(i-1)*4+1]+mydepth*0.7,cname[i+1]  )
}
dev.off()

name_pdf=paste(paste(args[length(args)-2],args[length(args)-1],sep="/"),"_depth.pdf",sep="")
pdf(name_pdf,width = 15, height = 2.5*(num-3))
col= rainbow(num-3)
par(mar=c(5,5,3,2),font=2,cex.axis=1.2,font.lab=2,font.axis=2,cex.lab=1.2)
plot(depthdata[,2], pch=20, type='h', ylim=c(0,mydepth*(num-3)), col=col[1], yaxt='n', ylab='Depth (X)', xlab='Bin', xaxs='i')
for(i in 2:(num-3)){
  lines(depthdata[,i+1]+(i-1)*mydepth,col=col[i])
  for(j in 3:length(depthdata[,i+1])){
    polygon(x=c(j-2,j-2,j-1,j,j),y=c((i-1)*mydepth, depthdata[,i+1][j-2]+(i-1)*mydepth, depthdata[,i+1][j-1]+(i-1)*mydepth, depthdata[,i+1][j]+(i-1)*mydepth ,(i-1)*mydepth),col=col[i], border = col[i])
  }  
}
chr=rep(0,23) # the position used to print the chr ID
for(i in 1:23){
  if(i==1){
    chr[i] = length(which(depthdata[,1]==i))
  }else{
    chr[i] = length(which(depthdata[,1]==i))+chr[i-1]
  }
}
for(i in 1:23){
  abline(v=chr[i],lty=2)
  if(i==1){text(chr[i]/2, mydepth*(num-3.1), paste("chr: ",i,sep="") ,cex=1.3  )}else if(i==23){
   text((chr[i]+chr[i-1])/2, mydepth*(num-3.1), "X" ,cex=1.3  )
  }else{
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
text(1000,mydepth*0.7,cname[2]  )
for(i in 2:(num-3)){ 
  text(1000,ax[(i-1)*4+1]+mydepth*0.7,cname[i+1]  )
}
dev.off()







