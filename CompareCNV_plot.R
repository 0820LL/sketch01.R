args = commandArgs(TRUE)
#args[1] is the cnv result; args[2]is the patient's directory; args[3] is outDir
cnv = read.table(file=args[1],as.is=T,head=T) #header是指定是否原文件是否包含列名,默认为F
temp = read.table(file=args[1],as.is=T,head=F) #防止字符向量变为factors
samp= c()
for(i in 5:ncol(temp)){ 
  samp[i-4] = as.character(temp[1,i])
}
bindata=c()
for(i in 1:length(samp)){
   bindata[i]=paste(paste(args[2],paste(samp[i],"05.CNV/Single",sep="/"),sep="/"),paste(samp[i],".bin.txt",sep=""),sep="/")
} #paste()将向量中的元素对应的链接起来，元素之间用sep来连接
temp1 = read.table(file=bindata[1],as.is=T,head=T)
chr=c()
pos=c()
for(i in 1:nrow(temp1)){
  aa = strsplit(temp1[i,1],split=":")[[1]][1]
  if(aa=="X"){ chr[i] = 23}else{
     chr[i] =as.numeric(aa)
  }
  pos[i] = as.numeric(strsplit(strsplit(temp1[i,1],split=":")[[1]][2], split="-")[[1]][1])
}
temp11 = cbind(temp1,chr,pos) #将矩阵横向合并成一个大矩阵
myorder = order(temp11[,6],temp11[,7])
temp1 = temp11[myorder,]
# the position of each gene
cut=c()
cut_s=c()
for(j in 1:nrow(cnv)){
    aa=which(temp1[,4] == cnv[j,1])
    cut[(j-1)*2+1]=aa[1]
	cut[j*2]=aa[length(aa)]
	cut_s[j]=aa[1]
}
cnvorder = sort(cut_s,index.return=T)$ix
cnv_new =cnv[cnvorder,]
cut_s = cut_s[cnvorder]
cut = sort(cut)
# read the data of *bin.txt
cnvdata = list()
for(i in 1:length(samp)){
   temp =  read.table(file=bindata[i],as.is=T,head=T)
   aa = log2(temp[,5]/temp[,2])
   cnvdata[[i]] = aa[myorder] 
}
# plot the cnv
for(i in 1:nrow(cnv_new)){
  pos = c() # the postion used to plot
  if(cut[(i-1)*2+1]-100 > 0){
    if(temp1[cut[(i-1)*2+1],6] == temp1[cut[(i-1)*2+1]-100,6] ){
      pos[1] = cut[(i-1)*2+1]-100
	  pos[2] = cut[(i-1)*2+1]
	}else{
	  pos[1] = min(which(temp1[,6] == temp1[cut[(i-1)*2+1],6]))
	  pos[2] = cut[(i-1)*2+1]
	}
  }else{
    pos[1] = min(which(temp1[,6] == temp1[cut[(i-1)*2+1],6]))
	pos[2] = cut[(i-1)*2+1]
  }
  if( cut[i*2]+100 <= nrow(temp1)){
    if(temp1[cut[i*2],6] == temp1[cut[i*2]+100,6]){
      pos[4] = cut[i*2]+100
	  pos[3] = cut[i*2]
    }else{
      pos[4] = max(which(temp1[,6] == temp1[cut[i*2],6]))
	  pos[3] = cut[i*2]
    }
  }else{
    pos[4] = max(which(temp1[,6] == temp1[cut[i*2],6]))
	pos[3] = cut[i*2]
  }
  #plot 
  myjpeg = paste(args[3],paste(cnv_new[i,1],"_CNV.jpeg",sep=""),sep="/")
  jpeg(myjpeg,width =1000 , height =600)
  ncol = c("#8B8878","#FF0000FF" , "#0066FFFF","#FF9900FF" ,  "#00FF66FF" ,"#CC00FFFF" ,"#FF0099FF","#DFFF00FF","#00FFFFFF","#00B3FFFF","#FF00E6FF" )
  par(mar=c(5,5,3,2),font=2,cex.axis=1.5,font.lab=2,font.axis=2,cex.lab=1.5)
  plot(cnvdata[[1]][pos[1]:pos[4]] ,col=ncol[1],lwd=2,type='p',ylim=c(-2,8),xaxt='n',ylab='log2(case/control)',xlab = '',pch=16)
  for(j in 2:length(samp)){
    lines(cnvdata[[j]][pos[1]:pos[4]],col=ncol[j],lwd=2,type="p",pch=16)
  }
  abline(h=0,lwd=2)
  legend("topright", samp , col = ncol[1:length(samp)],
       text.col = "black", pch = c(16,16),cex=1.8)   
  for(j in 1:length(samp)){   
        lines(x=c(pos[2]-pos[1]+1,pos[3]-pos[1]+1),y=c(mean(cnvdata[[j]][pos[2]:pos[3]]),mean(cnvdata[[j]][pos[2]:pos[3]])),col=ncol[j],lwd=3 )
		aa = paste("C=",cnv_new[i,j+4],sep="")
		if(cnv_new[i,j+4] != "-"){
    	    text(x=pos[3]-pos[1]+15,y = mean(cnvdata[[j]][pos[2]:pos[3]]),labels = aa,cex=1.5,col=ncol[j])
	    }
 }
 text(x=0.5*(pos[3]-pos[2])+pos[2]-pos[1],y = 6.5,labels = cnv_new[i,1],cex=1.5)
 dev.off()
 mypdf =  paste(args[3],paste(cnv_new[i,1],"_CNV.pdf",sep=""),sep="/")
 pdf(mypdf,width = 10, height = 6)
   ncol = c("#8B8878","#FF0000FF" , "#0066FFFF","#FF9900FF" ,  "#00FF66FF" ,"#CC00FFFF" ,"#FF0099FF","#DFFF00FF","#00FFFFFF","#00B3FFFF","#FF00E6FF" )
  par(mar=c(5,5,3,2),font=2,cex.axis=1.5,font.lab=2,font.axis=2,cex.lab=1.5)
  plot(cnvdata[[1]][pos[1]:pos[4]] ,col=ncol[1],lwd=2,type='p',ylim=c(-2,8),xaxt='n',ylab='log2(case/control)',xlab = '',pch=16)
  for(j in 2:length(samp)){
    lines(cnvdata[[j]][pos[1]:pos[4]],col=ncol[j],lwd=2,type="p",pch=16)
  }
  abline(h=0,lwd=2)
  legend("topright", samp , col = ncol[1:length(samp)],
       text.col = "black", pch = c(16,16),cex=1.8)
  for(j in 1:length(samp)){
        lines(x=c(pos[2]-pos[1]+1,pos[3]-pos[1]+1),y=c(mean(cnvdata[[j]][pos[2]:pos[3]]),mean(cnvdata[[j]][pos[2]:pos[3]])),col=ncol[j],lwd=3 )
                aa = paste("C=",cnv_new[i,j+4],sep="")
                if(cnv_new[i,j+4] != "-"){
            text(x=pos[3]-pos[1]+15,y = mean(cnvdata[[j]][pos[2]:pos[3]]),labels = aa,cex=1.5,col=ncol[j])
            }
 }
 text(x=0.5*(pos[3]-pos[2])+pos[2]-pos[1],y = 6.5,labels = cnv_new[i,1],cex=1.5)
 dev.off()

}









