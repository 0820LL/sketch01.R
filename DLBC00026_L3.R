#! /usr/bin/env Rscript
a<-read.table("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/raw_DLBC00026_L3.GC")
		x<-a[,1]
		y<-a[,4]
		png("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/raw_DLBC00026_L3.GC.png",type="cairo")
		plot(x,y,xlim=c(0,(2*150-1)),ylim=c(0,50),col="red",type="l",xlab="Position along reads",ylab="percent",main="Base percentage composition along reads",lty=1,lwd=1.5)
		p<-a[,7]
		q<-a[,10]
		s<-a[,13]
		m<-a[,16]
		lines(x,p,col="magenta",type="l",lty=2,lwd=1.5)
		lines(x,q,col="darkblue",type="l",lty=4,lwd=1.5)
		lines(x,s,col="green",type="l",lty=5,lwd=1.5)
		lines(x,m,col="cyan3",type="l",lty=6,lwd=1.5)
		legend("topright",legend=c("A","T","G","C","N"),col=c("red","magenta","darkblue","green","cyan3"),lty=c(1,2,4,5,6))
		abline(v=150,col="darkblue",lty=2)
		dev.off()

a<-read.table("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/raw_DLBC00026_L3.QD")
		x<-a[,1]
		y<-a[,2]
		png("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/raw_DLBC00026_L3.QD.png",type="cairo")
		plot(x,y,col="red",type="l",xlab="Quality score",ylab="Number of bases",main="Quality score distribution over all sequences")
		axis(side=1,at=seq(from=0,to=max(x),by=5))
		dev.off()

a<-read.table("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/raw_DLBC00026_L3.QM")
		x<-a[,1]
		y<-a[,2]
		z<-a[,3]
		png("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/raw_DLBC00026_L3.QM.png",type="cairo")
		plot(x,y,xaxt="n",xlim=c(0,(2*150-1)),ylim=c(0,40),col="red",type="p",pch=".",cex=1.5,xlab="Position along reads",ylab="Quality",main="Qualities Distribution")
		axis(side=1,at=seq(from=0,to=(2*150-1),by=20))
		abline(h=20,col="darkblue",lty=3)
		abline(v=20,col="darkblue",lty=3)
		abline(v=40,col="darkblue",lty=3)
		abline(v=60,col="darkblue",lty=3)
		abline(v=80,col="darkblue",lty=3)
		abline(v=100,col="darkblue",lty=3)
		abline(v=120,col="darkblue",lty=3)
		abline(v=140,col="darkblue",lty=3)
		abline(v=160,col="darkblue",lty=3)
		abline(v=180,col="darkblue",lty=3)
		png("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/clean_DLBC00026_L3.Error.png",type="cairo")
		plot(x,z,xaxt="n",xlim=c(0,(2*150-1)),col="red",type="h",xlab="Position along reads",ylab="% Error-Rate")
		axis(side=1,at=seq(from=0,to=(2*150-1),by=20))
		abline(v=20,col="darkblue",lty=3)
		abline(v=40,col="darkblue",lty=3)
		abline(v=60,col="darkblue",lty=3)
		abline(v=80,col="darkblue",lty=3)
		abline(v=100,col="darkblue",lty=3)
		abline(v=120,col="darkblue",lty=3)
		abline(v=140,col="darkblue",lty=3)
		abline(v=160,col="darkblue",lty=3)
		abline(v=180,col="darkblue",lty=3)
		dev.off()

a<-read.table("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/clean_DLBC00026_L3.GC")
		x<-a[,1]
		y<-a[,4]
		png("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/clean_DLBC00026_L3.GC.png",type="cairo")
		plot(x,y,xlim=c(0,(2*150-1)),ylim=c(0,50),col="red",type="l",xlab="Position along reads",ylab="percent",main="Base percentage composition along reads",lty=1,lwd=1.5)
		p<-a[,7]
		q<-a[,10]
		s<-a[,13]
		m<-a[,16]
		lines(x,p,col="magenta",type="l",lty=2,lwd=1.5)
		lines(x,q,col="darkblue",type="l",lty=4,lwd=1.5)
		lines(x,s,col="green",type="l",lty=5,lwd=1.5)
		lines(x,m,col="cyan3",type="l",lty=6,lwd=1.5)
		legend("topright",legend=c("A","T","G","C","N"),col=c("red","magenta","darkblue","green","cyan3"),lty=c(1,2,4,5,6))
		abline(v=150,col="darkblue",lty=2)
		dev.off()

a<-read.table("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/clean_DLBC00026_L3.QD")
		x<-a[,1]
		y<-a[,2]
		png("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/clean_DLBC00026_L3.QD.png",type="cairo")
		plot(x,y,col="red",type="l",xlab="Quality score",ylab="Number of bases",main="Quality score distribution over all sequences")
		axis(side=1,at=seq(from=0,to=max(x),by=5))
		dev.off()

a<-read.table("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/clean_DLBC00026_L3.QM")
		x<-a[,1]
		y<-a[,2]
		z<-a[,3]
		png("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/clean_DLBC00026_L3.QM.png",type="cairo")
		plot(x,y,xaxt="n",xlim=c(0,(2*150-1)),ylim=c(0,40),col="red",type="p",pch=".",cex=1.5,xlab="Position along reads",ylab="Quality",main="Qualities Distribution")
		axis(side=1,at=seq(from=0,to=(2*150-1),by=20))
		abline(h=20,col="darkblue",lty=3)
		abline(v=20,col="darkblue",lty=3)
		abline(v=40,col="darkblue",lty=3)
		abline(v=60,col="darkblue",lty=3)
		abline(v=80,col="darkblue",lty=3)
		abline(v=100,col="darkblue",lty=3)
		abline(v=120,col="darkblue",lty=3)
		abline(v=140,col="darkblue",lty=3)
		abline(v=160,col="darkblue",lty=3)
		abline(v=180,col="darkblue",lty=3)
		png("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/01.QC/raw_DLBC00026_L3.Error.png",type="cairo")
		plot(x,z,xaxt="n",xlim=c(0,(2*150-1)),col="red",type="h",xlab="Position along reads",ylab="%Error-Rate")
		axis(side=1,at=seq(from=0,to=(2*150-1),by=20))
		abline(v=20,col="darkblue",lty=3)
		abline(v=40,col="darkblue",lty=3)
		abline(v=60,col="darkblue",lty=3)
		abline(v=80,col="darkblue",lty=3)
		abline(v=100,col="darkblue",lty=3)
		abline(v=120,col="darkblue",lty=3)
		abline(v=140,col="darkblue",lty=3)
		abline(v=160,col="darkblue",lty=3)
		abline(v=180,col="darkblue",lty=3)
		dev.off()
