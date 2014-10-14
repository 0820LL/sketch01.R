        pdf(file="/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/02.Aln/Stat/RL010009_DLBC00026.histPlot.hasDup.pdf",w=8,h=6)
        rt <- read.table("/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/02.Aln/Stat/RL010009_DLBC00026.depth_frequency.hasDup.xls")
        opar <- par()
        
        rt_nrow = nrow(rt)
        mean_cvg = round(sum(rt$V1*rt$V3)/sum(rt$V3),2)
        median_cvg = median(rep(rt$V1, rt$V3))
        by = length(unlist(strsplit(split="",as.character(ceiling(mean_cvg)))))
        by = 10^(by-1)
        
        x_lim = median_cvg*2
        x_lim = which(rt$V2[x_lim:rt_nrow] < 10^-4)[1] + x_lim
        x_lim = floor(x_lim / by) * by
        
	if(x_lim>nrow(rt)){
		x_lim=nrow(rt)
	}
	        
        t=sum(rt$V2[(x_lim+1):length(rt$V2)])
        y=rt$V2[1:x_lim]
        y <- y*100
        y_lim = round(max(y),1) + 0.3  

        y=c(y,t*100)

        if(y_lim <= 1)
        {
           ybin = 0.2;
        }else{
           ybin = 0.5;
        }
        
        x <- rt$V1[1:(x_lim+1)]

        par(mar=c(4.5, 4.5, 2.5, 2.5))
        plot(x,y,col="blue",type='h', lwd=1.5, xlab="Sequencing depth", ylab="Fraction of target bases (%)", bty="l",ylim=c(0,y_lim),xlim=c(0,x_lim), cex.lab=1.5, cex.axis=1.4)
        arrows(mean_cvg, 0, mean_cvg , y_lim/2 , col="red", length = 0.1, lwd=2) 
        arrows(median_cvg, 0, median_cvg , y_lim/2 , col="gold", length = 0.1, lwd=2)
        temp = c( paste(sep=" ", 'mean_cvg', '=', mean_cvg) , paste(sep=" ", 'median_cvg', '=', median_cvg))
        legend("topleft", temp , text.col=c('red','gold'), ncol=1, bty='n', cex=1.4)
          
        par(opar)
        dev.off()

        png(filename="/PROJ/CR/Project/lb_sci_crq/beizhong-NSCLC/9_10_wbc.ca_20141004/lb/9/RL010009/02.Aln/Stat/RL010009_DLBC00026.histPlot.hasDup.png",width = 480, height = 360)
        par(mar=c(4.5, 4.5, 2.5, 2.5))
        plot(x,y,col="blue",type='h', lwd=1.5, xlab="Sequencing depth", ylab="Fraction of target bases (%)", bty="l",ylim=c(0,y_lim),xlim=c(0,x_lim), cex.lab=1.5, cex.axis=1.4)
        arrows(mean_cvg, 0, mean_cvg , y_lim/2 , col="red", length=0.1, lwd=2)
        arrows(median_cvg, 0, median_cvg , y_lim/2 , col="gold", length = 0.1, lwd=2)
        temp = c( paste(sep=" ", 'mean_cvg', '=', mean_cvg) , paste(sep=" ", 'median_cvg', '=', median_cvg))
        legend("topleft", temp , text.col=c('red','gold'), ncol=1, bty='n', cex=1.4)
        par(opar)
        dev.off()

