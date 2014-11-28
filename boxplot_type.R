mydata=read.table(file="aa.data.xls",header = T)
pdf("box_Lauren_type.pdf",width = 12, height = 6)
boxplot(mydata[,5]~mydata[,1] ,col = 'MediumVioletRed',main="The Somatic SNV mutation frequency of tumor",ylim=c(0,1),boxwex=0.5,pch=20,cex=0.3,xlab="sample",ylab="multation frequency")
boxplot(mydata[,6]~mydata[,1] ,col='DarkGreen',main="The Somatic SNV mutation frequency of ctDNA",ylim=c(0,1),boxwex=0.5,pch=20,cex=0.3,xlab="sample",ylab="multation frequency")


boxplot(mydata[,5]~mydata[,1] ,col = 'MediumVioletRed',main="The mutation frequency of somatic SNV",ylim=c(0,1),boxwex=0.25,xaxt="n",at = 1:26 - 0.17,pch=20,cex=0.3,xlab="sample",lxt="no",yab="multation frequency")
boxplot(mydata[,6]~mydata[,1] ,col='DarkGreen',ylim=c(0,1),boxwex=0.25,at = 1:26 + 0.17,add = TRUE, pch=20,cex=0.3,xlab="sample",xaxt="n",ab="multation frequency")
axis(side=1,at=c(1:26))

legend("topright", c("Tumor", "ctDNA"),
       fill = c("MediumVioletRed", "DarkGreen"))

dev.off()

