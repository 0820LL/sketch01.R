mydata=read.table(file="27_Sample_pre1.input.xls",header = T)
pdf("box_Lauren.pdf",width = 10, height = 6)
boxplot(mydata[,5]~mydata[,1] ,col = 'MediumVioletRed',main="the SomaticSNV multation frequency of tumor",boxwex=0.5,pch=20,cex=0.3,xlab="sample",ylab="multation frequency")
boxplot(mydata[,6]~mydata[,1] ,col='DarkGreen',main="the SomaticSNV multation frequency of ctDNA",boxwex=0.5,pch=20,cex=0.3,xlab="sample",ylab="multation frequency")


boxplot(mydata[,5]~mydata[,1] ,col = 'MediumVioletRed',main="the SomaticSNV multation frequency of tumor",boxwex=0.25,at = 1:26 - 0.2,pch=20,cex=0.3,xlab="sample",ylab="multation frequency")
boxplot(mydata[,6]~mydata[,1] ,col='DarkGreen',boxwex=0.25,at = 1:26 + 0.2,add = TRUE, pch=20,cex=0.3,xlab="sample",ylab="multation frequency")

dev.off()

