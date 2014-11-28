mydata=read.table(file="27_Sample_pre_1_change.xls",header = T)
pdf("box_Lauren_type.pdf",width = 12, height = 6)
boxplot(mydata[,5]~mydata[,1] ,col = 'MediumVioletRed',main="The Somatic SNV mutation frequency of tumor",ylim=c(0,1),boxwex=0.5,pch=20,cex=0.3,xlab="sample",ylab="multation frequency")
boxplot(mydata[,6]~mydata[,1] ,col='DarkGreen',main="The Somatic SNV mutation frequency of ctDNA",ylim=c(0,1),boxwex=0.5,pch=20,cex=0.3,xlab="sample",ylab="multation frequency")


boxplot(mydata[,5]~mydata[,1] ,col = 'MediumVioletRed',main="The mutation frequency of somatic SNV",ylim=c(0,1),boxwex=0.25,xaxt="n",at = 1:27 - 0.17,pch=20,cex=0.3,xlab="sample",lxt="no",yab="multation frequency")
boxplot(mydata[,6]~mydata[,1] ,col='DarkGreen',ylim=c(0,1),boxwex=0.25,at = 1:27 + 0.17,add = TRUE, pch=20,cex=0.3,xlab="sample",xaxt="n",ab="multation frequency")
 a= c('A10', 'A11', 'A12', 'A14', 'A15', 'A17', 'A19', 'A25', 'A26', 'A30', 'A31', 'A32', 'A34', 'A36', 'B16', 'B22', 'B28',  'B3', 'B38',  'B8',  'B9', 'C18', 'C23', 'C24', 'C35',  'C6',  'C7')
axis(side=1,at=c(1:27),labels=a)

legend("topright", c("Tumor", "ctDNA"),
       fill = c("MediumVioletRed", "DarkGreen"))

dev.off()

