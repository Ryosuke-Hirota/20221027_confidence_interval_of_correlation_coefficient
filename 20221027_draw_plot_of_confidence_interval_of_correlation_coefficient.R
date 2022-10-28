# this script is to investigate 95% confidence interval of correaltion coefficient by inverse fisher z
# 2022/10/27 made by Ryosuke Hirota

setwd("C:/Rdata")
dir.create("20221027_confidence_interval_of_correlation_coefficient")
setwd("C:/Rdata/20221027_confidence_interval_of_correlation_coefficient")

# set variables
r <-seq(-1,1,0.1)
n <-c(10,30,50,70,100,200,300,400,500,600,700,800,900)

df <-as.data.frame(matrix(nrow = length(r),ncol = length(n)))

# calculate 95% confidence interval by fisher's z conversion
for (i in 1:length(r)) {
  for (k in 1:length(n)) {
    z <-log((1+r[i])/(1-r[i]))/2
    zu <-z+(1.96/sqrt(n[k]-3))
    zl <-z-(1.96/sqrt(n[k]-3))
    ru <-(exp(2*zu)-1)/(exp(2*zu)+1)
    rl <-(exp(2*zl)-1)/(exp(2*zl)+1)
    rr <-paste0(ru,",",rl)
    df[i,k] <-rr
    }
}

colnames(df) <-n
rownames(df) <-r
df[21,] <-"1,1"

write.table(df,"table_of_confidence_interval_of_correlation_coefficient.txt",sep="\t",quote = F)

# draw plot
for (k in 1:length(n)) {
 ci <-str_split(pattern = ",",df[,k],simplify = T)
 cu <-as.data.frame(ci[,1])
 cu[,2] <-r
 cl <-as.data.frame(ci[,2])
 cl[,2] <-r 
 if(k==1){
 pdf("confidence_interval.pdf")
 plot(cu[,2],cu[,1],type="l",col=rainbow(18)[k],xaxt="n",yaxt="n",xlab = "",ylab = "")
 par(new=T)
 plot(cl[,2],cl[,1],type="l",col=rainbow(18)[k],xaxt="n",yaxt="n",xlab = "",ylab = "")
 }else{
   par(new=T)
   plot(cu[,2],cu[,1],type="l",col=rainbow(18)[k],xaxt="n",yaxt="n",xlab = "",ylab = "")
   par(new=T)
   plot(cl[,2],cl[,1],type="l",col=rainbow(18)[k],xaxt="n",yaxt="n",xlab = "",ylab = "")
  }
 if(k==13){
 par(new=T)
 plot(cl[,2],cl[,1],type="n",col=rainbow(18)[k],xlab = "population correlation coefficient",ylab="sample correlation coefficient",
      main = "95% confidence interval",xaxt="n",yaxt="n")
 par(new=T)
 axis(side=1,at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1))
 par(new=T)
 axis(side=2,at=c(-1,-0.8,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1))
 par(new=T)
 abline(v=0)
 par(new=T)
 abline(h=0)
 par(new=T)
 legend("topleft",legend = paste0("n=",n),col = rainbow(18),lty=1,ncol = 2)
 }else{}
}
dev.off()
