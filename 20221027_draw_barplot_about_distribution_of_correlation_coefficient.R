# this script is to draw a barplot about distributuion of correlation coefficient in correlation analysis result
# 2022/10/27 made by Ryosuke Hirota 

# inport result of correlation analysis
# this result is located at "# this data is located at "Dropbox/Okamura Lab share folder/Hirota/results_and_matterials/20220616_ROC_curve_for_determing_cutoff_of_cell_number""
setwd("C:/Rdata/ROC_curve_for_cutoff")
result <-read.table("combine_results_of_correlation_between_residual_and_RBP_exp.txt",sep="\t",header = T,stringsAsFactors = F)

# set variable for annotating
r <-seq(-1,1,0.1)
n <-c(50,seq(100,900,100))

# annotate range of correlation coefficient
for (i in 1:20) {
  if(r[i]<0){
    result[result[,5]>r[i]&result[,5]<=r[i+1],10] <-paste0(r[i],"ƒr…",r[i+1])
  }else{
    result[result[,5]<r[i+1]&result[,5]>=r[i],10] <-paste0(r[i],"…rƒ",r[i+1])
  }
}
result[result[,5]==(-1),10] <-(-1)

# annotate range of number of cell line
result[,11] <-NA

for (i in 1:10) {
  if(i==1){
    result[result[,9]<=n[i],11] <-paste0("n…",n[i])
  }
  if(i==10){
    result[result[,9]>=n[i],11] <-paste0("n†",n[i])
  }
  result[result[,9]>n[i-1]&result[,9]<=n[i],11] <-paste0(n[i-1],"ƒn…",n[i])
}

# remove unneccesary rows
df <-result[,c(10,11)]
colnames(df) <-c("r","number_of_cell_lines")

# make summary about number of cell lines and correlation coefficient
t.df <-as.data.frame(table(df[,2],df[,1]),stringsAsFactors = F)

# make empty matrix
m.df <-matrix(nrow =11,ncol=22 )

# set variable
r.range <-unique(df[,1])
n.range <-unique(df[,2])
n.range <-factor(n.range,levels = c("n…50","50ƒn…100","100ƒn…200","200ƒn…300",
                                    "300ƒn…400","400ƒn…500","500ƒn…600","600ƒn…700","700ƒn…800","800ƒn…900","n†900"))
n.range <-sort(n.range)
n.range <-as.character(n.range)

# make matrix for drawing barplot
for (i in 1:11) {
  for (k in 1:22) {
    freq <-t.df[t.df[,1]==n.range[i]&t.df[,2]==r.range[k],3]
    m.df[i,k] <-freq
  }}
rownames(m.df) <-n.range
colnames(m.df) <-r.range
 
# draw barplot
setwd("C:/Rdata/20221027_confidence_interval_of_correlation_coefficient")
pdf("distribution_of_correlation_coefficient.pdf")
barplot(m.df,legend=rownames(m.df),cex.names = 0.4,col=rainbow(11),ylab = "frequency",xlab="range of correlation coefficient")
dev.off()

# save matirx
write.table(m.df,"distribution_of_correaltion_coefficient.txt",sep="\t",quote = F)
