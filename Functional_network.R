library(WGCNA)
options(stringsAsFactors = FALSE)
Data =read.table("functions_network.txt",header = TRUE,row.names = 1)
datExpr0 = as.data.frame(t(Data))
h=corAndPvalue(datExpr0,y=NULL,use = "pairwise.complete.obs", method = "spearman")
write.table(h$cor,file="TOM_r.txt",quote=F,sep="\t")
write.table(h$p,file="TOM_p.txt",quote=F,sep="\t")
TOM =read.table("TOM_r.txt",header = TRUE)
tTOM=t(TOM)
dimnames(tTOM) = list(modProbes, modProbes)
#read the significance table an do the FDR correction
p=read.table("pvalue_linear.txt",header = FALSE)
p2=as.numeric(unlist(p))
p.adjust=p.adjust(p2, method = "BH", n = length(p2))
write.table(p.adjust,file="p.adjust.txt",quote=F,sep="\t")
#convert p.adjust into a dataframe object
q=as.data.frame(p.adjust)
#read the correlation table
r=read.table("corr_linear.txt",header = FALSE)
#extract the rownames matching certain significance criteria
w=which(q$p.adjust < 0.000000001)
w[1:10]
#subset the correlation table based on the rownames extracted above
r2=data.frame(which=w,r=r[w,])
#view the minimum correlation value as your threshold
summary(abs(r2))
#now export your network into edge file and node file with a correlation threshold
cyt=exportNetworkToCytoscape(tTOM,edgeFile = paste("CytoscapeInput-edges-.txt", sep=""),nodeFile = paste("CytoscapeInput-nodes-.txt", sep=""),weighted = TRUE,threshold = 0.60)