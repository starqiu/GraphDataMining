library(plyr)

BASE.PATH <- "/host/data/"
# BASE.PATH <- "~/gdm/"
FILE.NAME <- "liver_labeled_data.txt"
PERIOD.SAMPLE.COUNT <- 10 #each period has 10 samples
PERIOD.COUNT <- 5 #we have 5 periods:4wk,8wk,12wk,16wk,20wk
FEATURES.FILTERED.BY.SD <- 1000
FEATURES.SD.THRESHOLD <- 0.05
CLUSTER.AMOUNT <-3 
CLUSTER.HCLUST.H <- 0.75
PCC.OUT.AMOUNT <- 50

for(i in 1:PERIOD.COUNT){
    #4wk,8wk,12wk,16wk,20wk
    period.name <- paste("matrix_table_",i*4,"wk",sep="")
    #     calc.pcc(period.name)
    cor.table <- read.table(paste(BASE.PATH,period.name,"_cor_matrix.txt",sep=""),
                            header=TRUE,sep="")
    names(cor.table) <- row.names(cor.table)
    genes <- row.names(cor.table)
    genes.number <- length(genes)
    genes.index <- 1:genes.number
    
    set.seed(252964) # 设置随机值，为了得到一致结果。
    model <- hclust(as.dist(cor.table))
    cluster <- cutree(model,h = CLUSTER.HCLUST.H)
  
  sds <- read.table(paste(BASE.PATH,period.name,"_with_high_sd.txt",sep=""),
                    header=TRUE,
                    sep="")[PERIOD.SAMPLE.COUNT+2]
  
  df.with.cluster.genes.sds <- cbind(cluster,genes.index)
  df.with.cluster.genes.sds <- cbind(df.with.cluster.genes.sds,sds)
  colnames(df.with.cluster.genes.sds) <-c("cluster","genes.index","sds")
  df.aggr.by.cluster <- ddply(df.with.cluster.genes.sds,.(cluster),summarize,
                              models = paste(genes.index,collapse=","),
                              sd <- mean(sds))
  colnames(df.aggr.by.cluster) <-c("cluster","models","sd")
  
  cluster.aggr <- df.aggr.by.cluster$cluster
  models <- df.aggr.by.cluster$models
  cluster.number <- length(cluster.aggr)
  
  #make sure cor.table is upper.tri
  cor.table[lower.tri(cor.table,diag=TRUE)] <-NA
  
  
  pcc.in.mean <- numeric()
  pcc.out.mean <- numeric()
  pcc.in <- numeric()
  pcc.out <- numeric()
  for(cluster.index in 1:cluster.number){
    cur.model <- as.integer(unlist(strsplit(as.character(models[cluster.index]),",")))
    for(row.index in 1:genes.number){
      if(row.index %in% cur.model){
        pcc.in <- append(pcc.in,cor.table[row.index,cur.model])
        pcc.out <- append(pcc.out,cor.table[row.index,-cur.model])
      }else{
        pcc.out <- append(pcc.out,cor.table[row.index,cur.model])
      }
    }
    pcc.in.mean[cluster.index] <-mean(unlist(pcc.in),na.rm=TRUE)
    if(is.na(pcc.in.mean[cluster.index])){
      pcc.in.mean[cluster.index] <- 0
    }
    pcc.out <- unlist(pcc.out)
    pcc.out <- pcc.out[order(-pcc.out)]
    pcc.out.mean[cluster.index] <- mean(pcc.out[1:PCC.OUT.AMOUNT],na.rm=TRUE)  
  }
  
  ci <- pcc.out.mean*(df.aggr.by.cluster$sd)/pcc.in.mean
  
  ci.max <- max(ci)
  
  write.table(ci.max,
              paste(BASE.PATH,"max_ci_",i*4,"wk.txt"),
              row.names=FALSE,
              sep="\t",
              col.names=FALSE)
  max.model <- genes[as.integer(unlist(strsplit(as.character(models[which.max(ci)]),",")))]
  write.table(max.model,
              paste(BASE.PATH,"dnb_",i*4,"wk.txt"),
              row.names=FALSE,
              sep="\t",
              col.names=FALSE)
}


# df<-read.table(text="           
#  20100324 7.70   4.0000000
#  20100324 2.22   0.0000000
#  20100325 2.12   0.0000000
#  20100326 2.29   0.0000000
#  20100327 2.10   0.0000000
#  20100328 2.26   2.0000000
#  20100328 2.01   1.6000000
#  20100328 2.17   0.0000000
#  20100329 1.92   0.0000000
#  20100330 2.15   0.0000000")
# 
# ddply(df,.(V1),summarize,paste(V2,collapse=","))