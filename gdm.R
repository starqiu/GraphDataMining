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


divide.files.by.periods <- function(){
  matrix.table <- read.table(paste(BASE.PATH,FILE.NAME,sep=""),
                             header=TRUE,sep="")
  period.name <- ""
  z <- c((1-PERIOD.SAMPLE.COUNT):1)  
  
  for(i in 1:PERIOD.COUNT){
    z <- z+PERIOD.SAMPLE.COUNT
    z[1]<-1
    
    #4wk,8wk,12wk,16wk,20wk
    period.name <- paste("matrix_table_",i*4,"wk.txt",sep="")
    write.table(matrix.table[z],file=paste(BASE.PATH,period.name,sep=""),
                row.names = FALSE,
                sep="\t")
  }
  rm(matrix.table)
}

calc.and.filter.sd <- function(file.name,features.filered.by.sd=1000){
  period.matrix.table <- read.table(paste(BASE.PATH,file.name,".txt",sep=""),
                                    header=TRUE,sep="")  
  z <- c(2:(PERIOD.SAMPLE.COUNT+1))
  exam_names <-names(period.matrix.table)[z]
  #mean <- apply(period.matrix.table[z],1,mean)
  sd <- apply(period.matrix.table[z],1,sd) 
  #mean.sd <- data.frame(mean=mean,sd=sd)
  table.with.sd <- cbind(period.matrix.table,sd)
  
  write.table(table.with.sd,
              paste(BASE.PATH,file.name,"_with_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
  table.sorted.by.sd <-table.with.sd[order(-table.with.sd$sd),]
  table.sorted.by.sd <- table.sorted.by.sd[c(1:features.filered.by.sd),]
  write.table(table.sorted.by.sd,
              paste(BASE.PATH,file.name,"_with_high_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
}

calc.and.filter.sd.by.threshold <- function(file.name,features.sd.threshold=0.05){
  period.matrix.table <- read.table(paste(BASE.PATH,file.name,".txt",sep=""),
                                    header=TRUE,sep="")  
  z <- c(2:(PERIOD.SAMPLE.COUNT+1))
  exam_names <-names(period.matrix.table)[z]
  #mean <- apply(period.matrix.table[z],1,mean)
  sd <- apply(period.matrix.table[z],1,sd) 
  #mean.sd <- data.frame(mean=mean,sd=sd)
  table.with.sd <- cbind(period.matrix.table,sd)
  
  write.table(table.with.sd,
              paste(BASE.PATH,file.name,"_with_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
  table.sorted.by.sd <-table.with.sd[which(table.with.sd$sd >= features.sd.threshold),]
  #   table.sorted.by.sd <-table.with.sd[order(-table.with.sd$sd),]
  #   table.sorted.by.sd <- table.sorted.by.sd[c(1:features.filered.by.sd),]
  write.table(table.sorted.by.sd,
              paste(BASE.PATH,file.name,"_with_high_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
}

sd.test <- function(){
  for(i in 1:PERIOD.COUNT){   
    #4wk,8wk,12wk,16wk,20wk
    period.name <- paste("matrix_table_",i*4,"wk",sep="")
    #     calc.and.filter.sd(file.name=period.name,
    #                       features.filered.by.sd=FEATURES.FILTERED.BY.SD)
    calc.and.filter.sd.by.threshold(file.name=period.name,
                                    features.sd.threshold=FEATURES.SD.THRESHOLD)
  }
}

calc.pcc <- function(file.name){
  filtered.table <- read.table(paste(BASE.PATH,file.name,"_with_high_sd.txt",sep=""),
                               header=TRUE,sep="")
  geneIds <- filtered.table[,1] #as the row names and column names of matrix
  filtered.table <- filtered.table[,c(2:(PERIOD.SAMPLE.COUNT+1))]
  trans.matrix <- t(filtered.table) #matrix Transpose
  cor.matrix <- round(abs(cor(trans.matrix)),digits=2)
  rownames(cor.matrix) <- geneIds
  colnames(cor.matrix) <- geneIds
  write.table(cor.matrix,
              paste(BASE.PATH,file.name,"_cor_matrix.txt",sep=""),
              row.names=TRUE,
              sep="\t")
}

calc.ci <- function(x,pccin,pccout,sd){
  x[pccout]*x[sd]/x[pccin]
}

pcc.test <- function(period.name){

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
  cor.table[!upper.tri(cor.table)] <-NA
  
  
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
              paste(BASE.PATH,period.name,"_max_ci.txt"),
              row.names=FALSE,
              sep="\t",
              col.names=FALSE)
  
  max.model <- genes[as.integer(unlist(strsplit(as.character(models[which.max(ci)]),",")))]
  write.table(max.model,
              paste(BASE.PATH,period.name,"_dnb.txt"),
              row.names=FALSE,
              sep="\t",
              col.names=FALSE)
  
}

dnb.test <-function(){
  max.ci.file.name <- paste(BASE.PATH,"max_ci_features.txt")
  if(file.exists(max.ci.file.name)){
    file.remove(max.ci.file.name)
  }
  for(i in 1:PERIOD.COUNT){
    #4wk,8wk,12wk,16wk,20wk
    period.name <- paste("matrix_table_",i*4,"wk",sep="")
    calc.pcc(period.name)
    pcc.test(period.name)
  }
}

generate.dnb <-function(){
  max.ci.file.name <- paste(BASE.PATH,"max_ci_features.txt")
  max.ci.matrix <-read.table(max.ci.file.name,
                             sep="",
                             col.names=c("cluster","cluster.ci","cluster.vector"))
  # get cluster.vector
  dnb <- unlist(strsplit(as.character(max.ci.matrix[1,3]),","))
  # print(dnb)
  for(i in 2:PERIOD.COUNT){
    dnb <- intersect(dnb,unlist(strsplit(as.character(max.ci.matrix[i,3]),",")))
    #   print(dnb)
  }
  # print(dnb)
  write.table(dnb,paste(BASE.PATH,"dnb.txt",sep=""),sep="\n",
              col.names=FALSE,row.names=FALSE)
}

plot.ci <- function(){
  ci <- numeric()
  for(i in 1:PERIOD.COUNT){
    #4wk,8wk,12wk,16wk,20wk
    period.name <- paste("matrix_table_",i*4,"wk_max_ci.txt",sep="")
    ci[i] <- read.table(paste(BASE.PATH,period.name) 
  }
  png("ci.png")
  plot(c(1:PERIOD.COUNT)*4,ci)
  dev.off()
}

compare.to.example <- function(){
  example.dnb.t1 <-read.table(paste(BASE.PATH,"liver_DNB_t1.txt",sep=""))[,1]
  example.dnb.t4 <-read.table(paste(BASE.PATH,"liver_DNB_t4.txt",sep=""))[,1]
  dnb.4wk <-read.table(paste(BASE.PATH,"matrix_table_4wk_dnb.txt",sep=""))[,1]
  dnb.16wk <-read.table(paste(BASE.PATH,"matrix_table_16wk_dnb.txt",sep=""))[,1]

  #find common features
  common.features.t1 <- intersect(example.dnb.t1,dnb.4wk)
  write.table(common.features.t1,paste(BASE.PATH,"common_4wk_dnb.txt",sep=""),sep="\n",
              col.names=FALSE,row.names=FALSE)
  common.features.t4 <- intersect(example.dnb.t4,dnb.16wk)
  write.table(common.features.t4,paste(BASE.PATH,"common_16wk_dnb.txt",sep=""),sep="\n",
              col.names=FALSE,row.names=FALSE)
}

main <- function(){
  divide.files.by.periods()
  sd.test()
  dnb.test()
  plot.ci()
  compare.to.example()
}
sd.test()
dnb.test()
# plot.ci()
# compare.to.example()
# system.time(main())
