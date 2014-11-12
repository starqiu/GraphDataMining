library(plyr)
# BASE.PATH <- "/host/data/"
BASE.PATH <- "~/gdm/"
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
  cor.matrix <- cor(trans.matrix)
  rownames(cor.matrix) <- geneIds
  colnames(cor.matrix) <- geneIds
  write.table(cor.matrix,
              paste(BASE.PATH,file.name,"_cor_matrix.txt",sep=""),
              row.names=TRUE,
              sep="\t")
  #print(trans.matrix)
  #print(cor.matrix)  
}

calc.ci <- function(x,pccin,pccout,sd){
  x[pccout]*x[sd]/x[pccin]
}

# hclustfunc <- function(x, method = "complete", dmeth = "euclidean") {    
#   hclust(dist(x, method = dmeth), method = method)
# }

pcc.test <- function(file.name){
  cor.table <- read.table(paste(BASE.PATH,file.name,"_cor_matrix.txt",sep=""),
                          header=TRUE,sep="")
  names(cor.table) <- row.names(cor.table)
  cor.table <- abs(cor.table)
  #print(cor.table)
  
  set.seed(252964) # 设置随机值，为了得到一致结果。
  #   kmeans_result <- kmeans(cor.table,CLUSTER.AMOUNT)
  model <- hclust(as.dist(cor.table))
  cluster <- cutree(model,h = CLUSTER.HCLUST.H)
  #print(kmeans_result)
  #print(kmeans_result$cluster)
  
  sds <- read.table(paste(BASE.PATH,file.name,"_with_high_sd.txt",sep=""),
                    header=TRUE,
                    sep="")[PERIOD.SAMPLE.COUNT+2]
  #print(sds)
  
  #calc pcc.in and pcc.out
  cluster.index <-0
  pcc.in.mean <- numeric()
  pcc.out.mean <- numeric()
  #   cluster <- kmeans_result$cluster
  cluster.vector <- vector()
  seq <- 1:nrow(cor.table)
  for (i in seq){
    cluster.index <- cluster[i]
    if(is.na(cluster.vector[cluster.index])){
      cluster.vector[cluster.index] <- row.names(cor.table)[i]
    }else{
      cluster.vector[cluster.index] <- paste(cluster.vector[cluster.index],
                                             row.names(cor.table)[i],
                                             sep=",")
    }
    
    
    pccs <- cor.table[,i]
    pcc.in <- numeric()
    pcc.out <- numeric()
    for (j in seq[-i]){
      if(cluster.index == cluster[j]){
        pcc.in <- append(pcc.in,pccs[j])
      }else{
        pcc.out <- append(pcc.out,pccs[j])
      }
    }
    pcc.in.mean[i] <- mean(pcc.in,na.rm=TRUE)
    if(is.nan(pcc.in.mean[i])){
      pcc.in.mean[i] <- 0
    }
    pcc.out <- pcc.out[order(-pcc.out)]
    pcc.out.mean[i] <- mean(pcc.out[1:PCC.OUT.AMOUNT],na.rm=TRUE)
  }
  #   print(cluster.vector)
  cluster.pccin.pccout.sd <- cbind(cluster,pcc.in.mean,pcc.out.mean,sds)
  #   print(cluster.pccin.pccout.sd)
  cluster.pccin.pccout.sd.mean <-ddply(cluster.pccin.pccout.sd,.(cluster),
                                       summarize,
                                       in.mean=mean(pcc.in.mean),
                                       out.mean=mean(pcc.out.mean),
                                       sd.mean=mean(sd))
  #   print(cluster.pccin.pccout.sd.mean)
  cluster.ci <- apply(cluster.pccin.pccout.sd.mean,
                      1,
                      calc.ci,
                      pccin="in.mean",
                      pccout="out.mean",
                      sd="sd.mean")
  #   print(cluster.pccin.pccout.sd.mean[1])
  cluster.ci.features <- data.frame(cluster.pccin.pccout.sd.mean[1],cluster.ci,cluster.vector)
  #   print(cluster.ci.features)
  #  order by ci
  cluster.ci.features <- cluster.ci.features[order(-cluster.ci.features$cluster.ci),]
  write.table(cluster.ci.features[1,],
              paste(BASE.PATH,"max_ci_features.txt"),
              row.names=FALSE,
              sep="\t",
              append=TRUE,
              col.names=FALSE)
  #   print(cluster.ci.features)  
  #   ci.max <- max(cluster.ci)
  #   print("pcc.in.mean:")
  #   print(pcc.in.mean)
  #   print("pcc.out.mean:")
  #   print(pcc.out.mean)
  
  #   library(fpc)
  #   plotcluster(cor.table, kmeans_result$cluster) # 生成聚类图
}

dnb.test <-function(){
  max.ci.file.name <- paste(BASE.PATH,"max_ci_features.txt")
  if(file.exists(max.ci.file.name)){
    file.remove(max.ci.file.name)
  }
  for(i in 1:PERIOD.COUNT){
    #4wk,8wk,12wk,16wk,20wk
    period.name <- paste("matrix_table_",i*4,"wk",sep="")
    #     calc.pcc(period.name)
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

compare.to.example <- function(){
  example.dnb.t1 <-read.table(paste(BASE.PATH,"liver_DNB_t1.txt",sep=""))
  example.dnb.t4 <-read.table(paste(BASE.PATH,"liver_DNB_t4.txt",sep=""))
  dnb <-read.table(paste(BASE.PATH,"dnb.txt",sep=""))
  #translate into vectors
  example.dnb.t1 <- example.dnb.t1[,1]
  example.dnb.t4 <- example.dnb.t4[,1]
  dnb <- dnb[,1]
  
  #find common features
  common.features.t1 <- intersect(example.dnb.t1,dnb)
  print(common.features.t1)
  common.features.t4 <- intersect(example.dnb.t4,dnb)
  print(common.features.t4)
}

main <- function(){
  divide.files.by.periods()
  sd.test()
  dnb.test()
  generate.dnb()
  compare.to.example()
}
dnb.test()
# generate.dnb()
# compare.to.example()
# system.time(main())
