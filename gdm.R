library(plyr)
BASE.PATH <- "/host/data/"
FILE.NAME <- "liver_labeled_data.txt"
PERIOD.SAMPLE.COUNT <- 10 #each period has 10 samples
PERIOD.COUNT <- 5 #we have 5 periods
FEATURES.FILTERED.BY.SD <- 1000
CLUSTER.AMOUNT <-3 
matrix.table <- read.table(paste(BASE.PATH,FILE.NAME,sep=""),
                     header=TRUE,sep="")

divide.files.by.periods <- function(){
  
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

sd.test <- function(table.with.sd){
  for(i in 1:PERIOD.COUNT){   
    #4wk,8wk,12wk,16wk,20wk
    period.name <- paste("matrix_table_",i*4,"wk",sep="")
    calc.and.filter.sd(file.name=period.name,
                      features.filered.by.sd=FEATURES.FILTERED.BY.SD)
  }
}

calc.pcc <- function(file.name){
  filtered.table <- read.table(paste(BASE.PATH,file.name,"_with_high_sd.txt",sep=""),
                                    header=TRUE,sep="") 
  geneIds <- filtered.table[,1] #as the row names and column names of matrix
  filtered.table <- filtered.table[,c(2:(PERIOD.SAMPLE.COUNT+1))]
  trans.matrix <- t(filtered.table) #matrix Transpose
  cor.matrix <- 1-cor(trans.matrix)
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
  x[pccin]*x[sd]/x[pccout]
}

pcc.test <- function(file.name){
  cor.table <- read.table(paste(BASE.PATH,file.name,"_cor_matrix.txt",sep=""),
                          header=TRUE,sep="")
  names(cor.table) <- row.names(cor.table)
  #print(cor.table)
  
  set.seed(252964) # 设置随机值，为了得到一致结果。
  kmeans_result <- kmeans(cor.table,CLUSTER.AMOUNT)
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
  cluster <- kmeans_result$cluster
  cluster.vector <- vector()
  for (i in 1:FEATURES.FILTERED.BY.SD){
    cluster.index <- cluster[i]
    if(is.na(cluster.vector[cluster.index])){
      cluster.vector[cluster.index] <- row.names(cor.table)[i]
    }else{
      cluster.vector[cluster.index] <- paste(cluster.vector[cluster.index],
                                             row.names(cor.table)[i],
                                             sep=",")
    }
    
    
    pccs <- abs(1-cor.table[,i])
    pcc.in <- numeric()
    pcc.out <- numeric()
    for (j in 1:FEATURES.FILTERED.BY.SD){
      if(i != j & cluster.index == cluster[j]){
        pcc.in <- append(pcc.in,pccs[j])
      }
      else if(i != j & cluster.index != cluster[j]){
        pcc.out <- append(pcc.out,pccs[j])
      }
    }
    pcc.in.mean[i] <- mean(pcc.in,na.rm=TRUE)
    if(is.nan(pcc.in.mean[i])){
      pcc.in.mean[i] <- 0
    }
    pcc.out.mean[i] <- mean(pcc.out,na.rm=TRUE)
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

divide.files.by.periods()
sd.test()
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
