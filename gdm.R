library(plyr)
base.path <- "/host/data/"
file.name <- "liver_labeled_data.txt"
period.sample.count <- 10 #each period has 10 samples
period.count <- 5 #we have 5 periods
features.filered.by.sd <- 10
cluster.amount <-3 
matrix.table <- read.table(paste(base.path,file.name,sep=""),
                     header=TRUE,sep="")

divide.files.by.periods <- function(){
  
  period.name <- ""
  z <- c((1-period.sample.count):1)  
  
  for(i in 1:period.count){
    z <- z+period.sample.count
    z[1]<-1
    
    #4wk,8wk,12wk,16wk,20wk
    period.name <- paste("matrix_table_",i*4,"wk.txt",sep="")
    write.table(matrix.table[z],file=paste(base.path,period.name,sep=""),
                row.names = FALSE,
                sep="\t")
  }
}

calc.and.filter.sd <- function(file.name,features.filered.by.sd=1000){
  period.matrix.table <- read.table(paste(base.path,file.name,".txt",sep=""),
                                   header=TRUE,sep="")  
  z <- c(2:(period.sample.count+1))
  exam_names <-names(period.matrix.table)[z]
  #mean <- apply(period.matrix.table[z],1,mean)
  sd <- apply(period.matrix.table[z],1,sd) 
  #mean.sd <- data.frame(mean=mean,sd=sd)
  table.with.sd <- cbind(period.matrix.table,sd)
  
  write.table(table.with.sd,
              paste(base.path,file.name,"_with_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
  table.sorted.by.sd <-table.with.sd[order(-table.with.sd$sd),]
  table.sorted.by.sd <- table.sorted.by.sd[c(1:features.filered.by.sd),]
  write.table(table.sorted.by.sd,
              paste(base.path,file.name,"_with_high_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
}

sd.test <- function(table.with.sd){
  for(i in 1:period.count){   
    #4wk,8wk,12wk,16wk,20wk
    period.name <- paste("matrix_table_",i*4,"wk",sep="")
    calc.and.filter.sd(file.name=period.name,
                      features.filered.by.sd=features.filered.by.sd)
  }
}

calc.pcc <- function(file.name){
  filtered.table <- read.table(paste(base.path,file.name,"_with_high_sd.txt",sep=""),
                                    header=TRUE,sep="") 
  geneIds <- filtered.table[,1] #as the row names and column names of matrix
  filtered.table <- filtered.table[,c(2:(period.sample.count+1))]
  trans.matrix <- t(filtered.table) #matrix Transpose
  cor.matrix <- 1-cor(trans.matrix)
  rownames(cor.matrix) <- geneIds
  colnames(cor.matrix) <- geneIds
  write.table(cor.matrix,
              paste(base.path,file.name,"_cor_matrix.txt",sep=""),
              row.names=TRUE,
              sep="\t")
  #print(trans.matrix)
  #print(cor.matrix)  
}

pcc.test <- function(file.name){
  cor.table <- read.table(paste(base.path,file.name,"_cor_matrix.txt",sep=""),
                          header=TRUE,sep="")
  names(cor.table) <- row.names(cor.table)
  #print(cor.table)
  
  set.seed(252964) # 设置随机值，为了得到一致结果。
  kmeans_result <- kmeans(cor.table,cluster.amount)
  #print(kmeans_result)
  #print(kmeans_result$cluster)
  
  sds <- read.table(paste(base.path,file.name,"_with_high_sd.txt",sep=""),
                    header=TRUE,
                    sep="")[period.sample.count+2]
  #print(sds)
  cluster.index <-0
  pcc.in.mean <- numeric()
  pcc.out.mean <- numeric()
  cluster <- kmeans_result$cluster
  for (i in 1:features.filered.by.sd){
    cluster.index <- kmeans_result$cluster[i]
    pccs <- abs(1-cor.table[,i])
    pcc.in <- numeric()
    pcc.out <- numeric()
    for (j in 1:features.filered.by.sd){
      if(i != j && cluster.index == cluster[j]){
        pcc.in <- append(pcc.in,pccs[j])
      }
      else if(i != j && cluster.index != cluster[j]){
        pcc.out <- append(pcc.out,pccs[j])
      }
    }
    pcc.in.mean[i] <- mean(pcc.in,na.rm=TRUE)
    if(is.nan(pcc.in.mean[i])){
      pcc.in.mean[i] <- 0
    }
    pcc.out.mean[i] <- mean(pcc.out,na.rm=TRUE)
  }
  print("pcc.in.mean:")
  print(pcc.in.mean)
  print("pcc.out.mean:")
  print(pcc.out.mean)
  
  library(fpc)
  plotcluster(cor.table, kmeans_result$cluster) # 生成聚类图
}

#divide.files.by.periods()
#sd.test()
file.name <- paste("matrix_table_",1*4,"wk",sep="")
#calc.pcc(file.name)
pcc.test(file.name)