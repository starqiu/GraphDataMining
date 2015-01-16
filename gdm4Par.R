#! /usr/bin/Rscript --no-save
#
#' @author : star qiu
#' @date 2014.8.1
#' 
#' 
library(plyr)
library(foreach)
library(doParallel)

# BASE.PATH <- "/host/data/"
BASE.PATH <- "~/gdm/"
FILE.NAME <- "liver_labeled_data.txt"

PERIOD.COUNT <- 5 #we have 5 periods:4wk,8wk,12wk,16wk,20wk
PERIOD.SAMPLE.SEP <- 10 #each period has 10 samples
PERIOD.SAMPLE.COUNT <- 5 # divide into GK and WKY, each have 5 samples 
STATE <- c("gk","wt") #gk is case,wt is control
FEATURES.SD.THRESHOLD <- 0.001
CLUSTER.HCLUST.H <- 0.75
PCC.OUT.AMOUNT <- 50
CORES <- 6

init <- function(args){
  len <- length(args)
  for (i in seq(1,len,by=2)){
    set.key.value(args[i],args[i+1])
  }
}

set.key.value  <- function(key,value){
  switch(key,
         "-p" = ,
         "--base.path" = BASE.PATH <<- value,
         "-f" = ,
         "--file.name" = FILE.NAME <<- value,
         "--period.count" = PERIOD.COUNT <<- as.integer(value),
         "--period.sample.count" = PERIOD.SAMPLE.COUNT <<- as.integer(value),
         "--period.sample.sep" = PERIOD.SAMPLE.SEP <<- as.integer(value),
         "--features.sd.threshold" = FEATURES.SD.THRESHOLD <<- as.numeric(value),
         "--cluster.hclust.h" = CLUSTER.HCLUST.H <<- as.numeric(value),
         "--pcc.out.amount" = PCC.OUT.AMOUNT <<- as.integer(value),
         "--cores" = CORES <<- as.integer(value)
  )
}

print.usage <- function(){
  cat("Usage: gdm4Par.R [-h/--help | -p/--base.path directory] \n
        [-f/--file.name file] [--period.count number] \n
        [--period.sample.count number] [--period.sample.sep number] \n
        [--features.sd.threshold float] [--cluster.hclust.h float] \n
        [--pcc.out.amount number] [cores number]\n")
  cat("Details:\n")
  cat("\t -h/--help   show the usage of gdm4Par.R \n")
  cat("\t -p/--base.path   set the base running  directory of program .
                           the value could be ~/gdm/ \n")
  cat("\t -f/--file.name   set the name of data file. 
                           the default is liver_labeled_data.txt \n")
  cat("\t --period.count   set the number of periods we have
                           the default is 5 periods:4wk,8wk,12wk,16wk,20wk \n")
  cat("\t --period.sample.count   set the number of samples at each period 
                                  for each sort of rat . the default is 5 \n")
  cat("\t --period.sample.sep   set the number of samples at each period
                                the default is 10. \n")
  cat("\t --features.sd.threshold   set the threshold of filtering SD
                                    the default is 0.001 \n")
  cat("\t --cluster.hclust.h   set the h value when we call the cutree function
                               the default is 0.75 \n")
  cat("\t --pcc.out.amount  set the max number of the PCC between two modules 
                            we select to calculate. the default is 50 \n")
  cat("\t --cores  set the number of cores we use for parallel program 
                   the default is 6 \n")
  cat("Description:\n")
  cat("\t  if -h/--help is appeared,the other parameters is ignored. 
      otherwise base.path is necessary.
       \t  if you want have more cores ,you can set it larger value ,
      the program may run faster.
       \t  change features.sd.threshold may have suprise. it's good encough
      to the data of rat's liver.
      \n")
}

#GK , WT
divide.files.by.state <- function(file.name){
  matrix.table <- read.table(paste(BASE.PATH,file.name,sep=""),
                             header=TRUE,sep="")
  gk.index <- c(1,2:6,12:16,22:26,32:36,42:46)
  wt.index <- c(1,7:11,17:21,27:31,37:41,47:51)
  write.table(matrix.table[gk.index],file=paste(BASE.PATH,"gk_data.txt",sep=""),
              row.names = FALSE,
              sep="\t")
  write.table(matrix.table[wt.index],file=paste(BASE.PATH,"wt_data.txt",sep=""),
              row.names = FALSE,
              sep="\t")
}

#4wk,8wk,12wk,16wk,20wk
divide.files.by.periods <- function(state,file.name){
  matrix.table <- read.table(paste(BASE.PATH,state,file.name,sep=""),
                             header=TRUE,sep="")
  period.name <- ""
  z <- c((1-PERIOD.SAMPLE.COUNT):1)  
  
  foreach(i = 1:PERIOD.COUNT) %dopar% {
    z <- z+PERIOD.SAMPLE.COUNT*i
    z[1]<-1
    
    #4wk,8wk,12wk,16wk,20wk
    period.name <- paste(state,"_matrix_table_",i*4,"wk.txt",sep="")
    write.table(matrix.table[z],file=paste(BASE.PATH,period.name,sep=""),
                row.names = FALSE,
                sep="\t")
  }
}

sd.test <- function(file.name,features.sd.threshold=0.001){
  gk.period.matrix.table <- read.table(paste(BASE.PATH,"gk_",file.name,".txt",sep=""),
                                       header=TRUE,sep="")  
  wt.period.matrix.table <- read.table(paste(BASE.PATH,"wt_",file.name,".txt",sep=""),
                                       header=TRUE,sep="")  
  z <- c(2:(PERIOD.SAMPLE.COUNT+1))
  
  gk.sd <- apply(gk.period.matrix.table[z],1,sd) 
  wt.sd <- apply(wt.period.matrix.table[z],1,sd)
  
  gene.sd <- gk.sd/wt.sd
  gene.sd.log <- log(gene.sd)
  gene.sd.log.p <- unlist(lapply(gene.sd.log,pnorm,mean=mean(gene.sd.log),sd=sd(gene.sd.log)))
  high.sd.index <- which((gene.sd.log.p <= features.sd.threshold) | (gene.sd.log.p >= (1-features.sd.threshold))) 
  #li's method
#   sd.log.threshold <- pnorm(features.sd.threshold/2,mean=mean(gene.sd.log),sd=sd(gene.sd.log))
#   high.sd.index <- which(abs(gene.sd.log) >= sd.log.threshold)

  write.table(gene.sd,
              paste(BASE.PATH,file.name,"_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
  write.table(gene.sd[high.sd.index],
              paste(BASE.PATH,file.name,"_high_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
  
  write.table(gk.sd[high.sd.index],
              paste(BASE.PATH,"gk_",file.name,"_high_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
  write.table(wt.sd[high.sd.index],
              paste(BASE.PATH,"wt_",file.name,"_high_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
  
  write.table(gk.period.matrix.table[high.sd.index,],
              paste(BASE.PATH,"gk_",file.name,"_with_high_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
  write.table(wt.period.matrix.table[high.sd.index,],
              paste(BASE.PATH,"wt_",file.name,"_with_high_sd.txt",sep=""),
              row.names=FALSE,
              sep="\t")
}

calc.pcc <- function(state,period){
  filter.table <- read.table(paste(BASE.PATH,state,"_matrix_table_",period*4,"wk_with_high_sd.txt",sep=""),
                             header=TRUE,sep="")
  geneIds <- filter.table[,1] #as the row names and column names of matrix
  filter.table <- filter.table[,c(2:(PERIOD.SAMPLE.COUNT+1))]
  trans.matrix <- t(filter.table) #matrix Transpose
  cor.matrix <- abs(cor(trans.matrix))
  rownames(cor.matrix) <- geneIds
  colnames(cor.matrix) <- geneIds
  cor.matrix
}

pcc.test <- function(period){
  #control sample 
  #   wt.cor.table <- read.table(paste(BASE.PATH,"wt_",period.name,"_cor_matrix.txt",sep=""),
  #                              header=TRUE,sep="")
  wt.cor.table <- calc.pcc("wt",period)
  #   names(wt.cor.table) <- row.names(wt.cor.table)
  genes <- row.names(wt.cor.table)
  genes.number <- length(genes)
  genes.index <- 1:genes.number
  #matrix is more lightweight
  #   wt.cor.table <- as.matrix(wt.cor.table)
  
  #case sample
  #   gk.cor.table <- read.table(paste(BASE.PATH,"gk_",period.name,"_cor_matrix.txt",sep=""),
  #                              header=TRUE,sep="")
  gk.cor.table <- calc.pcc("gk",period)
  #   names(gk.cor.table) <- row.names(gk.cor.table)
  #   #matrix is more lightweight
  #   gk.cor.table <- as.matrix(gk.cor.table)
  
  model <- hclust(as.dist(1-gk.cor.table))
  cluster <- cutree(model,h = CLUSTER.HCLUST.H)
  
  gk.sd <- read.table(paste(BASE.PATH,"gk_matrix_table_",period*4,"wk_high_sd.txt",sep=""),
                      header=TRUE,
                      sep="")
  wt.sd <- read.table(paste(BASE.PATH,"wt_matrix_table_",period*4,"wk_high_sd.txt",sep=""),
                      header=TRUE,
                      sep="")
  
  df.with.cluster.genes.sds <- cbind(cluster,genes.index,gk.sd,wt.sd)
  colnames(df.with.cluster.genes.sds) <-c("cluster","genes.index","gk.sd","wt.sd")
  df.aggr.by.cluster <- ddply(df.with.cluster.genes.sds,.(cluster),summarize,
                              models = paste(genes.index,collapse=","),
                              sd = mean(gk.sd)/mean(wt.sd),
                              .parallel=TRUE)
  colnames(df.aggr.by.cluster) <-c("cluster","models","sd")
  
  cluster.aggr <- df.aggr.by.cluster$cluster
  models <- df.aggr.by.cluster$models
  cluster.number <- length(cluster.aggr)
  
  
  #the diag of table is meaningless
  diag(wt.cor.table) <- NA
  diag(gk.cor.table) <- NA
  
  pcc.in.mean <- numeric()
  pcc.out.mean <- numeric()
  
  for(cluster.index in 1:cluster.number){
    cur.model <- as.integer(unlist(strsplit(as.character(models[cluster.index]),",")))
    model.size <- length(cur.model)
    wt.pcc.in <- as.vector(wt.cor.table[cur.model,cur.model])
    gk.pcc.in <- as.vector(gk.cor.table[cur.model,cur.model])
    
    wt.pcc.out <- as.vector(wt.cor.table[-cur.model,cur.model])
    gk.pcc.out <- as.vector(gk.cor.table[-cur.model,cur.model])
    
    pcc.in.mean[cluster.index] <- (sum(gk.pcc.in,na.rm=TRUE)-model.size)/(sum(wt.pcc.in,na.rm=TRUE)-model.size)
    
    pcc.out <- cbind(wt.pcc.out,gk.pcc.out)
    pcc.out <- pcc.out[order(-pcc.out[,1]),]
    pcc.out <- pcc.out[1:(PCC.OUT.AMOUNT*model.size),]
    pcc.out.mean[cluster.index] <- mean(pcc.out[,2],na.rm=TRUE)/mean(pcc.out[,1],na.rm=TRUE)   
  }
  ci <- pcc.in.mean*(df.aggr.by.cluster$sd)/pcc.out.mean
  
  ci.max <- max(ci)
  write.table(ci.max,
              paste(BASE.PATH,"matrix_table_",period*4,"wk_max_ci.txt",sep=""),
              row.names=FALSE,
              sep="\t",
              col.names=FALSE)
  
  max.model <- genes[as.integer(unlist(strsplit(as.character(models[which.max(ci)]),",")))]
  write.table(max.model,
              paste(BASE.PATH,"matrix_table_",period*4,"wk_dnb.txt",sep=""),
              row.names=FALSE,
              sep="\t",
              col.names=FALSE)
}

plot.ci <- function(){
  ci <<- numeric()
  periods <-1:PERIOD.COUNT
  for(i in periods){
    #4wk,8wk,12wk,16wk,20wk
    period.name <- paste("matrix_table_",i*4,"wk_max_ci.txt",sep="")
    ci[i] <<- read.table(paste(BASE.PATH,period.name,sep=""))
  }
  names(ci) <<- c("4wk","8wk","12wk","16wk","20wk")
  print("ci table:")
  print(as.table(unlist(ci)))
  write.table(ci,
              paste(BASE.PATH,"all_ci.txt",sep=""),
              row.names=FALSE,
              sep="\t",
              col.names=FALSE)
  setwd(BASE.PATH)
  png("ci.png")
  plot(periods,unlist(ci),
       xlab="period (*4 wk)",
       ylab="ci",
       main="ci growth",
       type="b")
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


gdm <- function(){
  registerDoParallel(cores=CORES)
  
  divide.files.by.state(FILE.NAME)
  foreach(state = STATE) %dopar% {
    divide.files.by.periods(state,"_data.txt")
  }
  
  foreach (period = 1:PERIOD.COUNT) %dopar% {
    # for (period in 1:PERIOD.COUNT)  {
    #4wk,8wk,12wk,16wk,20wk
    file.name <- paste("matrix_table_",period*4,"wk",sep="")
    sd.test(file.name=file.name,features.sd.threshold=FEATURES.SD.THRESHOLD)
    foreach(state = STATE) %dopar% {
      calc.pcc(state,period)
    }
    pcc.test(period)
  }
  plot.ci()
}

main <- function(){
  args <- commandArgs(TRUE)
  if ((length(args) %% 2 != 0) | (length(args) == 0)){
    print.usage()
  }else {
    init(args)
    gdm()
  }
}
#main()
system.time(main())
