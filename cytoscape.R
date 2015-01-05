# BASE.PATH <- "/host/data/"
BASE.PATH <- "~/gdm/"
PERIOD.COUNT <- 5 #we have 5 periods:4wk,8wk,12wk,16wk,20wk
PERIOD.SAMPLE.COUNT <- 5 # divide into GK and WKY, each have 5 samples 
CORES <- 6

calc.pcc <- function(state,period){
  filter.table <- read.table(paste(BASE.PATH,state,"_matrix_table_",period*4,"wk.txt",sep=""),
                             header=TRUE,sep="")
  geneIds <- filter.table[,1] #as the row names and column names of matrix
  filter.table <- filter.table[,c(2:(PERIOD.SAMPLE.COUNT+1))]
  trans.matrix <- t(filter.table) #matrix Transpose
  cor.matrix <- abs(cor(trans.matrix))
  rownames(cor.matrix) <- geneIds
  colnames(cor.matrix) <- geneIds
  cor.matrix
}

cor.matrix.profile <- function(period){
  gk.cor.matrix <- calc.pcc("gk",period)
  wt.cor.matrix <- calc.pcc("wt",period)
  cor.matrix <- gk.cor.matrix/wt.cor.matrix
  rm(gk.cor.matrix)
  rm(wt.cor.matrix)
  cor.vector.log <- log(as.vector(cor.matrix[lower.tri(cor.matrix)]))
  rm(cor.matrix)
  hist(cor.vector.log)
}

gen.gdm.csv.bk <- function(period){
  gk.cor.matrix <- calc.pcc("gk",period)
  wt.cor.matrix <- calc.pcc("wt",period)
  cor.matrix <- gk.cor.matrix/wt.cor.matrix
  rm(gk.cor.matrix)
  rm(wt.cor.matrix)
  
  genes <- rownames(cor.matrix)
  
  # sum(cor.matrix>1)-10729
  # hist(as.vector(cor.matrix))
  total.row <- nrow(cor.matrix)
  title <- c("source","target","interaction","directed","symbol","value")
  
  save.file.name <- paste(BASE.PATH,"gdm_",period*4,"wk.csv",sep="")
  if (file.exists(save.file.name)){
    file.remove(save.file.name)
  }
  write.table(t(title),save.file.name,
              append=TRUE,quote=FALSE,sep=",",
              row.names =FALSE,col.names=FALSE)
  
  interaction <- "pp"
  directed <- "FALSE"
  
  for( i in 1:(total.row-1)){
    
    element.index <- (i+1):total.row
    element.num <- total.row - i
    
    mysource <- genes[i]
    mytarget <- genes[element.index]
    value <- cor.matrix[i,element.index]
    symbol <- paste("abcd",i,element.index,sep="")
    
    cyto.csv <- cbind(mysource,mytarget,interaction,directed,symbol,value)
    
    write.table(cyto.csv,save.file.name,
                append=TRUE,quote=FALSE,sep=",",
                row.names =FALSE,col.names=FALSE)
  }
}

gen.gdm.csv <- function(period){
  gk.cor.matrix <- calc.pcc("gk",period)
  wt.cor.matrix <- calc.pcc("wt",period)
  cor.matrix <- gk.cor.matrix/wt.cor.matrix
  rm(gk.cor.matrix)
  rm(wt.cor.matrix)
  genes <- rownames(cor.matrix)
  # sum(cor.matrix>1)-10729
  # hist(as.vector(cor.matrix))
  total.row <- nrow(cor.matrix)
  title <- c("source","target","interaction","directed","symbol","value")
  
  save.file.name <- paste(BASE.PATH,"gdm_",period*4,"wk.csv",sep="")
  if (file.exists(save.file.name)){
    file.remove(save.file.name)
  }
  write.table(t(title),save.file.name,
              append=TRUE,quote=FALSE,sep=",",
              row.names =FALSE,col.names=FALSE)
  
  interaction <- "pp"
  directed <- "FALSE"
  
  for( i in 1:(total.row-1)){
    
    #     element.index <- (i+1):total.row
    #     element.num <- total.row - i
    element.index <- which(log(cor.matrix[i,])>5)
    element.index <- element.index[element.index>i]
    element.num <- length(element.index)
    
    if (element.num > 0){
      mysource <- genes[i]
      mytarget <- genes[element.index]
      value <- cor.matrix[i,element.index]
      symbol <- paste("abcd",i,element.index,sep="")
      
      cyto.csv <- cbind(mysource,mytarget,interaction,directed,symbol,value)
      
      write.table(cyto.csv,save.file.name,
                  append=TRUE,quote=FALSE,sep=",",
                  row.names =FALSE,col.names=FALSE)
    }
  }
}

main <- function(){
  registerDoParallel(cores=CORES)
  
  foreach (i = 1:PERIOD.COUNT) %dopar% {
    gen.gdm.csv(i)
  }
}

system.time(main())
# cor.matrix.profile(1)
