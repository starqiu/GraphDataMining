cor.table <- read.table(paste(BASE.PATH,"matrix_table_4wk_cor_matrix.txt",sep=""),
                        header=TRUE,sep="")
names(cor.table) <- row.names(cor.table)
# cor.table <- 1-abs(cor.table)

m <- as.dist(cor.table)
m1 <- dist(cor.table)
write.table(as.matrix(m),"z.txt",sep="\t")