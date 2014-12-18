BASE.PATH <- "/host/data/"
# BASE.PATH <- "~/gdm/"

input.file.name <- "matrix_table_4wk_dnb.txt"
table.file.name <- "GPL.txt"

table.a <- read.table(paste(BASE.PATH,table.file.name,sep=""),header=FALSE,sep="\t")
ids <- read.table(paste(BASE.PATH,input.file.name,sep=""))[,1]