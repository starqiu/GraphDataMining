for(i in 1:period.count){
  #4wk,8wk,12wk,16wk,20wk
  period.name <- paste("matrix_table_",i*4,"wk.txt",sep="")
  calc.avg.and.deviation(file.name=period.name)
}

period.name <- paste("matrix_table_",1*4,"wk_with_mean_sd.txt",sep="")
[c(1:features.filered.by.sd),c(2:(period.sample.count+4))]