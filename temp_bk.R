s <- 1:10

c <- 0
for (i in s){
  for(j in s[-i]){
    print(paste(i,j))
    c <- c+1
  }
}

print(c)