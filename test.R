library(plyr)
(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))
print(r1)
x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(x2) <- letters[1:11]
(r2 <- rank(x2)) # ties are averaged

print(r2)