
x <- c(931990, 964502, 954937, 953597, 944941, 932109, 1009600, 964509, 942964, 938047, 975387, 967477, 961968, 998731, 935775, 914612, 914794, 925887, 934119, 933846, 971132, 905737, 917081, 915482, 907766, 904063, 901259, 956855, 906789, 897771)
x.mean <- mean(x)
x.sd <- sd(x)
n <- 2
upper.filter <- x.mean + n*x.sd
lower.filter <- x.mean - n*x.sd
abnormal <- c()
for(xi in x){
  if(xi < lower.filter | xi > upper.filter){
    abnormal <- c(abnormal, xi)
  }
}
abnormal.ind <- which(x == abnormal)
normal.ind <- which(x != abnormal)
# plot data
plot(abnormal.ind, abnormal, pch = 2, col = "blue")
plot(normal.ind, x[normal.ind], pch = 5, col = "red")
lines(x)
points(abnormal, cex = 4, col = "yellow")
