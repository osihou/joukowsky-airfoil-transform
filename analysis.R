joukowsky <- function (z){
  z+(1+0i)/z
}

a <- seq(0,2*pi, length.out=10000)

b_deg <- 15
b <- pi*b_deg/180
c<-1
eps <- 0.15


aaa <- c*(1+eps)
aa <- (aaa/cos(b))^2

set <- aa*(complex(real = sin(a) - eps*c, imaginary = cos(a) + aaa*tan(b)))

plot( joukowsky(set), asp =1, cex = 0.01 , xlab = "Re", ylab = "Im",axes = TRUE)





