seepal <- function(
    nc = 2,
    cex=1.6,
    pt.cex=5
)
{ plot(c(0,1),c(0,1), type="n", bty="n", ann = F, 
       xaxt="n", yaxt="n", xlab="", ylab="")
  legend("center", ncol = nc, bty = "n", 
         legend=paste(paste0(seq(1,length(palette())),"."),
                                        palette()),
         pt.bg = seq(1,length(palette())), 
         pch = 22, pt.cex=pt.cex, cex = cex,
         x.intersp = (pt.cex/5),
         y.intersp = 1.5*(pt.cex/5))
}