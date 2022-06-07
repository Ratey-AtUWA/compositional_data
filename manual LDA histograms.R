predClos <- predict(lda_rock_clos, Hallberg[,11:21])
predOpen <- predict(lda_rock_open, Hallberg[,11:21])

par(mfcol = c(nlevels(LD1c$Rock),2), mar = c(1,2,1,1), oma = c(1,0,1,0), 
    mgp = c(0.75,0.2,0), tcl=0.15)
LD1c <- data.frame(Rock=as.character(Hallberg$Rock),LD1=predClos$x[,1])
LD1c$Rock <- factor(LD1c$Rock, levels=levels(Hallberg$Rock))
for (i in 1:nlevels(LD1c$Rock)){
  with(subset(LD1c, subset=LD1c$Rock==levels(LD1c$Rock)[i]),
       hist(LD1, main = "", breaks = pretty(LD1c$LD1, n=20), col=3,
       xlim = c(min(LD1c$LD1, na.rm=T),max(LD1c$LD1, na.rm=T))))
  box()
  mtext(levels(LD1c$Rock)[i],3,-1.5, adj=0.02, cex = 0.85)
  if(i==1) mtext("(a) Closed data", font=2)
}

LD1o <- data.frame(Rock=as.character(Hallberg$Rock),LD1=predOpen$x[,1])
LD1o$Rock <- factor(LD1o$Rock, levels=levels(Hallberg$Rock))
for (i in 1:nlevels(LD1o$Rock)){
  with(subset(LD1o, subset=LD1o$Rock==levels(LD1o$Rock)[i]),
       hist(LD1, main = "", breaks = pretty(LD1o$LD1, n=20), col=4,
            xlim = c(min(LD1o$LD1, na.rm=T),max(LD1o$LD1, na.rm=T))))
  box()
  mtext(levels(LD1o$Rock)[i],3,-1.5, adj=0.02, cex = 0.85)
  if(i==1) mtext("(b) Open data", font=2)
}