require(corrplot)
par(mar=c(1,5,5,3), mfrow=c(1,3), mgp=c(1.5,0.2,0), xpd=T)
cor0 <- rcorr(as.matrix(topsoil[,c("Gravel","Silt","Clay")]))
m0 <- cor0$r; row.names(m0) <- c("Gravel.clo","Silt.clo","Clay.clo")
colnames(m0) <- row.names(m0)
corrplot(m0, method="ellipse",
         addCoef.col = "black", tl.col = "grey33", cl.pos = "b")
mtext("(a) Closed", side = 3, line = -3)
cor0 <- rcorr(as.matrix(topsoil[,c("Gravel.clr","Silt.clr","Clay.clr")]))
corrplot(cor0$r, method="ellipse", title = "",
         addCoef.col = "black", tl.col = "blue3", cl.pos = "b")
mtext("(b) Open (clr)", side = 3, line = -3)
cor0 <- rcorr(as.matrix(topsoil[,c("Gravel.alr","Silt.alr","Clay.alr")]))
corrplot(cor0$r, method="ellipse", title = "",
         addCoef.col = "black", tl.col = "darkred", cl.pos = "b")
mtext("(c) vOpen (alr)", side = 3, line = -3)
par(mfrow=c(1,1), mar = c(4,4,1,1))
