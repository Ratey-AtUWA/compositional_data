pca_usgs_clos <- prcomp(log10(usgs[,9:33]), scale. = TRUE)
pca_usgs_clos$rot[,1:5]
cat("...\n\nComponent Variances - Closed data\n")
pca_usgs_clos$sdev^2
cat("___\n\nProportions of variance explained by each component",
    "\nClosed data\n")
round(pca_usgs_clos$sdev^2/sum(pca_usgs_clos$sdev^2),3)
cat("___\n\nCumulative proportions of variance explained by each component",
    "\nClosed data\n")
cumsum(round(pca_usgs_clos$sdev^2/sum(pca_usgs_clos$sdev^2),3))
# _._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.

par(mfrow=c(1,2), mar = c(3,3,3,3), oma = c(0,0,0,0), 
    mgp=c(1.6,0.3,0), tcl = 0.25, font.lab=2,
    lend = "square", ljoin = "mitre")
palette(c("black",c("#104E8B80", "#A0522D80", "#36648B80", "#8B5A2B80", 
                    "#CD000080", "#A020F080", "#7D26CD80", "#CD4F3980"),"white"))

# choose components and set scaling factor (sf)
v1 <- 1; v2 <- 2; sf <- 0.06

plot(pca_usgs_clos$rot[,v1]*1.5, pca_usgs_clos$rot[,v2]*1.6, 
     type = "n", xlim=c(-0.7,0.55), ylim = c(-0.5,0.5), 
     xlab = paste0("PC",v1," (",
                   signif(100*pca_usgs_clos$sdev^2
                          /sum(pca_usgs_clos$sdev^2),3)[1],"%)"),
     ylab = paste0("PC",v2," (",
                   signif(100*pca_usgs_clos$sdev^2
                          /sum(pca_usgs_clos$sdev^2),3)[2],"%)"))
abline(h=0, col="grey",lty=3);abline(v=0, col="grey",lty=3)
# mtext(paste0("Scaled PC",v1," Observation Scores"), 3, 1.6, font = 2)
# mtext(paste0("Scaled PC",v2," Observation Scores"), 4, 1.6, font = 2)
# points(pca_usgs_clos$x[,v1]*sf, pca_usgs_clos$x[,v2]*sf,
#        pch = c(3,4,7,8,9,10,12,13)[usgs_clr$Rock],
#        lwd=c(2,2,2,2,0,0,0,0)[usgs_clr$Rock], 
#        col = c(2:9)[usgs_clr$Rock], cex = 1)

arrows(0, 0, pca_usgs_clos$rot[,v1], pca_usgs_clos$rot[,v2], 
       length = 0.08, lwd = 2)
text(pca_usgs_clos$rot[,v1]*1.1, 
           jitter(pca_usgs_clos$rot[,v2]*1.1, factor = 10),
           labels = row.names(pca_usgs_clos$rot), 
           col="black",bg="white")
mtext("(a)",3,-1.5, adj = 0.05, cex = 1.25)
# legend("topleft", bty = "n", inset = 0.003, ncol=1,
#        box.col = "gray", box.lwd = 2, bg = 14,
#        legend = levels(usgs_clr$Rock),
#        pch = c(3,4,7,8,9,10,12,13), 
#        pt.lwd = c(2,2,2,2,0,0,0,0),
#        col = c(2:9), pt.bg = c(2:9),
#        pt.cex = c(1.2, 1.4, 1.2,1.2,1.4),
#        cex = 1, x.intersp = 0.6, y.intersp = 0.9)

v1 <- 1; v2 <- 2; sf <- 0.06

plot(pca_usgs$rot[,v1]*1.5, pca_usgs$rot[,v2]*1.6, 
     type = "n", xlim=c(-0.7,0.55), ylim = c(-0.4,0.4), 
     xlab = paste0("PC",v1," (",
                   signif(100*pca_usgs$sdev^2
                          /sum(pca_usgs$sdev^2),3)[1],"%)"),
     ylab = paste0("PC",v2," (",
                   signif(100*pca_usgs$sdev^2
                          /sum(pca_usgs$sdev^2),3)[2],"%)"))
abline(h=0, col="grey",lty=3);abline(v=0, col="grey",lty=3)
# mtext(paste0("Scaled PC",v1," Observation Scores"), 3, 1.6, font = 2)
# mtext(paste0("Scaled PC",v2," Observation Scores"), 4, 1.6, font = 2)
# points(pca_usgs$x[,v1]*sf, pca_usgs$x[,v2]*sf,
#        pch = c(0,1,2,5,15,16,17,18)[usgs_clr$Rock],
#        lwd=c(2,2,2,2,0,0,0,0)[usgs_clr$Rock], 
#        col = c(2:9)[usgs_clr$Rock], cex = 1)

arrows(0, 0, pca_usgs$rot[,v1], pca_usgs$rot[,v2], 
       length = 0.08, lwd = 2)
text(pca_usgs$rot[,v1]+sign(pca_usgs$rot[,v1])*.03, 
           jitter(pca_usgs$rot[,v2]+sign(pca_usgs$rot[,v2])*.03, factor = 10),
           labels = row.names(pca_usgs$rot), 
           col="black",bg="white")
mtext("(b)",3,-1.5, adj = 0.05, cex = 1.25)
