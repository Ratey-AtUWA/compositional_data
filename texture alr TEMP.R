tempDF <- data.frame(Gravel.alr=rep(NA,NROW(topsoil)),
                     Silt.alr=rep(NA,NROW(topsoil)),
                     Clay.alr=rep(NA,NROW(topsoil)))
tempDF[,1:3] <- alr(topsoil[,c("Gravel","Sand","Silt","Clay")], 2, ifwarn = F)
topsoil[,c("Gravel.alr","Silt.alr","Clay.alr")] <- tempDF
summary(topsoil[,c("Gravel.alr","Silt.alr","Clay.alr")])

# We might (for instance) be interested in whether we can predict the
# density of soil, which affects porosity and penetration of plant roots,
# from the grain-size proportions. Since there are likely to be
# collinearities between closed grain-size proportions, we might expect a
# better linear regression model using the alr-transformed data with
# closure removed. Let's see...

lm_closed <- with(topsoil, lm(Bulk.density ~ Gravel + Clay + Silt + Sand))
summary(lm_closed)
lm_open <- with(topsoil, lm(Bulk.density ~ Gravel.alr + Clay.alr + 
                                           Silt.alr))
summary(lm_open)

spm(~Gravel+Sand+Silt+Clay+Bulk.density|LF3, data=topsoil, smooth=F, 
    col = c("blue2","purple","red3"), pch = c(1,2,15))
spm(~Gravel.alr+Silt.alr+Clay.alr+Bulk.density|LF3, data=topsoil,
    use = "pairwise.complete.obs", smooth=F, cex = 1.4,
    col = c("blue2","purple","red3"), pch = c(1,2,15),
    legend=list(coords="left", pt.cex=1.4))
