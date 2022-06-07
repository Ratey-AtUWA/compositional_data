logo <- readPNG("UWA logo_header.png")

par(mar = c(0.5,0.5,0.5,0.5))

layout(matrix(c(1,1,1,2),nrow = 1))

plot(c(0,1),c(0,1), axes=F, type="n",xaxt="n", yaxt="n",ann=F)
text(0,0.8, pos = 4, cex = 2, font = 2, 
     labels="ENVTM501 Data Analysis in R for Compositional Data")
text(0,0.6, pos = 4, cex = 1.5, 
     labels="Multivariate analyses on cities land-use compositional data")
text(0,0.4, pos = 4, font = 3, col = 12,
labels="Land use proportional area data for 40 cities on Earth")

plot(1,1, axes=F, type="n",xaxt="n", yaxt="n",ann=F)
addImg(logo, x = 1, y = 1, width = 0.5)

par(mar = c(3.5,3.5,0.5,0.5))