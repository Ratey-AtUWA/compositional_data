require(MASS) # required for LDA procedure
require(klaR) # required for stepwise refinement of LDA
#
directs <- c("backward","forward","both")
minimp <- c(0.001,0.002,0.003,0.005,0.01) # ,0.02,0.03,0.05
crits=c("CR","AC","AS","CF") # "CFvec",
cat("Sort order, Direction of steps",",", "Improvement tolerance",",","Improvement Criterion",
    ",", "Final predictor variables",",","Criterion value\n")
for (k in 1:3) {
  for (j in 1:5) {
    for (i in 1:4){
      # cat("\n",j,i,"Stepwise LDA, min. improvement tolerance =",minimp[j], ", Direction = both,
      #   Criterion = ", crits[i],"\n")
      stepwise.lda <- stepclass(formula = Type ~ Compact + Open + Lightweight + Industry, 
                                data=cities, output=FALSE, 
                                prior=as.numeric(summary(cities$Type))/
                                  nrow(cities), 
                                method="lda", improvement=minimp[j], direction=directs[k], 
                                criterion=crits[i])
      # print(stepwise.lda)
      finvars <- as.character(stepwise.lda$formula[3]) 
      finperf <- as.numeric(stepwise.lda$process[nrow(stepwise.lda$process),4])
      perfmeas <- stepwise.lda$performance.measure
      cat(((k*100)+((j*10)+i)),",",directs[k],",",minimp[j],",",perfmeas," (",crits[i],") ",
          ",", finvars,",", finperf,"\n")
    }
  }
}
# end nested loop
rm(list=c('directs','minimp','crits','i','j','k','finvars','finperf','perfmeas'))
