library(MASS)
library(RcmdrMisc)
library(knitr)
library(beepr)
### MATCH this to read file below
n0 <- 1000 # number of iterations
ftrain <- 0.75 # proportion of observations in training set
results <- data.frame(
  Rep = rep(NA, n0),
  matches = rep(NA, n0),
  non_matches = rep(NA, n0),
  success = rep(NA, n0))
# make vector of individual category non-matches
matchByClass <- data.frame(Matches = rep(0,nlevels(cities_clr$Type))) 
rownames(matchByClass) <- levels(lda.cities_clr.pred$class)
e0 <- 0
# colnames(matchByClass) <- c("Matches")
for (i in 1:n0) {
  # train <- sample(1:NROW(cities_clr), round(NROW(cities_clr)-5,0))
  train <- sample(1:NROW(cities_clr), round(NROW(cities_clr) * ftrain,0))
  if (is.na(match(NA,tapply(cities_clr[train,]$Open, cities_clr[train,]$Type, sd, na.rm=T))) == TRUE) {
    lda.cities_clr.train <- lda(formula = Type ~ Compact + Open + Lightweight + Industry, 
                            data = cities_clr[train,],
                            prior=as.numeric(summary(cities_clr$Type[train]))/
                              nrow(cities_clr[train,])) 
    lda.cities_clr.pred <- predict(lda.cities_clr.train, cities_clr[-train,])
    e0 <- e0 + 1
  }
  
  k=0 # number of non-matches
  m0 <- as.matrix(rep(0,5)) # vector of individual category non-matches
  rownames(m0) <- levels(lda.cities_clr.pred$class)
  rownames(matchByClass) <- levels(lda.cities_clr.pred$class)
  colnames(matchByClass) <- c("Matches")
  for (jM in 1:NROW(cities_clr[-train,])) {
    # cat("big loop #",jM,"\n")
    for (jS in 1:nlevels(lda.cities_clr.pred$class)) {
      if((lda.cities_clr.pred$class[jM] == levels(lda.cities_clr.pred$class)[jS]) & 
         (cities_clr$Type[-train][jM] == levels(lda.cities_clr.pred$class)[jS]) ) 
        m0[jS] = m0[jS] + 1
      else  m0[jS] = m0[jS] 
      # cat("small loop iteration #",jS,"; matching",
      #     levels(lda.cities_clr.pred$class)[jS],
      #     "; matches =",m0,"\n")
    }
    k = sum(m0)
    # cat("medium loop iteration ",jM,"; matches = ",k,"\n", sep="")
    # if(jM==NROW(cities_clr[-train,])) 
  }
  # cat("GIANT LOOP #",i,"\n")
  matchByClass <- matchByClass + m0
  # output to results data frame: iteration, matches, non-matches, proportion matched
  results[i,] <- c(i, k, NROW(cities_clr[-train,])-k, signif(k/NROW(cities_clr[-train,]),3))
  # cbind(lda.cities_clr.pred$class,cities_clr$Type[-train])
}
matchByClass$Actual <- round(n0*as.numeric(summary(cities_clr$Type))*
                               (NROW(cities_clr[-train,])/NROW(cities_clr)),0)
matchByClass$Proportion <- matchByClass$Matches/matchByClass$Actual

beep(sound = 10)

{cat("[Based on", n0, "random subsets of dataset to",
     "train LDA model to\npredict remaining observations]\n")
  cat("Number of obs. in random subsets =",NROW(train),
      " (predicting",NROW(cities_clr)-NROW(train),"samples)\n")
  print(numSummary(results[,2:4], statistics=c("mean","sd"))$table)
  ns0 <- numSummary(results$success)
  t0 <- t.test(results$success)
  cat(rep("-\u2013-",24),
      "\nStat. summary for 'success':\nMean = ",round(ns0$table[1],4),
      ", sd = ",round(ns0$table[2],4),", 95% confidence interval = (",
      signif(t0$conf.int[1],3),", ",signif(t0$conf.int[2],4),") (after ",i," reps)\n", sep="")
  cat(n0-e0,"iterations failed due to random sampling missing a group\n\n")
  print(matchByClass, digits=3)}
rm(list = c("n0","ftrain","i","e0","jS","jM","k","m0","matchByClass","results","train"))
