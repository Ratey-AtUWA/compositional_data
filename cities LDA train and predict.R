library(MASS)
library(RcmdrMisc)
library(knitr)
library(beepr)
### MATCH this to read file below
# sink(file="cities_type_pred.csv", type="output")
n0 <- 1000
results <- data.frame(
    Rep = rep(NA, n0),
    matches = rep(NA, n0),
    non_matches = rep(NA, n0),
    success = rep(NA, n0))
train <- sample(1:NROW(cities), round(NROW(cities)-5,0))
# train <- sample(1:NROW(cities), round(NROW(cities)*2/3,0))
lda.cities.train <- lda(formula = Type ~  Compact + Open + Lightweight + Industry, 
                    data = cities[train,],
                    prior = as.numeric(summary(cities$Type[train]))/
                      nrow(cities[train,])) 
lda.cities.pred <- predict(lda.cities.train, cities[-train,])
# make vector of individual category non-matches
matchByClass <- data.frame(Matches = rep(0,nlevels(cities$Type))) 
rownames(matchByClass) <- levels(lda.cities.pred$class)
e0 <- 0
# colnames(matchByClass) <- c("Matches")
for (i in 1:n0) {
  train <- sample(1:NROW(cities), round(NROW(cities)-5,0))
  # train <- sample(1:NROW(cities), round(NROW(cities)*2/3,0))
  if (is.na(match(NA,tapply(cities[train,]$Open, cities[train,]$Type, sd, na.rm=T))) == TRUE) {
    lda.cities.train <- lda(formula = Type ~ Compact + Open + Lightweight + Industry, 
                          data = cities[train,],
                          prior=as.numeric(summary(cities$Type[train]))/
                        nrow(cities[train,])) 
    lda.cities.pred <- predict(lda.cities.train, cities[-train,])
    e0 <- e0 + 1
  }

  k=0 # number of non-matches
  m0 <- as.matrix(rep(0,5)) # vector of individual category non-matches
  rownames(m0) <- levels(lda.cities.pred$class)
  rownames(matchByClass) <- levels(lda.cities.pred$class)
  colnames(matchByClass) <- c("Matches")
  for (jM in 1:NROW(cities[-train,])) {
    # cat("big loop #",jM,"\n")
    for (jS in 1:nlevels(lda.cities.pred$class)) {
      if((lda.cities.pred$class[jM] == levels(lda.cities.pred$class)[jS]) & 
         (cities$Type[-train][jM] == levels(lda.cities.pred$class)[jS]) ) 
        m0[jS] = m0[jS] + 1
      else  m0[jS] = m0[jS] 
      # cat("small loop iteration #",jS,"; matching",
      #     levels(lda.cities.pred$class)[jS],
      #     "; matches =",m0,"\n")
    }
    k = sum(m0)
    # cat("medium loop iteration ",jM,"; matches = ",k,"\n", sep="")
    # if(jM==NROW(cities[-train,])) 
  }
  # cat("GIANT LOOP #",i,"\n")
  matchByClass <- matchByClass + m0
  # output to results data frame: iteration, matches, non-matches, proportion matched
  results[i,] <- c(i, k, NROW(cities[-train,])-k, signif(k/NROW(cities[-train,]),3))
  # cbind(lda.cities.pred$class,cities$Type[-train])
}
matchByClass$Actual <- round(1000*as.numeric(summary(cities$Type))*
                               (NROW(cities[-train,])/NROW(cities)),0)
matchByClass$Proportion <- matchByClass$Matches/matchByClass$Actual

# sink() # close output file
### make sure read file is SAME AS SINK
beep(sound = 10)

# results <- read.csv("cities_type_pred.csv")
{cat("[Based on 1000 random subsets of dataset to",
    "train LDA model to\npredict remaining observations]\n")
print(numSummary(results[,2:4], statistics=c("mean","sd"))$table)
ns0 <- numSummary(results$success)
t0 <- t.test(results$success)
cat(rep("-\u2013-",24),
    "\nStat. summary for 'success':\nMean = ",round(ns0$table[1],3),
    ", sd = ",round(ns0$table[2],3),", 95% confidence interval = (",
    signif(t0$conf.int[1],3),", ",signif(t0$conf.int[2],3),") (after ",i," reps)\n", sep="")
cat(n0-e0,"iterations failed due to random sampling missing a group\n\n")
print(matchByClass, digits=3)}
rm(list = c("n0","i","e0","jS","jM","k","m0","matchByClass","results","train"))
