######################################################################
#
#   CS5608 Big Data Analytics
#   Coursework "Swiss referendum results analysis"
#   Annotated code
#   Author: Christophe Cestonaro
#
######################################################################
#
#   0. Preparing environment
#   1. Loading and merging data
#   2. Exploratory analysis 
#   3. Principal Component Analysis
#   4. Clustering analysis  
#   5. Decision tree analysis
#   6. Random forest analysis
#   7. Neural networks analysis
#
######################################################################
# 0. Preparing environment
# 

# Lot of features -> PCA shows it is hard to analyze all at once 
#
# split analysis along 3 axes, that don't make sense if mixed up
# - language
# - socio-economics
# - political

# Define variable to execute script in a verbose manner, i.e. providing more info about tasks progress
# and detailed results. If set to FALSE, only most important info is printed
verboseExecution = TRUE

#
# Helper functions
#
#  function to check whether the package is required and install it if necessary
#  The function first builds the list of packages to be installed so it can inform users
pkg_install <- function(pkg_list){
  missingPackages <- vector()
  for( i in pkg_list ){
    #  checking 'require' to load package
    if(!require(i,character.only = TRUE)){      
      missingPackages <- append(missingPackages, i)
    }
  }
  if (length(missingPackages)==0) {
    cat("No package needs to be installed\n")
  } else {
    cat("The following packages will be installed:")
    cat(missingPackages, sep=",")
    cat("\n")
    for( i in missingPackages ){
        install.packages(i,dependencies = TRUE)
        require(i,character.only = TRUE)
    }
  }
}

#  defining MinMax function used for resizing data intervals between 0 and 1
MinMax <- function(x){
  tx <- (x - min(x)) / (max(x) - min(x))
  return(tx)
}

# Prints the list of columns in the dataset passed as parameter along with their index
# Usefull to check the content of the dataset when it has many columns for debugging purposes
printCols <- function(dataset, verboseMode = FALSE) {
  if (verboseMode) {
    colnames <- colnames(dataset)
    for (i in (1:length(colnames))){
      cat(paste(i, colnames[i]),"\n")
    }
  }
}

#  call pkg_install with the list of package names
pkg_install(c("dplyr" , "e1071",  "cluster", "rpart", "rpart.plot","plyr",
              "neuralnet", "Metrics","stringr","ggcorrplot","corrplot", "FactoMineR","party", "arules", "randomForest"))

# set working directory to project root. Needed to access data files
setwd("E:/__Brunel/__Big Data analytics/R scripts")

######################################################################
#1. Loading and wrangling data

# Load referendum results, per town. Conveniently, the first line in the file contains the columns' headers
# We set the stringsAsFactors argument to FALSE as we dont want strings to eb considered as categorical variables  
RefResults <- read.csv("..\\Input data\\ResultsPerTownC.txt", header = TRUE, sep = "\t", fileEncoding="utf-16", stringsAsFactors=FALSE)
# Load localities portrait. Header is included too. Contains socio-economics and political details per locality
CommunPortrait <- read.csv("..\\Input data\\CommunesPortraitC.txt", header = TRUE, sep = "\t", fileEncoding="utf-16", stringsAsFactors=FALSE)
# Load language exceptions, i.e. localities that do not speak the same language as the rest of their cantom
CommunLanguageExcept <- read.csv("..\\Input data\\CommunesLanguageExceptions.txt", header = TRUE, sep = "\t", stringsAsFactors=FALSE)

#-------------------------------------------------------- JOIN ------------------------------------------------------
#Join the two table: inner join is performed as lines with partial information is not needed (and is marginal)
ds <- inner_join(RefResults, CommunPortrait, by ='No.commune')
#Mergin with language exceptions: 
ds <- left_join(ds,CommunLanguageExcept, by ='No.commune')

#Adding language information by cantons. D=German, F=French, I=Italian
ds[is.na(ds$Language),"Language"] <- "D" # As most communes speak German, we assign this value to all where missing
ds[ds$Canton=="Ticino","Language"] <- "I"
ds[ds$Canton=="Genève","Language"] <- "F"
ds[ds$Canton=="Neuchâtel","Language"] <- "F"
ds[ds$Canton=="Vaud","Language"] <- "F"

# Correct column names, to make them understandable, after R has replaced some caracters at import (like %)
colnames(ds)[colnames(ds)=="Yes.in.."] <- "Yes.in.perc"
colnames(ds)[colnames(ds)=="X.y"] <- "Commune.nb"
colnames(ds)[colnames(ds)=="Foreign.nationals.in.."] <- "Foreign.nationals.rate"
colnames(ds)[colnames(ds)=="Settlement.and.urban.area.in.."] <- "Settlement.and.urban.area.in.perc"
colnames(ds)[colnames(ds)=="Agricultural.area.in.."] <- "Agricultural.area.in.perc"
colnames(ds)[colnames(ds)=="Wooded.area.in.."] <- "Wooded.area.in.perc"
colnames(ds)[colnames(ds)=="Unproductive.area.in.."] <- "Unproductive.area.in.perc"

# Checking dimensions and basic statistics
if (verboseExecution) {
  #Referendum file contains 2228 towns (localities)
  dim(RefResults)     # Check number of rows and columns imported
  str(RefResults)     # Check that column types are what we expect. As it is fine, no need to force any to the expected type.
  summary(RefResults) # Check values disribution and basic statistics

  #Towns "portrait" file contains 2324 towns (localities)
  dim(CommunPortrait) # Check number of rows and columns imported
  str(CommunPortrait) # Check that column types are what we expect.

  summary(CommunPortrait) # Check values disribution and basic statistics
  dim(ds)
  # common lines are 2200
  
  # we check for consistency by getting total number of voters, taht is also provided in original source excel file
  sum(ds["Total.voters"])
}

# Removing any columns that are duplicated (other common columns in the 2 input files)
# As well as columns taht are useless in our analysis (changes in size size or numbers of citizen)
# and columns that are redundant, i.e. sum or % of values in other columns, as they don't
# bring any additional information
# 1 X.x 
# 2 Commune 
# 3 Total.voters 
# 4 Total.ballots 
# 5 Blank.ballots 
# 6 Invalid.ballots 
# 7 Valid.ballots 
# 8 No 
# 9 Commune.nb 
# 10 Change.in.. 
# 11 Change.in.ha 
# 12 Change.in.ha.1 
# 13 Employed.total 
# 14 Business.establishments.total (41)
# 15 New.housing.units.per.1000.residents (46)
# 16 Nom.Commune (62)

dropedCols <- c(1,5,6,7,9,10,11,13,15,18,32,34,37,41, 46, 62)
if (verboseExecution) {
  cat("Removing following columns:\n")
  printCols(ds[,dropedCols])               
}
ds <- ds[,-dropedCols]

######################################################################
# 2. Exploratory Analysis

# Data Preparation
# inspect the dataset, check dimensions, values range and distribution
dim(ds)
str(ds)
summary(ds)

# Checking for duplicates 
table(duplicated(ds)) # No duplicates found


if (verboseExecution) {
  # We check the top columns where the most important variance is observed
  list_std_dev <- apply(ds[8:47,],2,sd)
  head(sort(list_std_dev,decreasing = TRUE), 20)
}

# Basic statitics
# check 5 largest towns, as this dimension shows high variance and skewness
ds[(ds$Residents>100000),]
# total population
sum(ds$Residents) # 8097473
#average number of residents per locality
sum(ds$Residents)/length(ds) #172286.7

# generating a density plot for the votes to get an idea
plot(density(ds$Yes.in.perc), xlab="...")
# check the distribution by canton, to get a flavor
bp <- ggplot(ds, aes(x = Canton, y = Yes.in.perc)) +
  geom_boxplot(aes(colour = Canton), show.legend = FALSE)+
  theme(axis.text.x = element_text(angle =45))
bp
# at this stage, we can make faceted boxplots per language, to test if it plays a role
bp <- ggplot(ds, aes( y = Yes.in.perc)) +
  geom_boxplot(aes(colour = Language))+ facet_grid(. ~ as.factor(ds$Language))
bp

# As variance is very important in large towns (population or goegraphy), we fix this
# by applying a percentage of population measure instead of absolute number
ds$Primary.sector.emp <- ds$Primary.sector.emp/ds$Residents
ds$Secondary.sector.emp <- ds$Secondary.sector.emp/ds$Residents
ds$Tertiary.sector.emp <- ds$Tertiary.sector.emp/ds$Residents
ds$under.the.Foreign.Nationals.Act..FNA.<- ds$under.the.Foreign.Nationals.Act..FNA./ds$Residents
ds$under.the.Narcotics.Act..NarcA.<- ds$under.the.Narcotics.Act..NarcA./ds$Residents
ds$under.the.Swiss.Criminal.Code<- ds$under.the.Swiss.Criminal.Code/ds$Residents

if (verboseExecution) {
  # check about skewness for number of residents dimension
  boxplot(log(ds$Residents))
  summary(log(ds$Residents)-2.5)
  # We check again in what columns the most important variance is observed
  list_std_dev <- apply(ds[8:47,],2,sd)
  head(sort(list_std_dev,decreasing = TRUE), 20)
}

CommunPortrait<- CommunPortrait[,-c(49)]

if (verboseExecution) {
  # check if log value present any skweness any more after log tranformation
  opar <- par(mfrow=c(2,6))
  # original distributions
  boxplot((CommunPortrait$Residents), main="Residents")
  boxplot((CommunPortrait$Private.households), main="Households")
  boxplot((CommunPortrait$Population.density.per.km.sq), main="Pop density")
  boxplot((CommunPortrait$Secondary.sector.establ+1), main="Secondary estab")
  boxplot((CommunPortrait$Tertiary.sector.establ), main="Tertiary estab")
  boxplot((CommunPortrait$Total.surface.area.in.km.sq), main= "area in km2")
  # distributions after log transformation
  boxplot(log(CommunPortrait$Residents), main="log")
  boxplot(log(CommunPortrait$Private.households), main="log")
  boxplot(log(CommunPortrait$Population.density.per.km.sq), main="log")
  boxplot(log(CommunPortrait$Secondary.sector.establ+1), main="log")
  boxplot(log(CommunPortrait$Tertiary.sector.establ), main="log")
  boxplot(log(CommunPortrait$Total.surface.area.in.km.sq), main="log")
  par(opar)
}

# Where a percentage cannot be applied, we use a log function to get a less skewed distribution
# (efficiency of this method has been checked visually above)
ds$Residents <- log(ds$Residents)
ds$Private.households <- log(ds$Private.households)
ds$Population.density.per.km.sq <- log(ds$Population.density.per.km.sq)
ds$Secondary.sector.establ <- log(ds$Secondary.sector.establ+1)
ds$Tertiary.sector.establ <- log(ds$Tertiary.sector.establ)
ds$Total.surface.area.in.km.sq <- log(ds$Total.surface.area.in.km.sq)

# Displaying correlations between numerical features
votCorr <- cor(ds[8:46],use="complete.obs")
opar <- par(mfrow=c(1,1))
corrplot(votCorr, method="color", type="upper", tl.col="black", tl.srt=45,  tl.cex = 0.5 )
par(opar)


######################################################################
# 3. Principal component analysis

# To build PCA dataset, dropping the non-numeric variables and the target features
dropedCols <- c(1,2,3,4,5,6,7)

if (verboseExecution) {
  cat("Removing following columns for PCA:\n")
  printCols(ds[,dropedCols])
}
train_set <- ds[,-dropedCols,]

# Turning the language values into numeric ones (1,2,3)
train_set$Language<-as.numeric(factor(train_set$Language))

# Perform dimension reduction (PCA)
# Scale features since they are measured in different scales, the PCA must be performed with standardized data (mean = 0, variance = 1)
pc_ds <- prcomp(train_set, rank.=3, scale. = TRUE) 
if (verboseExecution) {
  summary(pc_ds)
}
# calculate the proportion of explained variance (PEV) from the std values
pc_var <- pc_ds$sdev^2
pc_var
pc_PEV <- pc_var / sum(pc_var)
pc_PEV

pc_ds_loadings <- pc_ds$rotation
# To understand the 2 mains loadings, print out the 5 main elements, and 5 last ones 
head(pc_ds_loadings[order(pc_ds_loadings[,1], decreasing =TRUE),1])
tail(pc_ds_loadings[order(pc_ds_loadings[,1], decreasing =TRUE),1])
head(pc_ds_loadings[order(pc_ds_loadings[,2], decreasing =TRUE),2])
tail(pc_ds_loadings[order(pc_ds_loadings[,2], decreasing =TRUE),2])

# plotting the explained proportion variance per PC, only 20 first values
opar <- par(mfrow=c(1,2))
barplot(
  pc_PEV[1:20], 
  main="Proportion of explained variance", 
  xlab="Dimensions (PCA)", ylab="PEV",col="lightblue")

plot(
  cumsum(pc_PEV),
  main = "Cummulative PEV",
  ylim = c(0,1),
  xlab = 'Dimensions (PCA)',
  ylab = 'cumulative PEV',
  pch = 20,
  col = 'orange'
)
abline(h = 0.8, col = 'red', lty = 'dashed')
abline(v = 16, col='red', lty = 'dashed') # 16 components needed to explain 80% of variance
par(opar)

#plotting the loadings
opar <- par(mfrow=c(1,1))
colvector = c('red', 'orange', 'yellow', 'green', 'cyan', 'blue', 'violet', 'brown')
labvector = c('PC1', 'PC2', 'PC3')
barplot(
  pc_ds_loadings[,c(1:3)],
  beside = T,
  yaxt = 'n',
  names.arg = labvector,
  col = colvector,
  ylim = c(-1,1),
  border = 'white',
  ylab = 'loadings'
)
axis(2, seq(-1,1,0.1))
legend(
  'bottomright',
  bty = 'n',
  col = colvector,
  pch = 15,
  row.names(pc_ds_loadings),
  cex=0.5
)
par(opar)

# Plot values along the 2 main components
opar = par(mfrow=c(1,1))
biplot(
  pc_ds,
  scale = 3,
  col = c('grey40','orange'), cex=0.5
)
par(opar)

######################################################################
#
#   4. Clustering analysis  
#
######################################################################
# K-Means Clustering   
# Net Income Adjustments and Earning per share fields are used to cluster
# data preparation: all data must be numerical and normalized 
# noramlise the data 
train_set_norm <- as.data.frame(apply(train_set, 2, MinMax))

y <- train_set_norm[,-c(7)]

# distance matrix with euclidian distance
dist_ds <- dist(y, method = 'euclidian')
# k-means algorithm call. Assumption is that there are two clusters we can identify
k_ds <- kmeans(y,centers=2, nstart=20)  
# inspect results
str(k_ds)
table(k_ds$cluster)

# inspect the 2 clusters, to try and make sense of them
cluster1 <-ds[k_ds$cluster==1,]
cluster2 <-ds[k_ds$cluster==2,]

# sort values by number of residents, to check if large towns are part of cluster 1
head(cluster2[order(cluster2$Residents, decreasing =TRUE),c("Name.of.commune","Residents", "Population.density.per.km.sq", "Agricultural.area.in.perc", "Wooded.area.in.perc")],5)
tail(cluster2[order(cluster2$Residents, decreasing =TRUE),c("Name.of.commune","Residents", "Population.density.per.km.sq", "Agricultural.area.in.perc", "Wooded.area.in.perc")],5)
# sort values by population density, to check if coutry-side localities are part of cluster 2
head(cluster1[order(cluster1$Population.density.per.km.sq, decreasing =TRUE),c("Name.of.commune","Residents","Population.density.per.km.sq", "Agricultural.area.in.perc", "Wooded.area.in.perc")],5)
tail(cluster1[order(cluster1$Population.density.per.km.sq, decreasing =TRUE),c("Name.of.commune","Residents","Population.density.per.km.sq", "Agricultural.area.in.perc", "Wooded.area.in.perc")],5)

# As clustering has split the locilities in cities vd country-side, check if this split explains referendum results
opar <-par(mfrow=c(1,2)) 
boxplot(cluster1$Yes.in.perc, ylim = c(0, 60), ylab ="Yes vote (%)", main="Cluster 1")
boxplot(cluster2$Yes.in.perc, ylim = c(0, 60), ylab ="Yes vote (%)", main="Cluster 2")
par(opar)

opar <-par(mfrow=c(1,6)) 
boxplot(cluster1$Residents, ylab ="Residents (log)",main="Clust 1")
boxplot(cluster2$Residents ,main="Clust 2")
boxplot(cluster1$Tertiary.sector.establ, ylab ="Tertiary establ nb (log)",main="Clust 1")
boxplot(cluster2$Tertiary.sector.establ, ylab ="Tertiary establ nb (log)", main="Clust 2")
boxplot(cluster1$Agricultural.area.in.perc, ylab ="Agricultural area (log km2)",main="Clust 1")
boxplot(cluster2$Agricultural.area.in.perc, ylab ="Agricultural area (log km2)",main="Clust 2")
par(opar)


# Plotting the k-means clusters 
opar <- par(mfrow=c(1,1)) 
clusplot(y,
         k_ds$cluster,
         main = paste("Clusters"),
         labels=1,
         shade = TRUE,
         color = TRUE,
         col.p =k_ds$cluster+2,
         line = 0)
par(opar)

######################################################################
#
#   5. Decision tree analysis
#
######################################################################
# Point is to do a regression with decision tree, to check what factors are
# usefull to predict a referendu result and how

# dropping unneeded features, but not target value of course
dropedCols <- c(1,2,3,5,7)
if (verboseExecution) {
  cat("Removing following columns:\n")
  printCols(ds[,dropedCols])   
}
train_set <- ds[,-dropedCols,]

# encode language feature numericaly and 
train_set$Language<-as.numeric(factor(train_set$Language))
# normalize values, so large values dont get largest weight algorithm
train_set_norm <- as.data.frame(apply(train_set, 2, MinMax))

# grow regression tree using rpart() 
fit <- rpart(Yes.in.perc  ~ .,
             method="anova", data=train_set_norm)

printcp(fit) # display the results 
opar <-  par(mfrow=c(1,1)) 
plotcp(fit) # visualize cross-validation results 
par(opar)
summary(fit) # detailed summary of splits

# plot tree 
opar <-  par(mfrow=c(1,1)) 
rpart.plot(fit,
     main="Regression Tree for yes in %",
     varlen = -15,
    cex = 0.75
     )
par(opar)

# calculate Rsquare
rsq.rpart(fit)
opar <- par(mfrow=c(1,2)) 
rsq.rpart(fit)
par(opar)

#Evaluate accuracy
res <- train_set_norm$Yes.in.perc - predict(fit,train_set_norm)
#MSE
mean(res^2) #0.006091819
# RMSE
sqrt(mean(res^2)) #0.07805011
# R-squared
1 - var(res) / var(train_set_norm$Yes.in.perc) #  0.4829105
#plot predicted values vs actual ones
plot(train_set_norm$Yes.in.perc,predict(fit,train_set_norm),col="blue",
     main = "Predicted values vs actual ones")
abline(c(0,1),col=2)

#plot predictions does not make much sense, as it is average values
plot(train_set_norm$Yes.in.perc,predict(fit,train_set_norm),col="blue")

#measure precision using k-fold validation, to check overfitting
errorTst <- vector()
RsqTst <- vector()
errorTrain <- vector()
RsqTrain <- vector()
train_set_norm_l <- train_set_norm%>%select(-2,everything())
for(i in 1:10){
  # split into test and train, using a 90% ratio (remember this is with the k-fold loop)
  index <- sample(1:nrow(train_set_norm_l),round(0.9*nrow(train_set_norm_l))) # Sample with replacement
  train <- train_set_norm_l[index,]
  test <- train_set_norm_l[-index,]
  # grow regression tree using rpart() 
  fit <- rpart(Yes.in.perc  ~ .,
               method="anova", data=train)
  predictionsTest <- predict(fit, test[,-c(42),])   # compute predictions on test data
  predictionsTrain <- predict(fit, train[,-c(42),]) # compute predictions on train data
  residualTst <- test$Yes.in.perc - predictionsTest     # compute residuals on test data
  residualTrain <- train$Yes.in.perc - predictionsTrain # compute residuals on train data
  errorTst[i] <- mean(residualTst^2)                         # compute MSE on test data residuals
  RsqTst[i] <- 1 - (var(residualTst) / var(test$Yes.in.perc))      # compute R^2 on test data residuals and actual values
  errorTrain[i] <- mean(residualTrain^2)                     # compute MSE on train data residuals
  RsqTrain[i] <- 1 - (var(residualTrain) / var(train$Yes.in.perc)) # compute R^2 on test train residuals and actual values
}
# plot results 
opar <-  par(mfrow=c(1,4))
boxplot(errorTrain, main = paste("Dec. Tree Train MSE"), ylim = c(0.005, 0.009))
boxplot(errorTst, main = paste("Dec. Tree Test MSE"), ylim = c(0.005, 0.009))
boxplot(RsqTrain, main = paste("Dec. Tree Train R^2"),ylim = c(0.2, 0.6))
boxplot(RsqTst, main = paste("Dec. Tree Test R^2"),ylim = c(0.2, 0.6))
par(opar)

# Print out results of accuray metrics
cat("Decision tree accuracy results:\n")
cat("Train MSE:", mean(errorTrain[]),"\n")
cat("Test  MSE :", mean(errorTst[]),"\n" )
cat("Train R^2  :", mean(RsqTrain[]),"\n" )
cat("Test  R^2  :", mean(RsqTst[]),"\n" )

#same exercise but without political information, to check what other features are use next
train_set_norm_apolitical <- train_set_norm[,-c(28:38)]
str(train_set_norm_apolitical)
# grow tree 
fit <- rpart(Yes.in.perc  ~ .,
             method="anova", data=train_set_norm_apolitical)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

par(mfrow=c(1,2)) 
rsq.rpart(fit)

# plot tree 
opar <-  par(mfrow=c(1,1)) 
rpart.plot(fit,
           main="Regression Tree for yes in %",
           varlen = -15,
           cex = 0.75
)
par(opar)

#Evaluate accuracy
res <- train_set_norm$Yes.in.perc - predict(fit,train_set_norm_apolitical)
#MSE
mean(res^2) #0.008200373
# RMSE
sqrt(mean(res^2)) #0.07805011
# R-squared
1 - var(res) / var(train_set_norm$Yes.in.perc) #  0.303931

#plot predicted values vs actual ones (ground truth)
plot(train_set_norm$Yes.in.perc,predict(fit,train_set_norm_apolitical),col="blue",
     main = "Predicted values vs actual ones")
abline(c(0,1),col=2)

######################################################################
#
#   6. Random forest analysis
#
######################################################################

# we now use random forest to check features importance
train <- sample(1:nrow(train_set_norm),2000)
set.seed(101) # for reproducibility
rf <- randomForest(Yes.in.perc ~ . , data = train_set_norm , subset = train, importance=TRUE)
print(rf)

opar <-  par(mfrow=c(1,1)) 
plot(rf, main = "Random forest error")
par(opar)

#check where the error is minimal, in terms of 
which.min(rf$mse) #-> 489

#Evaluate variable importance
importance(rf)
varImpPlot(rf, main = "Feature importance in random forest")

plot(train_set_norm$Yes.in.perc,predict(rf,train_set_norm),col="blue",
     main = "Predicted values vs actual ones")
abline(c(0,1),col=2)

######################################################################
#
#   7. Neural networks analysis
#
######################################################################

# Neural network
# to be consistent with usual practice, we move the target feature to the last column
train_set_norm <- train_set_norm%>%select(-2,everything())

# Implementing cross validation in order to measure accury as well as possible
set.seed(2019)
nn.ExecTime <- NULL
k <- 10 # Using 10 folds cross validation
m <- 1 # initial numbeer of neurons in second layer
l <- 5 # Max number of neurons in second layer$

nn.errorTst <- matrix(, nrow = k, ncol = l-m+1)
nn.R2Tst <- matrix(, nrow = k, ncol = l-m+1)
nn.errorTrain <- matrix(, nrow = k, ncol = l-m+1)
nn.R2Train <- matrix(, nrow = k, ncol = l-m+1)

pbar <- create_progress_bar('text') # Implementing progress bar as process takes time
pbar$init(k*(l-m+1)) 
for (j in m:l){
  cat("Training network with ",j,",3 hidden neurons.\n")
  for(i in 1:k){
    # Keep track of time spent
    start.time <- Sys.time()
    # split into test and train, using a 90% ratio (remember this is with the k-fold loop)
    index <- sample(1:nrow(train_set_norm),round(0.9*nrow(train_set_norm))) # Sample with replacement
    train.cv <- train_set_norm[index,]
    test.cv <- train_set_norm[-index,]
    # Perceptron has 2 layers with 3 in the second. First layer has from m to l 
    nn <- neuralnet(Yes.in.perc ~ . ,data=train.cv,hidden=c(j,3)) 
    predictionsTest  <- compute(nn, test.cv[,-c(42),])   # compute predictions on test data
    predictionsTrain <- compute(nn, train.cv[,-c(42),])  # compute predictions on train data
    residualTst   <- test.cv$Yes.in.perc  - predictionsTest$net.result     # compute residuals on test data
    residualTrain <- train.cv$Yes.in.perc - predictionsTrain$net.result    # compute residuals on train data
    nn.errorTst[i,j-m+1]   <- mean(residualTst^2)                          # compute MSE on test data residuals
    nn.errorTrain[i,j-m+1] <- mean(residualTrain^2)                        # compute MSE on train data residuals
    nn.R2Tst[i,j-m+1]      <- 1- var(residualTst)   / var(test.cv$Yes.in.perc)  # compute R^2 on test data residuals and actual values
    nn.R2Train[i,j-m+1]    <- 1- var(residualTrain) / var(train.cv$Yes.in.perc) # compute R^2 on train data residuals and actual values
    pbar$step()             # Advance progress bar
    end.time <- Sys.time()  # Compute elapsed time
    nn.ExecTime[i] <- end.time - start.time
  }
  # Print out results of accuray metrics
  cat("Results for layer:",j,"\n")
  cat("Train MSE  :", mean(nn.errorTrain[,j-m+1]),"\n")
  cat("Test  MSE  :", mean(nn.errorTst[,j-m+1]),"\n" )
  cat("Train RMSE :", sqrt(mean(nn.errorTrain[,j-m+1])),"\n" )
  cat("Test  RMSE :", sqrt(mean(nn.errorTst[,j-m+1])),"\n" )
  cat("Train R^2  :", mean(nn.R2Train[,j-m+1]),"\n" )
  cat("Test  R^2  :", mean(nn.R2Tst[,j-m+1]),"\n" )
  cat("Time spent :", mean(nn.ExecTime), "\n" )
}

# plot results, i.e. accuracy measures for a ANN, as boxplots
opar <-  par(mfrow=c(1,4))
boxplot(nn.errorTrain, main = paste("ANN Train MSE"), names = c(1,2,3,4,5), ylim = c(0.002, 0.01))
boxplot(nn.errorTst, main = paste("ANN Test MSE"), names = c(1,2,3,4,5), ylim = c(0.002, 0.01))
boxplot(nn.R2Train, main = paste("ANN Train R^2"), names = c(1,2,3,4,5),ylim = c(0.2, 0.8))
boxplot(nn.R2Tst, main = paste("ANN Test R^2"), names = c(1,2,3,4,5),ylim = c(0.2, 0.8))
par(opar)

# Environment dump, for reproducibility
#
# sessionInfo()
# R version 3.5.1 (2018-07-02)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=French_Switzerland.1252  LC_CTYPE=French_Switzerland.1252    LC_MONETARY=French_Switzerland.1252
# [4] LC_NUMERIC=C                        LC_TIME=French_Switzerland.1252    
# 
# attached base packages:
#   [1] stats4    grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] rpart.plot_3.0.7    arules_1.6-3        Matrix_1.2-14       party_1.3-3         strucchange_1.5-1  
# [6] sandwich_2.5-0      zoo_1.8-4           modeltools_0.2-22   mvtnorm_1.0-10      FactoMineR_1.41    
# [11] corrplot_0.84       ggcorrplot_0.1.2    ggplot2_3.1.0       stringr_1.3.1       Metrics_0.1.4      
# [16] neuralnet_1.44.2    rpart_4.1-13        cluster_2.0.7-1     e1071_1.7-1         dplyr_0.8.0.1      
# [21] randomForest_4.6-14 tree_1.0-39        
# 
# loaded via a namespace (and not attached):
#   [1] tidyselect_0.2.5     coin_1.3-0           reshape2_1.4.3       purrr_0.3.1          splines_3.5.1       
# [6] lattice_0.20-35      colorspace_1.3-2     survival_2.42-3      rlang_0.3.1          pillar_1.3.1        
# [11] glue_1.3.0           withr_2.1.2          matrixStats_0.54.0   multcomp_1.4-10      plyr_1.8.4          
# [16] munsell_0.5.0        gtable_0.2.0         leaps_3.0            codetools_0.2-15     labeling_0.3        
# [21] parallel_3.5.1       class_7.3-14         TH.data_1.0-10       Rcpp_1.0.0           scales_1.0.0        
# [26] flashClust_1.01-2    scatterplot3d_0.3-41 digest_0.6.18        stringi_1.2.4        tools_3.5.1         
# [31] magrittr_1.5         lazyeval_0.2.1       tibble_2.0.1         crayon_1.3.4         pkgconfig_2.0.2     
# [36] MASS_7.3-50          libcoin_1.0-4        assertthat_0.2.0     R6_2.3.0             compiler_3.5.1 
