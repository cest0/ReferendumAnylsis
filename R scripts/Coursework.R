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
verboseExecution <- FALSE

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

# Prints the list of columns in the dataset passed as parameter along with their index
# Usefull to check the content of the dataset when it has many columns for debugging purposes
printCols <- function(dataset){
  colnames <- colnames(dataset)
  for (i in (1:length(colnames))){
    cat(paste(i, colnames[i]),"\n")
  }
}


#  call pkg_install with the list of package names
pkg_install(c("dplyr" , "e1071",  "cluster", "test",
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
# inspect the dataset
dim(ds)
str(ds)
summary(ds)
# we check there is not NA value in our dataset

# Checking for duplicate and missing values
table(duplicated(ds))

# generating a barplot for the votes
plot(density(ds$Yes.in..), xlab="...")

#check 5 largest towns
CommunPortrait[(CommunPortrait$Residents>100000),]

# We check in waht coumns the most important variance is observed
list_std_dev <- apply(ds[8:47,],2,sd)
head(sort(list_std_dev,decreasing = TRUE), 20)

#Resize column as percentage of total population in commune
#asPercentageOfPop <- function(colNameList, dataset, populationColName){
#  head(dataset[,populationColName])
#  for (colName in colNameList){
#    dataset$colName <- dataset$colName/dataset$populationColName
#  }
#  return(dataset)
#}
# As variance is very important in large towns (population or goegraphy), we try to fix this

# we set all population numbers as percentage (employment sectors)
#colsToResize <- c("Primary.sector.emp", "Secondary.sector.emp", "Tertiary.sector.emp", "Employed.total", 
#                  "under.the.Foreign.Nationals.Act..FNA.", "under.the.Narcotics.Act..NarcA.", 
#                  "under.the.Swiss.Criminal.Code")

#asPercentageOfPop(colsToResize, CommunPortrait, "Residents")
CommunPortrait$Primary.sector.emp <- CommunPortrait$Primary.sector.emp/CommunPortrait$Residents
CommunPortrait$Secondary.sector.emp <- CommunPortrait$Secondary.sector.emp/CommunPortrait$Residents
CommunPortrait$Tertiary.sector.emp <- CommunPortrait$Tertiary.sector.emp/CommunPortrait$Residents
CommunPortrait$Employed.total <- CommunPortrait$Employed.total/CommunPortrait$Residents
CommunPortrait$under.the.Foreign.Nationals.Act..FNA.<- CommunPortrait$under.the.Foreign.Nationals.Act..FNA./CommunPortrait$Residents
CommunPortrait$under.the.Narcotics.Act..NarcA.<- CommunPortrait$under.the.Narcotics.Act..NarcA./CommunPortrait$Residents
CommunPortrait$under.the.Swiss.Criminal.Code<- CommunPortrait$under.the.Swiss.Criminal.Code/CommunPortrait$Residents

if (verb) {
  boxplot(log(CommunPortrait$Residents))
  summary(log(CommunPortrait$Residents)-2.5)
}

CommunPortrait$Residents <- log(CommunPortrait$Residents)
# not used for now: categorical size of towns
#CommunPortrait$TownSize<-cut((CommunPortrait$Residents)-2.5, seq(0,10,1))
#summary(CommunPortrait$TownSize)
#str(CommunPortrait$TownSize)
#barplot(prop.table(table(CommunPortrait$TownSize)))
CommunPortrait<- CommunPortrait[,-c(49)]
#CommunPortrait$TownSize <- discretize(CommunPortrait$Residents, method = "interval", breaks = 10)
#head(CommunPortrait$TownSize)

if (verb) {
  boxplot(log(CommunPortrait$Private.households))
  boxplot(log(CommunPortrait$Population.density.per.km.sq))
  boxplot(log(CommunPortrait$Secondary.sector.establ+1))
  boxplot(log(CommunPortrait$Tertiary.sector.establ))
  boxplot(log(CommunPortrait$Total.surface.area.in.km.sq))
}

# Where a percentage cannot be applied, we use a log function to get a less skewed distribution
CommunPortrait$Private.households <- log(CommunPortrait$Private.households)
CommunPortrait$Population.density.per.km.sq <- log(CommunPortrait$Population.density.per.km.sq)
summary(CommunPortrait$Secondary.sector.establ)
CommunPortrait$Secondary.sector.establ <- log(CommunPortrait$Secondary.sector.establ+1)
CommunPortrait$Tertiary.sector.establ <- log(CommunPortrait$Tertiary.sector.establ)
CommunPortrait$Total.surface.area.in.km.sq <- log(CommunPortrait$Total.surface.area.in.km.sq)


# Displaying correlations
votCorr <- cor(ds[8:46],use="complete.obs")
corrplot(votCorr, method="color", type="upper", tl.col="black", tl.srt=45,  tl.cex = 0.5 )

printCols(ds)
# dropping the factor variables and dealing with any N/A before PCA, and the target features
dropedCols <- c(1,2,3,4,5,6,7) #8) #,47)
cat("Removing following columns:\n")
printCols(ds[,dropedCols])   
train_set <- ds[,-dropedCols,]
table(train_set$Language)

#############################################
train_set$Language<-as.numeric(factor(train_set$Language))
printCols(train_set)

str(train_set)

list_std_dev <- apply(train_set,2,sd)
head(sort(list_std_dev,decreasing = TRUE))

#train_set <- data.frame(t(na.omit(t(ds[-c(1,2,3,6,7)]))))
#train_set <-as.numeric(levels(train_set))[train_set]
#test_set = data.frame(t(na.omit(t(test_HPStocks[-c(1,2,67,68,69,70,71,72)])))
head(train_set)
str(train_set)
# Perform dimension reduction
# PCA
# Scale features since they are measured in different scales, the PCA must be performed with standardized data (mean = 0, variance = 1)

pc_ds <- prcomp(train_set, rank.=3, scale. = TRUE) 
summary(pc_ds)
# calculate the proportion of explained variance (PEV) from the std values
pc_var <- pc_ds$sdev^2
pc_var
pc_PEV <- pc_var / sum(pc_var)
pc_PEV
# plotting the explained proportion variance per PC, only 20 first values
barplot(pc_PEV[1:20], main="proportion of explained variance", xlab="Dimensions (PCA)", ylab="PEV",col="lightblue")

#plotting the loadings
pc_ds_loadings <- pc_ds$rotation
head(pc_ds_loadings[order(pc_ds_loadings[,1], decreasing =TRUE),1])
tail(pc_ds_loadings[order(pc_ds_loadings[,1], decreasing =TRUE),1])
head(pc_ds_loadings[order(pc_ds_loadings[,2], decreasing =TRUE),2])
tail(pc_ds_loadings[order(pc_ds_loadings[,2], decreasing =TRUE),2])

opar <- par()
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

opar <- par()
plot(
  cumsum(pc_PEV),
  ylim = c(0,1),
  xlab = 'PC',
  ylab = 'cumulative PEV',
  pch = 20,
  col = 'orange'
)
abline(h = 0.8, col = 'red', lty = 'dashed')
abline(v = 17, col='red', lty = 'dashed')
par(opar)

opar = par()
biplot(
  pc_ds,
  scale = 3,
  col = c('grey40','orange'), cex=0.5
)
par(opar)

#  defining MinMax function
MinMax <- function(x){
  tx <- (x - min(x)) / (max(x) - min(x))
  return(tx)
}
train_set_norm <- as.data.frame(apply(train_set, 2, MinMax))
summary(train_set_norm)
#------------------------------------ Clustering ---------------------
# K-Means Clustering   
# Net Income Adjustments and Earning per share fields are used to cluster
# data preparation: all data must be numerical and normalized -> OK
y <- train_set_norm[,-c(7)]

# distance matrix with euclidian distance
dist_ds <- dist(y, method = 'euclidian')
# k-means 
k_ds <- kmeans(y,centers=2, nstart=20)  
str(k_ds)

table(k_ds$cluster)

cluster1 <-ds[k_ds$cluster==1,]
cluster2 <-ds[k_ds$cluster==2,]
#cluster3 <-ds[k_ds$cluster==3,]
head(cluster1[order(cluster1$Residents, decreasing =TRUE),c("Name.of.commune","Residents", "Population.density.per.km.sq", "Agricultural.area.in.perc", "Wooded.area.in.perc")],5)
tail(cluster1[order(cluster1$Residents, decreasing =TRUE),c("Name.of.commune","Residents", "Population.density.per.km.sq", "Agricultural.area.in.perc", "Wooded.area.in.perc")],5)

head(cluster2[order(cluster2$Population.density.per.km.sq, decreasing =TRUE),c("Name.of.commune","Residents","Population.density.per.km.sq", "Agricultural.area.in.perc", "Wooded.area.in.perc")],5)
tail(cluster2[order(cluster2$Population.density.per.km.sq, decreasing =TRUE),c("Name.of.commune","Residents","Population.density.per.km.sq", "Agricultural.area.in.perc", "Wooded.area.in.perc")],5)
#head(cluster3[order(cluster3$Population.density.per.km.sq, decreasing =TRUE),c("Name.of.commune","Residents","Population.density.per.km.sq", "Agricultural.area.in.perc", "Wooded.area.in.perc")],10)

par(mfrow=c(1,3)) 
boxplot(cluster1$Yes.in.., ylim = c(0, 60))
boxplot(cluster2$Yes.in.., ylim = c(0, 60))

# Plotting the k-means cluster 
par(mfrow=c(1,1)) 
clusplot(y,
         k_ds$cluster,
         main = paste("Clusters"),
         #ylab = "yyy",
         #xlab = "xxxx",
         shade = TRUE,
         color = TRUE,
         line = 0)
# col=km$cluster

summary(k_ds$cluster)
#3 supervised learning
# Classification Tree with rpart
library(rpart)

# dropping unneeded features, but not target value of course
dropedCols <- c(1,2,3,5,7)
cat("Removing following columns:\n")
printCols(ds[,dropedCols])   
train_set <- ds[,-dropedCols,]
str(train_set)

train_set$Language<-as.numeric(factor(train_set$Language))
train_set_norm <- as.data.frame(apply(train_set, 2, MinMax))
# grow tree 
fit <- rpart(Yes.in..  ~ .,
             method="anova", data=train_set_norm)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
par(mfrow=c(1,1)) 
plot(fit, uniform=TRUE, 
     main="Classification Tree for yes in %")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
# calculate Rsquare
rsq.rpart(fit)

par(mfrow=c(1,2)) 
rsq.rpart(fit)

#Evaluate accuracy
res <- train_set_norm$Yes.in.. - predict(fit,train_set_norm)
#MSE
mean(res^2) #0.006091819
# RMSE
sqrt(mean(res^2)) #0.07805011

# R-squared
1 - var(res) / var(train_set_norm$Yes.in..) #  0.4829105

#plot predictions does not make much sense, as it is average values
plot(train_set_norm$Yes.in..,predict(fit,train_set_norm),col="blue")

#same excercice but without political information
train_set_norm_apolitical <- train_set_norm[,-c(28:38)]
str(train_set_norm_apolitical)
# grow tree 
fit <- rpart(Yes.in..  ~ .,
             method="anova", data=train_set_norm_apolitical)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

par(mfrow=c(1,2)) 
rsq.rpart(fit)

# plot tree 
par(mfrow=c(1,1)) 
plot(fit, uniform=TRUE, 
     main="Classification Tree for yes in %")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


library(party)
# par <- ctree_control(minsplit=20, minbucket=10)
arbre <- ctree(Yes.in.. ~ ., data = train_set_norm_apolitical)
arbre
plot(arbre, cex=.5)

# we now use random forest to check features importance
train <- sample(1:nrow(train_set_norm),2000)
set.seed(101) # for reproducibility
rf <- randomForest(Yes.in.. ~ . , data = train_set_norm , subset = train)
print(rf)

res <- train_set_norm$Yes.in.. - predict(rf,train_set_norm)
#MSE
mean(res^2) #0.001169949
# RMSE
sqrt(mean(res^2)) #0.03420452

# R-squared
1 - var(res) / var(train_set_norm$Yes.in..) #  0.9006967


plot(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)

plot(train_set_norm$Yes.in..,predict(rf,train_set_norm),col="blue")
abline(c(0,1),col=2)

qqplot(res)

# Neural network
# fit neural network
nn <- neuralnet(Yes.in.. ~ . , data = train_set_norm , hidden=5,act.fct = "logistic", linear.output = TRUE)

# plot neural network
plot(nn)

res <- train_set_norm$Yes.in.. - predict(nn,train_set_norm)
#MSE
mean(res^2) # 0.003506525
# RMSE
sqrt(mean(res^2)) # 0.05921592

# R-squared
1 - var(res) / var(train_set_norm$Yes.in..) # 0.702357 

# fit neural network
nn <- neuralnet(Yes.in.. ~ . , data = train_set_norm , hidden=c(9,5), linear.output=FALSE, threshold=0.01)

# plot neural network
plot(nn)

res <- train_set_norm$Yes.in.. - predict(nn,train_set_norm)
#MSE
mean(res^2) # 0.002229739
# RMSE
sqrt(mean(res^2)) # 0.04722012

# R-squared
1 - var(res) / var(train_set_norm$Yes.in..) # 0.8107339

# fit neural network
nn <- neuralnet(Yes.in.. ~ . , data = train_set_norm , hidden=c(11,7), linear.output=FALSE, threshold=0.01)

# plot neural network
plot(nn)

res <- train_set_norm$Yes.in.. - predict(nn,train_set_norm)
#MSE
mean(res^2) # 0.001881572
# RMSE
sqrt(mean(res^2)) # 0.04337709

# R-squared
1 - var(res) / var(train_set_norm$Yes.in..) # 0.8402873

# fit neural network
nn <- neuralnet(Yes.in.. ~ . , data = train_set_norm , hidden=c(15,7), linear.output=FALSE, threshold=0.01)

# plot neural network
plot(nn)

res <- train_set_norm$Yes.in.. - predict(nn,train_set_norm)
#MSE
mean(res^2) # 0.001881572
# RMSE
sqrt(mean(res^2)) # 0.04337709

# R-squared
1 - var(res) / var(train_set_norm$Yes.in..) # 0.8402873

plot(train_set_norm$Yes.in..,predict(nn,train_set_norm),col="blue")
abline(c(0,1),col=2)
