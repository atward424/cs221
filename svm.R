
# random forest model
library(R.matlab)
library(caret)
library(boot)
library(e1071)
library(randomForest)

proj_dir = 'C:\\Users\\Andrew\\Documents\\Stanford\\cs221\\221project\\'

labels = read.csv(paste0(proj_dir, "data\\train_info.csv")) # stringsAsFactors = F


features = data.frame(readMat(paste0(proj_dir, "featureMat.mat")))

# Load features and labels generated by Matlab from dataset
# y = data.frame(readMat("/Users/daniel/Dropbox/Stanford/2015-2016/Fall 2015/CS 229/Project/Data/features/labels3.mat"))
# X = data.frame(readMat("/Users/daniel/Dropbox/Stanford/2015-2016/Fall 2015/CS 229/Project/Data/features/features3.mat"))


# Set RNG seed for determining cross-validation folds
set.seed(1024)

numPictures = dim(features)[1]
rm(newLabels)
for (i in seq(1,numPictures)) {
  ind = which(labels[,1]==paste0(features[i,1], ".jpg"))
  
  if (!exists("newLabels")) {
    newLabels = labels[ind,]
  }
  else {
    newLabels = rbind(newLabels, labels[ind,])
  }
}

numFeatures = dim(features)[2]

picsToCompare = 20
rands = sample(seq(1,dim(newLabels)[1]), picsToCompare, replace=F)


rm(pairFeatures)
rm(pairLabels)
for (i in 1:picsToCompare) {
  
  firstPicFeatures = features[rands[i], 2:numFeatures]
  for (j in 1:numPictures){
    secondPicFeatures = features[j, 2:numFeatures]
    
    newFeatRow = data.frame(firstPicFeatures, 
                            abs(secondPicFeatures-firstPicFeatures),
                            check.names = T)
    
    if (!exists("pairFeatures")) {
      pairFeatures = newFeatRow
    }
    else {
      pairFeatures = rbind(pairFeatures, newFeatRow)
    }
    
    pairLabRow = cbind(labels[rands[i],1], labels[j,1], 
                       labels[rands[i],]$artist == labels[j,]$artist,
                       labels[rands[i],]$style == labels[j,]$style,
                       labels[rands[i],]$genre == labels[j,]$genre
    )
    
    if (!exists("pairLabels")) {
      pairLabels = pairLabRow
    }
    else {
      pairLabels = rbind(pairLabels, pairLabRow)
    }
  }
}

pairLabels = data.frame(pairLabels)
colnames(pairLabels) = c("pic1", "pic2", "sameArtist", "sameStyle", "sameGenre")



avg.sensitivities = rep(0, 5)
avg.specificities = rep(0, 5)

# runs svm model

# Set up data subset by conditioning on alarm type
# y.alarm = factor((y[(y[,6] == alarm), alarm] + 1)/2)
# X.alarm = X[(y[,6] == alarm),]
# data.alarm = cbind(X.alarm, y.alarm)

# Pseudo-randomly generate cross-validation fold indices
K = 5 # number of CV folds
folds = createFolds(pairLabels$sameArtist, k=K, list=TRUE, returnTrain=FALSE)

C = 100

# Average sensitivity/specificity across all CV folds
avg.sensitivities = matrix(0,  C)
avg.specificities = matrix(0,  C)
# Sensitivity/specificity for each fold, for the current alarm
cv.sensitivities = array(0, c( C, K))
cv.specificities = array(0, c( C, K))

y.pred.factor = array(0, c(length(unlist(folds[1])), K))
# Train/test per fold
for (k in 1:K) {
  
  # Format the current fold indices and separate out the test/train sets
  fold = unlist(folds[k])
  X.train = pairFeatures[-fold,]
  X.test = pairFeatures[fold,]
  y.train = pairLabels$sameArtist[-fold]
  y.test = pairLabels$sameArtist[fold]
  
  # Build the svm regression model using the current training set.
  svm.fit = svm(x = X.train, y = y.train)
  
  # Generate the predictions using the current fold as a validation set.
  y.pred.factor[, k] = predict(svm.fit, X.test, type = "response")
  
}


max.pred = max(y.pred.factor)
min.pred = min(y.pred.factor)
diff.pred = max.pred - min.pred
confidence = seq(from=min.pred - (diff.pred/C), to=max.pred, by=diff.pred/(C+1))

for (k in 1:K) {
  for (c in 1:C) {
    
    
    y.pred.factor2 = factor(y.pred.factor[, k] >= confidence[c])
    
    levels(y.pred.factor2)[levels(y.pred.factor2) == FALSE] = 0
    levels(y.pred.factor2)[levels(y.pred.factor2) == TRUE] = 1
    
    # Convert labels and predictions to factors for the confusion matrix
    y.test.factor = factor(y.test, levels=c(0,1))
    
    # Generate the confusion matrix and extract sensitivity/specificity
    CM = confusionMatrix(y.pred.factor2, y.test.factor)
    cv.sensitivities[c, k] = CM$byClass["Sensitivity"]
    cv.specificities[c, k] = CM$byClass["Specificity"]
  }
}

# Average the specificity/sensitivity over all CV estimates
for (c in 1:C) {
  avg.sensitivities[ c] = mean(cv.sensitivities[ c,])
  avg.specificities[ c] = mean(cv.specificities[ c,])
}


svm_ss = data.frame(sens = avg.sensitivities, spec =  avg.specificities)


