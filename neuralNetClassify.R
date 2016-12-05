
library(R.matlab)
library(caret)
library(boot)
library(e1071)
library(nnet)


# Set RNG seed for determining cross-validation folds
set.seed(1024)

# get a random sample of test pictures
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


featsArt = cbind(features, artist = newLabels$artist)





numHidden = 20
misclass = mat.or.vec(1,numHidden);
trainPredMat = mat.or.vec(numHidden, dim(features)[1]);
# for (sz in seq(1, numHidden)) {
sz = 20

# average over 10 observations for better results
tsumPred = 0;
# sumPred = 0;
for (i in seq(1,10)) {
  set.seed(345 + i)
  artnet <- nnet(artist~., data = featsArt,  
                 size = sz, rang = 0.5, decay = 0.1, 
                 MaxNWts = Inf, maxit = 2000);
  
  # predict on both the training and test set
  tpred <- predict(artnet, featsArt, type = "class");
  tsumPred = c(unlist(tsumPred), tpred)
  #     pred <- predict(spamnet, spam.test, type = "raw")
  #     sumPred = sumPred + pred;
}

# calculate misclassification rates
# avgPred = sumPred/10;
tavgPred = tsumPred/10;
trainPredMat[sz,] = tavgPred;
# testPredMat[sz,] = avgPred;

trueCol = dim(featuresWithArtist)[2]
finalPred = round(tavgPred);
correct = (finalPred == featuresWithArtist[,trueCol]);
misclassification_rate = 1-sum(correct)/length(correct)
falseNeg <- (finalPred == 0 & featuresWithArtist[,trueCol] == 1)
falsePos <- (finalPred == 1 & featuresWithArtist[,trueCol] == 0)
#   spam_misclassification_rate = sum(spam_incorrect)/sum(spam_tot);
#   nspam_misclassification_rate = sum(nspam_incorrect)/sum(nspam_tot);

misclass[sz] = misclassification_rate


# }