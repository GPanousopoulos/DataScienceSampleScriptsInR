# Shuffle 6 cross-validation sets:
# The shuffled dataset is assumed to be loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Initialize the accs vector
accs <- rep(0,6)

for (i in 1:6) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):((i*round((1/6) * nrow(shuffled))))
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  tree <- rpart(Survived ~ ., train, method = "class")
  
  # Make a prediction on the test set using tree
  pred <- predict(tree, test, type="class")
  
  # Assign the confusion matrix to conf
  conf <- table(test$Survived, pred)
  print(conf)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- (conf[1,1] + conf[2,2])/(conf[1,1]+conf[2,1] + conf[1,2] + conf[2,2])
}

# Print out the mean of accs
mean(accs)