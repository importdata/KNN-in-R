# Mini Project 1 - K-Nearest Neighbors (KNN)
# Name: Jaemin Lee
# NetID: JXL142430

library(class)

# open training data csv file
train = read.csv("C:/Users/jaemi/Desktop/STAT 4360/Projects/1-training_data.csv")

table(train$y)  # there are 2000 yes's and no's
summary(train)  # check the summary of the train data

train.x = cbind(train$x.1, train$x.2) # train.x contains columns from x.1 and x.2
train.y = train$y   # train.y contains data from column y

# check the correlation between x.1 and x.2
plot(train.x, xlab = "x.1", ylab = "x.2", col = ifelse(train.y == "yes", "green", "red")) 
  # notice there are two distinct clusters (red and green) on the right side of the graph
  # but there's also a spot mixed with green (yes) and red (no) on the top of the graph,
  # meaning there's a weak relationship between them

# open test data csv file
test = read.csv("C:/Users/jaemi/Desktop/STAT 4360/Projects/1-test_data.csv")
test.x = cbind(test$x.1, test$x.2) # test.x contains conlumns from x.1 and x.2
test.y = test$y # test.y contains data from column y

# (a) Fit KNN with K = 1, 6, ..., 151
ks <- c(seq(1, 151, by=5)) # K's sequence is incremented by 5

# number of Ks
nks <- length(ks) 

# initialize train & test error rates
err.rate.train <- numeric(length = nks) # error rate for train data
err.rate.test <- numeric(length = nks)  # error rate for test data
names(err.rate.train) <- names(err.rate.test) <- ks # get names for train and test data

# loop through the several Ks (1-151)
for (i in seq(along = ks)) {
  set.seed(1) # random number generator
  fit.train <- knn(train.x, train.x, train.y, k = ks[i]) # to fit train error rate with several Ks
  set.seed(1) # used the same random number generator as above
  fit.test <- knn(train.x, test.x, train.y, k = ks[i]) # to fit test error rate with several Ks
  # create arrays for train and test error rates
  err.rate.train[i] <- mean(fit.train != train.y) # train error rate is the mean of the case when fit.train != train.y
  err.rate.test[i] <- mean(fit.test != test.y)  # test error rate is the mean of the case when fit.test != test.y
}

# B) plot KNN against the Error rate for test and training data
plot(ks, err.rate.train, xlab = "Number of nearest neighbors", ylab = "Error rate", 
     type = "b", ylim = range(c(err.rate.train, err.rate.test)), col = "blue", pch = 20) # graph train error rate
lines(ks, err.rate.test, type="b", col="red", pch = 20) # graph test error rate
legend("bottomright", lty = 1, col = c("blue", "red"), legend = c("training", "test"))

# C) to find optimal K and the training and test error rates associated with the optimal K

# create a data frame with several Ks that corresponds to train & test error rates
result <- data.frame(ks, err.rate.train, err.rate.test) 
result[err.rate.test == min(result$err.rate.test), ] # find the optimal K, train & test error rate
                                                     # test error rate is the best when it's a minimum value

# Decision boundary for optimal K = 101
n.grid <- 60 # make the number of grid (sqrt of 4000 is roughly 60)
x1.grid <- seq(f = min(train.x[, 1]), t = max(train.x[, 1]), l = n.grid) # x-axis
x2.grid <- seq(f = min(train.x[, 2]), t = max(train.x[, 2]), l = n.grid) # y-axis
grid <- expand.grid(x1.grid, x2.grid) # 60 by 60 grid

k.opt <- 101 # optimal K = 101
set.seed(1)
fit.opt <- knn(train.x, grid, train.y, k = k.opt, prob = T) # fit KNN using optimal K
prob <- attr(fit.opt, "prob") # prob is voting fraction for winning class
prob <- ifelse(fit.opt == "yes", prob, 1 - prob) # now it is voting fraction for y == "yes"
prob.matrix <- matrix(prob, n.grid, n.grid) # create a probability matrix

# D) plot the training data that shows the decision boundary for the optimal K. 
plot(train.x, col = ifelse(train.y == "yes", "blue", "red")) # if train.y is 'yes" then blue, otherwise red
points(grid, col = ifelse(prob >= 0.5, "blue", "red"), pch = ".", cex = 2) # predict blue if prob >= 0.5, otherwise red
# draw decision boundary
contour(x1.grid, x2.grid, prob.matrix, levels = 0.5, add = T) 
