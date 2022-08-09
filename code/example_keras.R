########### DL ######################
# If problems with SSL in Windows, copy DLLs as in the 2nd answer
#https://stackoverflow.com/questions/45954528/pip-is-configured-with-locations-that-require-tls-ssl-however-the-ssl-module-in
# Problems with versions on 2021-11-06, force later TF version
#https://stackoverflow.com/questions/69694944/how-to-fix-error-cannot-register-2-metrics-with-the-same-name-tensorflow-api/69831814
# devtools::install_github("rstudio/keras")
library(keras)
library(tensorflow)
tensorflow::install_tensorflow(version = "2.7")

#Simulate a data set
set.seed(1)
n = 100 #number of cases present/absent
w = 10 #window size
props = c(0.9, 0.1) #observed proportions of each class
tmp <- sample(c(0, 1), n, prob = props, replace = TRUE)
x1 <- sapply(tmp, function(x) rnorm(rpois(1, 5), mean = x*2, sd = 1 + x*2))
y <- rep(tmp, times = sapply(x1, length))
x1 <- unlist(x1)
x2 <- x1 * 2 + rnorm(length(x1), sd = 0.1)
x3 <- x1 * 2 + rnorm(length(x1), sd = 0.1)
plot.ts(x1)

D <- cbind(y, x1, x2, x3)
#separate the data into w-sized windows
tmp <- lapply((w/2 + 1):(nrow(D) - w/2), function(x) D[(x-w/2):(x+w/2-1),])
Y <- sapply(tmp, function(x) ifelse(any(x[,"y"] == 1), 1, 0))
trainindex <- 1:round(0.7*length(Y))
DD <- array(do.call(cbind, lapply(tmp, function(x) x[,-1])),
            dim = c(dim(tmp[[1]][,-1]), length(tmp), 1)
)
all(DD[,,1,] == tmp[[1]][,-1])

train_array <- array(data = DD[,,trainindex,], dim  = c(dim(DD[,,trainindex,]), 1))
train_array <- aperm(train_array, c(3,2,1,4)) #reorder dimensions
test_array <- array(data = DD[,,-trainindex,], dim  = c(dim(DD[,,-trainindex,]), 1))
test_array <- aperm(test_array, c(3,2,1,4)) #reorder dimensions

# Fix structure for 2d CNN
train_array %>%
    layer_dropout(rate = 0.25) %>%
    layer_flatten() %>%
    layer_dense(units = 50, activation = "relu") %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 1, activation = "sigmoid")

# Build CNN model
model <- keras_model_sequential()
model %>%
    layer_conv_2d(kernel_size = c(w/5, 2), filter = 32,
                  activation = "relu", padding = "same",
                  input_shape = c(dim(train_array)[c(2,3)], 1), ##c(50, 50, 1),
                  data_format = "channels_last") %>%
    # layer_conv_2d(kernel_size = c(w/5, 2), filter = 32,
    # activation = "relu", padding = "valid") %>%
    # layer_max_pooling_2d(pool_size = 2) %>%
    # layer_dropout(rate = 0.25) %>%

    # layer_conv_2d(kernel_size = c(w/5, 2), filter = 64, strides = 2,
    #               activation = "relu", padding = "same") %>%
    # layer_conv_2d(kernel_size = c(w/5, 2), filter = 64,
    #               activation = "relu", padding = "valid") %>%
    # layer_max_pooling_2d(pool_size = 2) %>%
    # layer_dropout(rate = 0.25) %>%

layer_flatten() %>%
    layer_dense(units = 50, activation = "relu") %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 1, activation = "sigmoid")

summary(model)

model %>% compile(
    loss = 'binary_crossentropy', #"categorical_crossentropy", #
    optimizer = "adam",
    metrics = c('accuracy')
)

history <- model %>% fit(
    x = train_array, y = array(Y[trainindex], dim = c(length(Y[trainindex]), 1)),
    epochs = 30, batch_size = 20,
    validation_split = 0.2
)

plot(history)

# Compute probabilities and predictions on test set
predictions <-  predict_classes(model, test_array)
probabilities <- predict_proba(model, test_array)
#Confusion matrix
table(Y[-trainindex], predictions)
