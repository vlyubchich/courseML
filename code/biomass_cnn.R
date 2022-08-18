rm(list = ls()) # clean Environment

# Packages and functions ----
library(dplyr)
library(keras)
library(tensorflow)

#devtools::install_github("andrie/deepviz")
library(deepviz)

# Data ----
# load biomass
BM <- read.csv("./dataderived/Cell_Proximity.csv") %>%
    mutate(Date = as.Date(Date))
# load model data; select only cells corresponding to the BM
Drca <- read.csv("./dataraw/rca_data_2012_2022-06-29.csv") %>%
    filter(is.element(CellID, unique(BM$CellID))) %>%
    mutate(Date = as.Date(Date))
# for each cell in the BM,
# select a history of the water quality variables
# use w days prior to sampling
w = 30
# and let the variables be 'colors' or channels
# (here using all daily average values, ends_with("_avg"))
n = nrow(BM) # total sample size
# create an empty array to hold data;
# 1 is because we deal with time series (1D data, not 2D like space or image)
# we will select so many, nvar, variables (like colors of an image)
nvar = 5
Drca_array <- array(NA, dim = c(n, 1, w, nvar))
for (i in 1:n) { # i = 1
    drca <- Drca %>%
        filter(CellID == BM$CellID[i]) %>%
        filter(Date <= BM$Date[i] & Date > (BM$Date[i] - w)) %>%
        arrange(Date) %>%
        select(ends_with("_avg"))
    Drca_array[i, 1, , ] <- data.matrix(drca)
}
y <- BM$BM_bv
y_cat <- y > 0 # categorical response (absence/presence of biomass, 0/1)

## Standardize ----
# y <- y / max(y)
y <- scale(y)
# for (j in 1:dim(Drca_array)[4]) {
#     Drca_array[, 1, , j] <- scale(Drca_array[, 1, , j])
# }

## Training and testing sets ----
set.seed(123) # set the seed to create reproducible results
itrain <- sample(n, 0.7*n) # create a training index (70%)
itest <- c(1:n)[-itrain] # create a testing index (30%)

train_x <- Drca_array[itrain,,,,drop = FALSE]
train_y <- y[itrain]
test_x <- Drca_array[itest,,,,drop = FALSE]
test_y <- y[itest]

train_y_cat <- y_cat[itrain]
test_y_cat <- y_cat[itest]

# CNN model ----
# https://towardsdatascience.com/deep-learning-which-loss-and-activation-functions-should-i-use-ac02f1c56aa8
# CNN model
model <- keras_model_sequential()
model %>%
    layer_conv_1d(kernel_size = c(w/5), padding = "same",
                  filters = 16,
                  activation = "relu",
                  input_shape = dim(train_x)[-1],
                  data_format = "channels_last") %>%
    layer_flatten() %>%
    layer_dense(units = 50, activation = "relu") %>%
    # layer_batch_normalization() %>%
    layer_dropout(rate = 0.25) %>%
    layer_dense(units = 1, activation = "linear")
summary(model)

model %>% compile(
    loss = "mean_squared_error", # https://keras.io/api/losses/
    # metric = "root_mean_squared_error", # https://keras.io/api/metrics/
    # optimizer = "adam"
    optimizer = optimizer_adam(learning_rate = 0.0001, decay = 0)
)

history <- model %>%
    fit(x = train_x, y = train_y,
        epochs = 50, batch_size = 10,
        validation_split = 0.2
    )

## Predictions on the test set ----
# note that the response has been normalized
predictions <- predict(model, test_x)
# test errors
e <- test_y - predictions
# RMSE = root mean squared error
sqrt(mean(e^2))
# MAE = mean absolute error
mean(abs(e))

# visualize
plot(test_y, predictions, las = 1)
abline(coef = c(0, 1), col = "blue", lwd = 2)

model %>% plot_model()



# Transfer learning ----

# https://keras.io/guides/transfer_learning/
# https://towardsdatascience.com/understanding-and-coding-a-resnet-in-keras-446d7ff84d33

# the code below doesn't work for our data because of the
# input dimensions that must be at least 32x32 with 3 channels
# we have 1 x 30
base_model <- application_resnet50(
    include_top = FALSE,
    weights = "imagenet",
    input_tensor = NULL,
    input_shape = dim(train_x)[-1], #input dimensions that must be at least 32x32
    pooling = "avg",
    classes = 2
)

#freeze the added layers
for (layer in base_model$layers)
    layer$trainable <- FALSE
# summary(base_model)

model_output <-
    # layer_resizing(height = 300, width = 300, interpolation = "bilinear") %>%
    base_model$output %>%
    layer_flatten(trainable = TRUE) %>%
    layer_dense(units = 512, activation = "relu", trainable = TRUE) %>%
    # layer_batch_normalization(trainable = TRUE) %>%
    layer_dropout(rate = 0.5, trainable = TRUE) %>%
    layer_dense(units = 1, trainable = TRUE, activation = "sigmoid")

model <- keras_model(inputs = base_model$input, outputs = model_output)

# summary(model)
# Proceed fitting the model as usual.
