#### LSTM

# https://towardsdatascience.com/illustrated-guide-to-lstms-and-gru-s-a-step-by-step-explanation-44e9eb85bf21


rm(list = ls()) # clean the environment

# Libraries and custom functions ----
library(dplyr)
library(keras)
library(tensorflow)

lag_transform <- function(x, k = 1) {
    lagged =  c(rep(NA, k), x[1:(length(x) - k)])
    DF = as.data.frame(cbind(lagged, x))
    colnames(DF) <- c( paste0('x-', k), 'x')
    DF[is.na(DF)] <- 0
    return(DF)
}

scale_data = function(train, test, feature_range = c(0, 1)) {
    x = train
    fr_min = feature_range[1]
    fr_max = feature_range[2]
    std_train = ((x - min(x) ) / (max(x) - min(x)))
    std_test  = ((test - min(x)) / (max(x) - min(x)))
    scaled_train = std_train * (fr_max - fr_min) + fr_min
    scaled_test = std_test * (fr_max - fr_min) + fr_min
    return(list(scaled_train = as.vector(scaled_train),
                scaled_test = as.vector(scaled_test),
                scaler = c(min = min(x), max = max(x))))
}

invert_scaling = function(scaled, scaler, feature_range = c(0, 1)) {
    min = scaler[1]
    max = scaler[2]
    t = length(scaled)
    mins = feature_range[1]
    maxs = feature_range[2]
    inverted_dfs = numeric(t)
    for (i in 1:t) {
        X = (scaled[i] - mins)/(maxs - mins)
        rawValues = X * (max - min) + min
        inverted_dfs[i] <- rawValues
    }
    return(inverted_dfs)
}


# Data ----
# load model data; select one cell for this example
Drca <- read.csv("./dataraw/rca_data_2012_2022-06-29.csv") %>%
    filter(CellID == 6185) %>%
    mutate(Date = as.Date(Date)) %>%
    arrange(Date)
y <- Drca$DOAVEG_avg
plot(y)

Y <- lag_transform(y, 1)
head(Y)


# Model ----

# sample size
n = nrow(Y)
# size of the training set
ntr = 200
# forecasting horizon (testing set size)
nh = 10
# number of model and forecasting runs (can be set to reach n)
nr = 5

nbatch = 1 # must be a common factor of both the train and test samples
units = 10 # can adjust this, in model tuning phase
epochs = 50

#### Model LSTM ####
model <- keras_model_sequential()
model %>%
    layer_lstm(units, batch_input_shape = c(nbatch, 1, 1), stateful = TRUE) %>%
    layer_dense(units = 1)

model %>% compile(
    loss = 'mean_squared_error',
    optimizer = 'adam', #optimizer_adam(lr = 0.05, decay = 1e-6),
    metrics = c('accuracy')
)

summary(model)


PRED_lstm = matrix(NA, nr, nh)

for (j in 1:nr) { # j = 1
    # index of data in the training set
    itrain <- 1:ntr + j - 1
    # index of data in the testing set
    itest <- (itrain[ntr] + 1):(itrain[ntr] + nh)
    train <- Y[itrain,]
    test <- Y[itest,]

    ## scale data
    Scaled = scale_data(train, test, c(-1, 1))
    y_train = Scaled$scaled_train[, 2]
    x_train = Scaled$scaled_train[, 1]
    y_test = Scaled$scaled_test[, 2]
    x_test = Scaled$scaled_test[, 1]

    # Reshape the input to 3-dim
    dim(x_train) <- c(ntr, 1, 1)

    for (i in 1:epochs){
        model %>% fit(x_train, y_train, epochs = 1,
                      batch_size = nbatch, verbose = 1, shuffle = FALSE)
        model %>% reset_states()
    }

    #https://www.r-bloggers.com/2018/11/lstm-with-keras-tensorflow/
    # pred_out <- model %>% predict(x_test, batch_size = batch_size) %>% .[,1]
    # plot_ly(myts, x = ~index, y = ~price, type = "scatter", mode = "markers", color = ~vol) %>%
    #   add_trace(y = c(rep(NA, 2000), pred_out), x = myts$index, name = "LSTM prediction", mode = "lines")

    #L = length(x_test)

    ############################ Predict using LSTM ####
    # dim(x_test) = c(length(x_test), 1, 1)
    # predict(model, x_test, batch_size = nbatch)

    for (i in 1:nh) { # i = 1
        X = ifelse(i == 1, x_test[i], yhat0)
        dim(X) = c(1, 1, 1)
        yhat0 = predict(model, X, batch_size = nbatch)
        # invert scaling
        yhat = invert_scaling(yhat0, Scaled$scaler,  c(-1, 1))
        print(c(yhat0, yhat))
        # store
        PRED_lstm[j, i] <- yhat
    }
}

PRED_lstm.df <- as.data.frame(PRED_lstm)


