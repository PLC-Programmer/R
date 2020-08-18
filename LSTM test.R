# LSTM test.R
#
# PLC-Programmer, 09.08.2020, 10.08.2020, 12.08.2020
#
#
# this code is basically copied from: http://rwanjohi.rbind.io/2018/04/05/time-series-forecasting-using-lstm-in-r/
#
# env.: R version 4.0.2 (2020-06-22) -- "Taking Off Again"
#       Platform: x86_64-w64-mingw32/x64 (64-bit)
#
# see Wiki for needed installation of python, miniconda, tensorflow, keras in Windows 10:
#          https://github.com/PLC-Programmer/R/wiki/R:-How-to-get-a-Long-short-term-memory-(LSTM)-neural-network-running-on-Windows-10:-installation-guide-and-an-example-program-in-R
#
#
# test: OK
#


library(keras)
library(tensorflow)

library(tidyquant)  # loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR, %>%
library(ggplot2)


Series_name <- "A real life time series"
Series <- c(1092,
            2367,
            575,
            433,
            1605,
            607,
            514,
            1678,
            293,
            280,
            1130,
            367,
            685,
            2263,
            635,
            312,
            841,
            203
            )

start_date  <- as.Date("2019-02-01", "%Y-%m-%d")
deliv_date  <- seq(start_date, by = "month", length.out = length(Series))



train_ratio <- 0.7  # what is the ratio of training data? --> 70%

diffed = diff(Series, differences = 1)

lag_transform <- function(x, k= 1){

      lagged =  c(rep(NA, k), x[1:(length(x)-k)])
      DF = as.data.frame(cbind(lagged, x))
      colnames(DF) <- c( paste0('x-', k), 'x')
      DF[is.na(DF)] <- 0
      return(DF)
}

supervised = lag_transform(diffed, 1)

invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)

  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}



N = nrow(supervised)
n = round(N * train_ratio, digits = 0)
train = supervised[1:n, ]
test  = supervised[(n+1):N, ]

scale_data = function(train, test, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  std_test  = ((test - min(x) ) / (max(x) - min(x)  ))

  scaled_train = std_train *(fr_max -fr_min) + fr_min
  scaled_test = std_test *(fr_max -fr_min) + fr_min

  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
}


Scaled = scale_data(train, test, c(-1, 1))

y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]

y_test = Scaled$scaled_test[, 2]
x_test = Scaled$scaled_test[, 1]


dim(x_train) <- c(length(x_train), 1, 1)

# specify required arguments
X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1                # must be a common factor of both the train and test samples
units = 1                     # can adjust this, in model tuninig phase



# miniconda and python and tensorflow installed?
model <- keras_model_sequential()

model %>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE) %>%
  layer_dense(units = 1)


model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),
  metrics = c('accuracy')
)


summary(model)
# Model: "sequential"
# _________________________________________________________________________________________________________
# Layer (type)                                   Output Shape                              Param #
# =========================================================================================================
# lstm (LSTM)                                    (1, 1)                                    12
# _________________________________________________________________________________________________________
# dense (Dense)                                  (1, 1)                                    2
# =========================================================================================================
# Total params: 14
# Trainable params: 14
# Non-trainable params: 0
# _________________________________________________________________________________________________________
#


# this takes some time:
Epochs = 50
for(i in 1:Epochs) {
  model %>% fit(x_train, y_train, epochs = 1, batch_size = batch_size, verbose = 1, shuffle = FALSE)
  model %>% reset_states()
}


L = length(x_test)
scaler = Scaled$scaler
predictions = numeric(L)

for(i in 1:L) {
     X = x_test[i]
     dim(X) = c(1,1,1)
     yhat = model %>% predict(X, batch_size = batch_size)
     # invert scaling
     yhat = invert_scaling(yhat, scaler,  c(-1, 1))
     # invert differencing
     yhat  = yhat + Series[(n+i)]
     # store
     predictions[i] <- yhat
}



######################################
#
# plotting 3 lines: ......

# some data preparations:


Series_df       <- data.frame(Series)

Series_train_df <- data.frame(c(Series_df[1:(n+1), ], rep(NA, N - n)))

Series_test_df  <- data.frame(c(rep(NA, n), Series_df[(n+1):(N+1), ]))

Series_pred_df  <- data.frame(c(rep(NA, n), Series_df[n+1, ], predictions))


Series_df2 <- data.frame(cbind(deliv_date,
                               Series_train_df,
                               Series_test_df,
                               Series_pred_df))
colnames(Series_df2) <- c("date", "train", "test", "pred")


plot_title  <- paste(Series_name)

ylabel      <- paste("deliveries")

xlabel      <- paste("month since deliveries started")
xlabel      <- paste(xlabel, "\nblack: training data")
xlabel      <- paste(xlabel, "\nblue: test data")
xlabel      <- paste(xlabel, "\nred: prediction with LSTM:\n• Long short-term memory on python, miniconda, tensorflow, keras")


p1 <- Series_df2 %>%
        ggplot(aes(x = deliv_date)) +

        geom_line(aes(y = test),  colour = "blue",  size = 0.4, alpha = 1.0) +
        geom_point(aes(y = test), colour = "blue") +

        geom_line(aes(y = pred),  colour = "red",   size = 0.4, alpha = 1.0) +
        geom_point(aes(y = pred), colour = "red") +

        geom_line(aes(y = train),  colour = "black", size = 0.4, alpha = 1.0) +
        geom_point(aes(y = train), colour = "black") +


        ggtitle(plot_title) +

        labs(# subtitle = plot_subtitle_total,
             x = xlabel, y = ylabel) +

        theme_tq() +

        theme(
          plot.title = element_text(size = 13)
        )

print(p1)


# end of LSTM test.R