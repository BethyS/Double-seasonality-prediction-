series_to_supervised <-  function(dataset, Lookback, delay)
{
  dimensions <- length(dim(dataset)) - 1
  if (dimensions  == 1)
  {
    dataset <-
      as.data.frame(dataset[1:nrow(dataset), ])#((nrow(dataset)-delay+1))
    input = array(0,  c((nrow(dataset) - Lookback - delay + 1), Lookback))
    output = array(0, c((nrow(dataset) - Lookback - delay + 1), delay))
    for (j in 1:Lookback) {
      input[, j] <- dataset[j:(nrow(dataset) - Lookback - delay + j), 1]
    }
    for (i in 1:delay) {
      output[, i] <- dataset[(Lookback + i):(nrow(dataset) - (delay - i)), 1]
    }
    s = list(as.data.frame(input), as.data.frame(output))
  } else if (dimensions == 2)
  {
    dataset <- as.data.frame(dataset)
    # the input data dimensions are as a form of (feature1, feature2,time)
    input = array(0, c((nrow(dataset) - delay), Lookback, ncol(dataset)))
    output = array(0, c((nrow(dataset) - delay), delay, ncol(dataset)))
    if (delay > 1) {
      decod = array(0, c((nrow(dataset) - delay), delay, ncol(dataset)))
    }
    for (j in seq(Lookback, 1,-1))
    {
      if ((nrow(dataset) - delay - j + 1) >= 1) {
        temp <- as.matrix(dataset[(1:(nrow(dataset) - delay - j + 1)),])
        input[(j:(nrow(dataset) - delay)), ((Lookback + 1) - j), ] = temp
        
      }
    }
    for (k in seq(1, delay, 1))
    {
      temp <- as.matrix(dataset[(1 + k):(nrow(dataset) - delay + k), ])
      output[, k, ] = temp
      if ((delay > 1) && (k < delay)) {
        temp <- as.matrix(dataset[(1 + k):(nrow(dataset) - delay + k), ])
        decod[, (k + 1), ] = temp
      }
    }
    if (delay > 1) {
      s = list(input, output, decod)
    } else {
      s = list(input, output)
    }
    
  } else if (dimensions == 3)
  {
    # the input data dimensions are as a form of (feature1, feature2,time)
    input = array(0, c((nrow(dataset) - delay), Lookback, ncol(dataset), ncol(dataset[1, , ])))
    output = array(0, c((nrow(dataset) - delay), delay, ncol(dataset), ncol(dataset[1, , ])))
    decod = array(0, c((nrow(dataset) - delay), delay, ncol(dataset), ncol(dataset[1, , ])))
    for (j in seq(Lookback, 1,-1))
    {
      if ((nrow(dataset) - delay - j + 1) >= 1) {
        input[(j:(nrow(dataset) - delay)), ((Lookback + 1) - j), , ] = dataset[(1:(nrow(dataset) - delay - j + 1)), ,]
      }
    }
    for (k in seq(1, delay, 1))
    {
      output[, k, , ] = dataset[(1 + k):(nrow(dataset) - delay + k), , ]
      if ((delay > 1) && (k < delay)) {
        decod[, (k + 1), , ] = dataset[(1 + k):(nrow(dataset) - delay + k), , ]
      }
    }
    if (delay > 1) {
      s = list(input, output, decod)
    } else {
      s = list(input, output)
    }
  } else {
    warning("unsupported input dimensions")
    break
  }
  s
}
