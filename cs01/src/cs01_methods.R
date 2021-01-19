processLine <- function(x)
{
  tokens = strsplit(x, "[;=,]")[[1]]
  if (length(tokens) == 10)
    return(NULL)
  tmp = matrix(tokens[- (1:10)], ncol = 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6, byrow = TRUE), tmp)
}

roundOrientation = function(angles) {
  refs = seq(0, by = 45, length = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}

readData <- function(x)
{
  
  txt <- readLines(x)
  lines = txt[substr(txt, 1, 1) != "#"]
  options(error = recover, warn = 1)
  tmp = lapply(lines, processLine)
  offline = as.data.frame(do.call("rbind", tmp),stringsAsFactors = FALSE)
  names(offline) = c("time", "scanMac", "posX", "posY", "posZ",
                     "orientation", "mac", "signal",
                     "channel", "type")
  
  numVars = c("time", "posX", "posY", "posZ",
              "orientation", "signal")
  #factors = c("mac", "channel", "scanMac","type")
  
  offline[numVars] = lapply(offline[numVars], as.numeric)
  
  #offline[factors] = lapply(offline[factors], factor) 
  
  offline = offline[offline$type == "3",]
  offline$rawTime = offline$time
  offline$time = offline$time/1000
  class(offline$time) = c("POSIXt", "POSIXct")
  offline$angle = roundOrientation(offline$orientation)
  
  subMacs <- names(sort(table(offline$mac), decreasing = TRUE)) [1:7]
  offline <- offline[offline$mac %in% subMacs,]
  #offline$mac <- factor(offline$mac)
  #locDF = with(offline, by(offline, list(posX, posY), function(x) x))
  #locDF = locDF[!sapply(locDF, is.null)]
  
  return(offline)
}

# this is the original reshape function
reshapeSS_00 = function(data, varSignal = "signal", 
                     keepVars = c("posXY", "posX","posY")) {
  byLocation =
    with(data, by(data, list(posXY), 
                  function(x) {
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol = length(avgSS),
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))
  
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}

# define critical prediction and CV functions for analysis purposes

# reshape signal strength, ensure we use if (sampleAngle) for CV purposes
reshapeSS_01 = function(data, varSignal = "signal", 
                          keepVars = c("posXY", "posX","posY"),
                          sampleAngle = FALSE, 
                          refs = seq(0, 315, by = 45)) {
  byLocation =
    with(data, by(data, list(posXY), 
                  function(x) {
                    if (sampleAngle) {
                      x = x[x$angle == sample(refs, size = 1), ]}
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    
                    y = matrix(avgSS, nrow = 1, ncol = length(avgSS),
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))
  
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}


surfaceSS = function(data, mac, angle = 45) {
  require(fields)
  oneAPAngle = data[ data$mac == mac & data$angle == angle, ]
  smoothSS = Tps(oneAPAngle[, c("posX","posY")], 
                 oneAPAngle$avgSignal)
  vizSmooth = predictSurface(smoothSS)
  plot.surface(vizSmooth, type = "C", 
               xlab = mac, ylab = "", xaxt = "n", yaxt = "n")
  points(oneAPAngle$posX, oneAPAngle$posY, pch=19, cex = 0.5) 
}

# define critical prediction and CV functions for analysis purposes

# reshape signal strength, ensure we use if (sampleAngle) for CV purposes
reshapeSS = function(data, varSignal = "signal", 
                     keepVars = c("posXY", "posX","posY"),
                     sampleAngle = FALSE, 
                     refs = seq(0, 315, by = 45)) {
  byLocation =
    with(data, by(data, list(posXY), 
                  function(x) {
                    if (sampleAngle) {
                      x = x[x$angle == sample(refs, size = 1), ]}
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol = 6,
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))
  
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}

# get training data for each new obs (including angle logic)

selectTrain = function(angleNewObs, signals = NULL, m = 1){
  refs = seq(0, by = 45, length  = 8)
  nearestAngle = roundOrientation(angleNewObs)
  
  if (m %% 2 == 1) 
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  else {
    m = m + 1
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    if (sign(angleNewObs - nearestAngle) > -1) 
      angles = angles[ -1 ]
    else 
      angles = angles[ -m ]
  }
  angles = angles + nearestAngle
  angles[angles < 0] = angles[ angles < 0 ] + 360
  angles[angles > 360] = angles[ angles > 360 ] - 360
  angles = sort(angles) 
  
  offlineSubset = signals[ signals$angle %in% angles, ]
  reshapeSS_01(offlineSubset, varSignal = "avgSignal")
}

# KNN: FIND NEIGHBORS - AVG BASED
findNN = function(newSignal, trainSubset) {
  diffs = apply(trainSubset[ , 4:9], 1, 
                function(x) x - newSignal) # this inverts, places x,y on cols when called as.numeric
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2)) ) # this gets our distance (euclidean)
  closest = order(dists) # orders our distances in ascending
  return(trainSubset[closest, 1:3 ]) # returns the subset from training with closest distances, gives xy ID, x, y
}

# PREDXY: takes an input of newSignals, their angles, and returns the estimated positions

predXY = function(newSignals, newAngles, trainData, 
                  numAngles = 1, k = 3){
  
  closeXY = list(length = nrow(newSignals))
  
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] = findNN(newSignal = as.numeric(newSignals[i, ]),
                          trainSS)
  }
  
  estXY = lapply(closeXY, function(x)
    sapply(x[ , 2:3], 
           function(x) mean(x[1:k])))
  estXY = do.call("rbind", estXY)
  return(estXY)
}


# ERROR CALCULATION

calcError = 
  function(estXY, actualXY) 
    sum( rowSums( (estXY - actualXY)^2) )

# WKNN: WEIGHTED K NEIGHBORS

findWtdNN = function(newSignal, trainSubset) {
  diffs = apply(trainSubset[ , 4:9], 1, 
                function(x) x - newSignal) 
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2)) ) 
  closest = order(dists) # orders distances ascending
  closeXY = trainSubset[closest, 1:3 ]
  weight = as.numeric(1/dists[closest]) 
  return(cbind(closeXY, weight)) 
}


# pull cross validation process into a repeatable function
cross_validate = function(train_full, train_summary, K=20, folds=11, 
                          keepVars = c("posXY", "posX","posY", "angle"),
                          wtd=FALSE) {
  set.seed(25)
  permuteLocs = sample(unique(train_summary$posXY))
  permuteLocs = matrix(permuteLocs, ncol = folds, 
                       nrow = floor(length(permuteLocs)/folds))
  
  onlineCVSummary = reshapeSS_01(train_full, keepVars = keepVars, sampleAngle = TRUE)
  
  err = rep(0, K)
  
  for (j in 1:folds) {
    onlineFold  = subset(onlineCVSummary,  posXY %in% permuteLocs[ , j])
    offlineFold = subset(train_summary, posXY %in% permuteLocs[ , -j])
    actualFold = onlineFold[ , c("posX", "posY")]
    
    for (k in 1:K) {
      if(wtd == TRUE){
        estFold = predXYwtd_total(newSignals = onlineFold[ , 5:10],
                                  newAngles = onlineFold[ , 4], 
                                  offlineFold, numAngles = 3, k = k)
        err[k] = err[k] + calcError(estFold, actualFold)
        
      } else {
        estFold = predXY(newSignals = onlineFold[ , 5:10],
                         newAngles = onlineFold[ , 4], 
                         offlineFold, numAngles = 3, k = k)
        err[k] = err[k] + calcError(estFold, actualFold)
      }
    }
  }
  
  rmseMin = min(err)
  kMin = which(err == rmseMin)[1]
  
  return(list(rmseMin, kMin, err))
}

# weighted prediction vector results, adjusted
predXYwtd_adj = function(newSignals, newAngles, trainData, 
                     numAngles = 1, k = 3){
  
  closeXY = list(length = nrow(newSignals))
  
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    base = findWtdNN(newSignal = as.numeric(newSignals[i, ]), trainSS) # get matrix of x,y, numerator for weights
    wts = append(base[1:k, 4]/sum(base[1:k, 4]), rep(0, nrow(base)-k))  # calculate weights based on K, append zero array for delta of len-k
  }
  return(cbind(base[,2:3], wts))
}

# weighted prediction function in total
predXYwtd_total = function(newSignals, newAngles, trainData, 
                     numAngles = 1, k = 3){
  
  closeXY = list(length = nrow(newSignals))
  
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    base = findWtdNN(newSignal = as.numeric(newSignals[i, ]), trainSS) # get matrix of x,y, numerator for weights
    wts = append(base[1:k, 4]/sum(base[1:k, 4]), rep(0, nrow(base)-k))  # calculate weights based on K, append zero array for delta of len-k
    base[, 2:3] = base[, 2:3]*wts # multiply weights array * matrix of x,y to get weighted vals
    closeXY[[i]] = base[,1:3] # append weighted xy, x, y values to list
  }
  estXY = lapply(closeXY, # loop over each xy position-based dataframe
                 function(x) sapply(x[ , 2:3], function(x) sum(x))) # sum all as neighbors > k == 0 now, and x,y is already weighted!
  estXY = do.call("rbind", estXY) # pull predictions together for each observation xy in test set
  return(estXY)
}

# show errors on the floor
floorErrorMap = function(estXY, actualXY, trainPoints = NULL, AP = NULL){
  
  plot(0, 0, xlim = c(0, 35), ylim = c(-3, 15), type = "n",
       xlab = "", ylab = "", axes = FALSE)
  box()
  if ( !is.null(AP) ) points(AP, pch = 15)
  if ( !is.null(trainPoints) )
    points(trainPoints, pch = 19, col="grey", cex = 0.6)
  
  points(x = actualXY[, 1], y = actualXY[, 2], 
         pch = 19, cex = 0.8 )
  points(x = estXY[, 1], y = estXY[, 2], 
         pch = 8, cex = 0.8 )
  segments(x0 = estXY[, 1], y0 = estXY[, 2],
           x1 = actualXY[, 1], y1 = actualXY[ , 2],
           lwd = 2, col = "red")
}

macs =    c("00:0f:a3:39:e1:c0", "00:0f:a3:39:dd:cd", "00:14:bf:b1:97:8a",
            "00:14:bf:3b:c7:c6", "00:14:bf:b1:97:90", "00:14:bf:b1:97:8d",
            "00:14:bf:b1:97:81")

online = readData("data/online.final.trace.txt")
online$posXY = paste(online$posX, online$posY, sep = "-")

keepVars = c("posXY", "posX", "posY", "orientation", "angle")
byLoc = with(online, 
             by(online, list(posXY), 
                function(x) {
                  ans = x[1, keepVars]
                  avgSS = tapply(x$signal, x$mac, mean)
                  y = matrix(avgSS, nrow = 1, ncol = 7,
                             dimnames = list(ans$posXY, names(avgSS)))
                  cbind(ans, y)
                }))

onlineSummary = do.call("rbind", byLoc)