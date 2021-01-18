library(dplyr)
processLine = function(x)
{
  tokens = strsplit(x, "[;=,]")[[1]]
  if (length(tokens) == 10)
    return(NULL)
  tmp = matrix(tokens[- (1:10)], ncol = 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6, byrow = TRUE), tmp)
}

txt <- readLines("data/offline.final.trace.txt")
lines = txt[substr(txt, 1, 1) != "#"]
options(error = recover, warn = 1)
tmp = lapply(lines, processLine)
offline = as.data.frame(do.call("rbind", tmp),stringsAsFactors = FALSE)

t_of <- offline %>% count(mac)

t_of[order(t_of$n),]

summary(offline)

names(offline) = c("time", "scanMac", "posX", "posY", "posZ",
                   "orientation", "mac", "signal",
                   "channel", "type")

numVars = c("time", "posX", "posY", "posZ",
            "orientation", "signal")

offline[numVars] = lapply(offline[numVars], as.numeric)

str(offline)

offline = offline[offline$type == "3",]
offline = offline[, "type" != names(offline)]
dim(offline)
str(offline)
offline$rawTime = offline$time
offline$time = offline$time/1000
class(offline$time) = c("POSIXt", "POSIXct")
unlist(lapply(offline, class))
summary(offline[, numVars])
summary(sapply(offline[, c("mac", "channel", "scanMac")],as.factor))
offline = offline[, !(names(offline) %in% c("scanMac", "posZ"))]
unique(offline$mac)
