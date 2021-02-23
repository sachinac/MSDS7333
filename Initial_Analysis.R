
# Date : 01/11/2021
# Author : Sandesh Ojha



# Variable Units
# t         timestamp in milliseconds since midnight, January 1, 1970 UTC
# id        MAC address of the scanning device
# pos       the physical coordinate of the scanning device
# degree    orientation of the user carrying the scanning device in degrees
# MAC       MAC address of a responding peer (e.g., an access point or a device
#          in adhoc mode) with the corresponding values for signal strength
#          in dBm (Decibel-milliwatts), the channel frequency and its mode
#          (access point = 3, device in adhoc mode = 1)


# reading the offline data
data <- readLines("C:\\Users\\Sandesh\\Documents\\SMU\\Spring 2021\\DS7333 Quantifying the World\\DS7333_SO_CaseStudies\\offline.final.trace.txt")

# Identify comments starting with a # sign - 5,312
sum(substr(data, 1, 1) == "#")

# lines in the file - 151,392
length(data)

# check the semicolon split the fourth line
strsplit(data[4], ";")[[1]]


# further split each element at the '=' characters.
unlist(lapply(strsplit(data[4], ";")[[1]],
              function(x)
                sapply(strsplit(x, "=")[[1]], strsplit, ",")))

# Remove comments and use ;= as the delimiter
tokens = strsplit(data[4], "[;=,]")[[1]]

# first 10 elements of tokens give the information about the hand-held device:
tokens[1:10]

# n extract the values of these variables 
tokens[c(2, 4, 6:8, 10)]

# remaining values in the split vector
tokens[ - ( 1:10 ) ]

# Include one location per line
tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
mat = cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow = nrow(tmp), ncol = 6, byrow = TRUE), tmp)

#confirm that we have 11 rows and 10 columns in the matrix
dim(mat)

# Build the above into a single function
processLine = function(x)
{
  tokens = strsplit(x, "[;=,]")[[1]]
  if (length(tokens) == 10)
    return(NULL)
  
  tmp = matrix(tokens[ - (1:10)], ncol = 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow = nrow(tmp),ncol = 6, byrow = TRUE), tmp)
}

# Apply our function to several lines of the input:
tmp = lapply(data[4:20], processLine)


# Validate a list of 17 matrices
sapply(tmp, nrow)


# Running the processline function across the entire data set - 10 columns and 1,181,628 rows
lines = data[ substr(data, 1, 1) != "#" ]
tmp = lapply(lines, processLine)
offline = as.data.frame(do.call("rbind", tmp),
                        stringsAsFactors = FALSE)
dim(offline)




# Column names assigned
names(offline) = c("time", "scanMac", "posX", "posY", "posZ", "orientation", "mac", "signal", "channel", "type")

# Convert the  values to numeric
numVars = c("time", "posX", "posY", "posZ",
            "orientation", "signal")
offline[ numVars ] = lapply(offline[ numVars ], as.numeric)

# Remove non-access point devices (since 1= adhoc and 3 = access point) - 978,443 rows and 9 columns
offline = offline[ offline$type == "3", ]
offline = offline[ , "type" != names(offline) ]
dim(offline)

# Convert time format
offline$rawTime = offline$time
offline$time = offline$time/1000
class(offline$time) = c("POSIXt", "POSIXct")

# Review data summary
summary(offline[, numVars])

# Further cleaning the data, posZ and ScanMac are all 0 values for all rows - remove
offline = offline[ , !(names(offline) %in% c("scanMac", "posZ"))]

# Exploring Orientation - 1 x 203
length(unique(offline$orientation))

# Plot the values from the orientation field
plot(ecdf(offline$orientation))

# Round the orientation so they are more inline with the intended analysis
roundOrientation = function(angles) {
  refs = seq(0, by = 45, length = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}
offline$angle = roundOrientation(offline$orientation)

# Plot angles
with(offline, boxplot(orientation ~ angle,
                      xlab = "nearest 45 degree angle",
                      ylab = "orientation"))



###Code is good all the upto here, need to work on the below.

# Count number of unique MAC and channels - row counts
c(length(unique(offline$mac)), length(unique(offline$channel)))
table(offline$mac)



offline = offline[ , "channel" != names(offline)]


locDF = with(offline,
             by(offline, list(posX, posY), function(x) x))
length(locDF)

