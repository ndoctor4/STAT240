## Lab o4
## February 08-2020

## Problem 1
counts <- function(x, n = 1) {
  range_x <- (max(x)-min(x))/n
  bin <- c(min(x),min(x)+range_x)
  dat <- vector(mode="numeric", length=n)
  i <- 1
  
  while (bin[2]<=max(x)) {
    freq <- 0
    for (j in 1:length(x)) {
      if (x[j]>=bin[1] && x[j]<bin[2]) {
        freq <- freq + 1
      }
    }
    
    dat[i] <- freq
    i <- i + 1
    bin <- c(bin[2],bin[2]+range_x)
  }
  
  return(dat)
}

histo <- function(x, n = 1) {
  range_x <- (max(x)-min(x))/n
  bin <- c(min(x),min(x)+range_x)
  height <- counts(x,n)
  
  plot(1, type="n", main="Plot of Frequency", 
       xlab="x", ylab="Counts",xlim=c(min(x),max(x)), 
       ylim=c(0,max(height)+1))
  
  for (i in 1:n) {
    # left vertical line
    lines(x=c(bin[1],bin[1]), y=c(0,height[i]))
    # top horizontal line
    lines(x=bin,y=c(height[i],height[i]))
    # right vertical line
    lines(x=c(bin[2],bin[2]), y=c(0,height[i]))
    
    bin <- c(bin[2],bin[2]+range_x)
  }
}

# Testing
# counts(c(1,1,1,2,3,3,4,5,5,5,5,6,7,8), 3)
# histo(c(1,1,1,2,3,3,4,5,5,5,5,6,7,8), 3)

# 1c - Testing
Test_c <- rnorm(100, mean=-1, sd=1) + rnorm(100, mean=1, sd=1)
counts(Test_c, 10)
histo(Test_c, 10)
# barplot(height = c(counts(Test_c,10)), width = c(rep(0.6317956,10)), ylim = c(0,20))
  # Check using barplot, if it's the same

# 1d - Testing
histo(c(0,0,0,1,1,2), 3)


## Next Unit
library(DBI)
library(RSQLite)

my_db = dbConnect(SQLite(), dbname = "lab04.sqlite")
dbListTables(my_db)

mov_avg1 = "SELECT Edition, Count(Edition) AS TotalNumber 
            FROM Olymp_meds GROUP BY Edition" 
out = dbGetQuery(my_db, mov_avg1)

plot(out$Edition, out$TotalNumber, 
     main="Total number of athletes who obtained Olympic medals", 
     xlab="year",ylab="Count",type="p", 
     col="red",lwd=2,las=2)

## Virtual Table
mov_avg2 = "CREATE VIEW tot_meds AS SELECT Edition, 
            Count(Edition) AS TotalNumber 
            FROM Olymp_meds GROUP BY Edition" 
dbSendQuery(my_db, mov_avg2)

dbListTables(my_db) # "tot_meds" is added to our databases

mov_avg3 = "SELECT Edition, TotalNumber FROM tot_meds" 
out = dbGetQuery(my_db, mov_avg3)

#remaking the plot from 1a (not necessary for students): 
plot(out$Edition, out$TotalNumber, 
     main="Total number of athletes who obtained Olympic medals", 
     xlab="year",ylab="Count",type="p", col="red",lwd=2,las=2)

lines(out$Edition, out$TotalNumber,col=1,lwd=2) 
# Note that the line must go through the points 
# to show that it is from "lines" instead of type="b".

check = "SELECT * FROM tot_meds AS t, 
        (SELECT t1.Edition, AVG(t2.TotalNumber) AS mavg 
        FROM tot_meds AS t1, tot_meds AS t2 
        WHERE t2.Edition BETWEEN (t1.Edition-4) AND (t1.Edition+4) 
        GROUP BY t1.Edition) sq WHERE (t.Edition = sq.Edition)" 
movingAvg = dbGetQuery(my_db, check)

# new part to above plot
lines(movingAvg$Edition, movingAvg$mavg,type="l",col=3,lwd=2) 
legend("topleft",lwd=2,lty=c(NA,1,1),pch=c(1,NA,NA), 
       col=c(2,1,3), c("medals per year points", "medals per year line", "moving average"))

dbSendQuery(my_db, "drop view tot_meds")

## Problem 2
library(MASS)
library(sp)
library(rworldmap)
library(rworldxtra)

Van_Poke <- dbReadTable(my_db, "Vanpoke")

worldmap = getMap(resolution = "high") 
NrthAm = worldmap[which(worldmap$REGION =="North America"),] 

plot(NrthAm, xlim=c(-123.35,-122.65), ylim=c(49,49.35), main = "Pokemon in Vancouver")
points(Van_Poke$longitude,Van_Poke$latitude,pch='.') 

plot(NrthAm, xlim=c(-123.35,-122.65), ylim=c(49,49.35), main = "Pokemon in Vancouver")
est2 = kde2d(Van_Poke$longitude,Van_Poke$latitude,n = c(121,150)) 
contour(est2, add=TRUE,col=2,lwd=3)

## 2d
plot(NrthAm, xlim=c(-123.35,-122.65), ylim=c(49,49.35), main = "Pokemon in Vancouver")
est2 = kde2d(Van_Poke$longitude,Van_Poke$latitude,n = c(121,150)) 
contour(est2, add=TRUE,col=2,lwd=3)
text(c(-123.1207,-122.9805,-122.7932), 
     c(49.2827,49.2488,49.2838), 
     c("Vancouver","Burnaby","Coquitlam"))


dbDisconnect(my_db)
