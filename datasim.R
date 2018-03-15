
par(mar=c(8,4.1,1,1))
v1 <- c(runif(50,2,10))
v2 <- c(runif(50,5,10))
v3 <- c(runif(50,5,15))

df <- data.frame(v1,v2,v3)
colnames(df) <-c("zufällig/\nzufällig ","meiste Nachbarn/\nmeiste Nachbarn ","Heuristik/\nHeuristik ")

boxplot(df, las= 2, notch = T, whisklty = 0, staplelty = 0, col="lightblue")

#reset plot margins
par(mar=c(5.1, 4.1, 4.1, 2.1))
