#boxplots

par(mar=c(10,4.1,1,1))
v1 <- c(runif(50000,2,10))
v2 <- c(runif(50000,5,10))
v3 <- c(runif(50000,5,15))

df <- data.frame(v1,v2,v3)
colnames(df) <-c("zufällig/\nzufällig ","meiste Nachbarn/\nmeiste Nachbarn ","Heuristik/\nHeuristik ")

boxplot(df, las= 2, notch = T, whisklty = 0, staplelty = 0, col="lightblue",cex.axis=1.3)
means <- colMeans(df)
points(means, pch=18, cex=2.5)

#reset plot margins
par(mar=c(5.1, 4.1, 4.1, 2.1))


#barplots
par(mar=c(10, 4.1, 4.1, 2.1)) #change accordingly
lvec <- c(3,4,5,3,4,5,3,4,5)
df2 <-data.frame()
mp<-barplot(lvec,col="lightblue",cex.axis=1.3)
axis(1,c(seq(0.7,10.3,by=1.2)),c("zufällig/\nzufällig ","meiste Nachbarn/\nmeiste Nachbarn ",
                "Heuristik/\nHeuristik ","zufällig/\nzufällig ",
                "meiste Nachbarn/\nmeiste Nachbarn ","Heuristik/\nHeuristik ",
                "zufällig/\nzufällig ","meiste Nachbarn/\nmeiste Nachbarn ",
                "Heuristik/\nHeuristik "),las=2,cex.axis=1.3)
#text(mp,lvec,labels=lvec,pos=3) #if used, set ylim=c(0,max+1) in mp

#reset plot margins
par(mar=c(5.1, 4.1, 4.1, 2.1))

#histograms
par(mfrow=c(1,3))
par(mar=c(9.1, 5.5, 4.1, 2.1))
hist(df[,1], col="lightblue",main="",xlab = "",
     ylab= "", cex.axis=1.5,ylim=c(0,40000))
title(xlab="zufällig/\nHeuristik", cex.lab=2,line=6)
title(ylab="Häufigkeit", cex.lab=2,line=3.5)
hist(df[,2], col="lightblue",main="",xlab = "",
     ylab= "", cex.axis=1.5,ylim=c(0,40000),yaxt="n")
title(xlab="zufällig/\nHeuristik", cex.lab=2,line=6)

hist(df[,3], col="lightblue",main="",xlab = "",
     ylab= "", cex.axis=1.5,ylim=c(0,40000),yaxt="n")
title(xlab="zufällig/\nHeuristik", cex.lab=2,line=6)

#reset plot margins
par(mar=c(5.1, 4.1, 4.1, 2.1))

#histograms (i=100 to 10k)
par(mfrow=c(1,3))
par(mar=c(5.1, 3.1, 4.1, 2.1))
hist(df[,1], col="lightblue",main="",xlab = "",
     ylab= "", cex.axis=1.5,yaxt="n")
title(main="100 Wiederholungen", cex.main=2,line=1)
hist(df[,2], col="lightblue",main="",xlab = "",
     ylab= "", cex.axis=1.5,yaxt="n")
title(main="1.000 Wiederholungen", cex.main=2,line=1)

hist(df[,3], col="lightblue",main="",xlab = "",
     ylab= "", cex.axis=1.5,yaxt="n")
title(main="10.000 Wiederholungen", cex.main=2,line=1)

#reset plot pars
par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))
