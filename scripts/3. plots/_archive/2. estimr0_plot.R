# Clear matrix #
#--------------#
rm(list = ls())
# Set working-directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Plot the data #
#################
# The plot is developed for Germmany and the state of Brandenburg
#----------------------------------------------------------------
ger <- read.csv2("../../data/rnull/federal_r.csv")
state <- read.csv2("../../data/rnull/Brandenburg_r.csv")

# We seek to plot the mean and the 95% comfidence intervall
# of R0 for each week (focusing on the day a mean was estimated)
pdf("../../outcome/OutcomeRNull.pdf", width=10, height=5)
par(mar=c(5.5, 5, 1, 0)) 
plot(seq(1, nrow(ger), 1)
     , las = 1
     , type="n"
     , ylim=c(.5,3.5)
     , xlim=c(1,nrow(ger))
     , xlab=""
     , xaxt="n" 
     , ylab="Durschnittliche Reproduktionszahl [R0]"
     , main= "Durchschnittliche Reproduktionszahl von COVID-19 pro Woche"
     , cex.lab = .9
     , cex.main = .9
     , frame.plot=F)

polygon(c(((nrow(ger) - nrow(state)) + seq(1, nrow(state), 1))
          ,rev(((nrow(ger) - nrow(state)) + seq(1, nrow(state), 1))))
        ,c(state$Quantile.0.975.R.,rev(state$Quantile.0.025.R.))
        ,col= adjustcolor( "darkgray", alpha.f = .2), border = adjustcolor( "darkgray", alpha.f = .2), cex = 1.75)  
points((state$t_start-1 + nrow(ger) - nrow(state)), state$Mean.R., pch=20, cex=1, col="black", type = "b")   

polygon(c(seq(1, nrow(ger), 1),rev(seq(1, nrow(ger), 1)))
        ,c(ger$Quantile.0.975.R.,rev(ger$Quantile.0.025.R.))
        ,col=adjustcolor( "darkgray", alpha.f = .4), border = adjustcolor( "darkgray", alpha.f = .4), cex = 1.75)  
points(ger$t_start-1, ger$Mean.R., pch=18, cex=1, col="black", type = "b")   

# For the x-Axis
upperdate <- as.Date(ger$date, "%Y-%m-%d") + 7
upperdate <- gsub("2020-", "", upperdate)
upperdate <- ifelse(substr(upperdate, 1,2) == "03"
               , gsub("03-", "03/", upperdate)
               , upperdate)
upperdate <- ifelse(substr(upperdate, 1,2) == "04"
               , gsub("04-", "04/", upperdate)
               , upperdate)
upperdate <- ifelse(substr(upperdate, 1,2) == "05"
               , gsub("05-", "05/", upperdate)
               , upperdate)

date <- gsub("2020-", "", ger$date)
date <- ifelse(substr(date, 1,2) == "03"
            , gsub("03-", "03/", date)
            , date)
date <- ifelse(substr(date, 1,2) == "04"
               , gsub("04-", "04/", date)
               , date)
date <- ifelse(substr(date, 1,2) == "05"
               , gsub("05-", "05/", date)
               , date)

weeks <- paste(date,"-",upperdate)  

axis(1
     , at = seq(1, nrow(ger), 1)
     , labels = weeks
     , las=2, tck=-.02
     , cex.axis = .8) # D

text(median(seq(1, nrow(ger), 1)), 3.5, "Unter der Annahme eines durchschnittlichen seriellen Intervalls von 4 Tagen \n und einer Standardabweichung von 4.75 Tagen \n [Quelle: Du et al. (2020) DOI: 2020.02.19.20025452]", pos = 1,  srt=0, xpd = TRUE, cex = .8,  col = "black")   
text(max(seq(1, nrow(ger), 1))-4, .6, "by Benjamin G. Engst & David M. Grundmanns", pos = 1,  srt=0, xpd = TRUE, cex = .7,  col = "black")   

text(11, .575, labels = "Zeitraum der Schulschließungen", xpd = TRUE, cex=0.75, pos = 3)
segments(7, .6, 15, .6, lty = 1, lwd = 5, col = "black")

legend("right"
       , legend = c("Deutschlandweit", "Bundesland Brandenburg", "95% Konfidenzintervall")
       , pch = c(18, 20, 15) # Kästchen mit den vergebenen Farben
       , col = c("black","black","gray80" )
       , pt.cex = c(1,1,1.5)
       , text.col = c("black")
       , cex = .8  # Schriftgröße
       , bg = NULL # Keine Hintergrungfarbe
       , bty = "n"
       , horiz = FALSE)
dev.off()
# All code for this project developed by: 
# (c) Benjamin G. Engst & David M. Grundmanns 