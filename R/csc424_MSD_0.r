install.packages("pairsD3")
require("pairsD3")

music <- read.csv("db_10k.csv",header=TRUE)

#ridding of NA values
music$artist_terms_freq[is.na(music$artist_terms_freq)] <- 0
music$artist_familiarity[is.na(music$artist_familiarity)] <- 0
#convert wrongly assigned types
music$artist_terms_weight <- as.numeric(music$artist_terms_weight)
music$analysis_sample_rate <- as.numeric(music$analysis_sample_rate)
music$end_of_fade_in <- as.numeric(music$end_of_fade_in)
music$artist_hotttnesss<- as.numeric(music$artist_hotttnesss)
music$segments_loudness_max<- as.numeric(music$segments_loudness_max)
music$song_hotttnesss <- as.numeric(music$song_hotttnesss)

vars <- colnames(music)

head(music)

dim(music)

library(corrgram)
corrgram(music, order=NULL, lower.panel=panel.ellipse, upper.panel=panel.pts,main="Corregram Matrix",pch=20) 

# make correlation matrix
music_cor = cor(music)

# visualize it
corrplot(music_cor, method="ellipse",order="AOE")

# those two things break right now, some data issue 

# try shiny

shinypairs(music)


# Zack's scatterplot matrix based on highly correlated variables 

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}


panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                      cex = 1, col.smooth = "black", ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  abline(stats::lm(y ~ x), col = col.smooth, ...)
}


panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}





pairs(music[,c(17,19,21,22,24)], lower.panel=panel.smooth,upper.panel=panel.cor,
      main="Million Song Database Scatterplot Matrix")
