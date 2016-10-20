require("pairsD3")
require("psych")
require('ggplot2')
library(corrgram)
music <- read.csv("db_10k.csv",header=TRUE)
describe(music)
head(music)
dim(music)
library(rattle)

ggplot(music)

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



# Build the training/validate/test datasets.

crs <- new.env(parent = baseenv())

crs$dataset <- read.csv("db_modified.csv", na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

set.seed(993714) 
crs$nobs <- nrow(crs$dataset) # 10000 observations 
crs$sample <- crs$train <- sample(nrow(crs$dataset), 0.7*crs$nobs) # 7000 observations
crs$validate <- sample(setdiff(seq_len(nrow(crs$dataset)), crs$train), 0.15*crs$nobs) # 1500 observations
crs$test <- setdiff(setdiff(seq_len(nrow(crs$dataset)), crs$train), crs$validate) # 1500 observations

# The following variable selections have been noted.

crs$input <- c("artist_familiarity", "artist_hotttnesss", "artist_id", "artist_mbid",
               "artist_playmeid", "artist_7digitalid", "artist_latitude", "artist_longitude",
               "artist_location", "artist_name", "release", "release_7digitalid",
               "song_hotttnesss", "title", "track_7digitalid", "similar_artists",
               "artist_terms", "artist_terms_freq", "analysis_sample_rate", "audio_md5",
               "danceability", "duration", "end_of_fade_in", "key",
               "key_confidence", "loudness", "mode", "mode_confidence",
               "start_of_fade_out", "tempo", "time_signature", "time_signature_confidence",
               "segments_start", "segments_confidence", "segments_loudness_max", "segments_loudness_max_time",
               "segments_loudness_start", "sections_start", "sections_confidence", "beats_start",
               "beats_confidence", "bars_start", "bars_confidence", "tatums_start",
               "tatums_confidence", "artist_mbtags", "artist_mbtags_count", "year")

crs$numeric <- c("artist_familiarity", "artist_hotttnesss", "artist_playmeid", "artist_7digitalid",
                 "artist_latitude", "artist_longitude", "artist_terms_freq", "danceability",
                 "duration", "key", "key_confidence", "loudness",
                 "mode", "mode_confidence", "start_of_fade_out", "tempo",
                 "time_signature", "time_signature_confidence", "segments_start", "segments_confidence",
                 "segments_loudness_max_time", "segments_loudness_start", "sections_start", "sections_confidence",
                 "beats_start", "beats_confidence", "bars_start", "bars_confidence",
                 "tatums_start", "tatums_confidence", "artist_mbtags_count", "year")

crs$categoric <- c("artist_id", "artist_mbid", "artist_location", "artist_name",
                   "release", "release_7digitalid", "song_hotttnesss", "title",
                   "track_7digitalid", "similar_artists", "artist_terms", "analysis_sample_rate",
                   "audio_md5", "end_of_fade_in", "segments_loudness_max", "artist_mbtags")

crs$target  <- "artist_terms_weight"
crs$risk    <- NULL
crs$ident   <- c("song_id", "track_id")
crs$ignore  <- c("energy", "X", "X.1", "X.2")
crs$weights <- NULL

library(Hmisc, quietly=TRUE)
library(corrplot, quietly=TRUE)

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation db_modified.csv using Pearson")

# Principal Components Analysis (on numerics only).

pc <- princomp(na.omit(crs$dataset[crs$sample, crs$numeric]), scale=TRUE, center=TRUE, tol=0)

# Show the output of the analysis.

pc

# Summarise the importance of the components found.

summary(pc)

# Display a plot showing the relative importance of the components.

plot(pc, main="")
title(main="Principal Components Importance db_modified.csv")


# Display a plot showing the two most principal components.

biplot(pc, main="")
title(main="Principal Components db_modified.csv")
