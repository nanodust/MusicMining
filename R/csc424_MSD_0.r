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
