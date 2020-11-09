df = read.csv("/Users/jordiwalder/Desktop/R Programmierung/R Data Science/9-Machine Learning mit R/student-mat.csv", sep = ";")

any( is.na(df)) # --> FALSE, Daten sauber

# Daten anschauen mit str, summary, usw.

library(ggplot2)
library(ggthemes)
library(dplyr)

num.cols = sapply(df, is.numeric) # nur numerische Werte

cor.data = cor( df[,num.cols]) # Korrelation der numerischen Werte

install.packages( 'corrgram', repos = 'http://cran.us.r-project.org')
install.packages( 'corrplot', repos = 'http://cran.us.r-project.org')
install.packages( 'viridisLite', repos = 'http://cran.us.r-project.org')

library( corrplot )
library( corrgram )
library( viridisLite )
help( corrplot)
corrplot( cor.data, method = 'color')
corrgram(df, order = TRUE, panel = panel.shade, lower.panel = panel.shade, upper.panel = panel.pie,
         text.panel = panel.txt)
ggplot( df, aes( x = G3)) + geom_histogram( bins = 20, alpha = 0.5, fill = 'blue') + theme_minimal()

#install.packages( 'caTools') 
library( caTools)

# Aufteilen in Trainings und Test Daten
set.seed( 101 )
sample = sample.split( df$G3, SplitRatio = 0.70 )
train = subset( df, sample = TRUE)
test = subset( df, sample = FALSE )

# Ausführen des Modells

model = lm( G3 ~ ., train )

summary( model )

res = residuals( model )
res = as.data.frame( res )

ggplot( res, aes(res)) + geom_histogram( fill = 'blue', alpha = 0.5 )

plot( model )

# Vorhersagen

G3.predictions = predict(model, test)
results = cbind( G3.predictions, test$G3)
colnames( results) = c('pred', 'real')
results = as.data.frame( results )

to_zero = function(x){
  if( x < 0){ return(0) }
  else{ return(x) }
}

results$pred = sapply( results$pred, to_zero)

mse = mean(( results$real - results$pred)^2)
print( mse )

SSE = sum(( results$pred - results$real)^2)
SST = sum(( mean(df$G3) - results$real)^2)

R2 = 1 - SSE/SST
R2 #0.84, 80 Prozent Ergebnisse in Varianz
