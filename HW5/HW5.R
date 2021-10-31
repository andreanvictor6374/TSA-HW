
hand_letters<-read.csv("16_HandwrittenEnglish_Letters.csv",header = TRUE)

str(hand_letters)
# install.packages("sjPlot")
library(sjPlot)

tab_corr(hand_letters[,-1])

# page 644
# install.packages("lavaan")
library(lavaan)
fit<-cfa(model1, data=mydata, missing = 'FIML')

scaled_hand_letters<-scale(hand_letters[,-1])


hand_letters_train <- hand_letters[1:15000, ]
hand_letters_test <- hand_letters[15001:20000, ]

