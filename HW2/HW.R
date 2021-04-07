# library(fBasics)
# library(xts)
# library(highfrequency)
# library(dplyr)
# library(lubridate)
# library(ggplot2)
# library(stargazer)
# library(psych)


# In this chapter we are using the Quality of life and chronic disease dataset, Case06_QoL_Symptom_ChronicIllness.csv. 
# This dataset has 41 variables. Detailed description for each variable is provided here.

# Important variables:
# Charlson Comorbidity Index: ranging from 0-10. A score of 0 indicates no comorbid conditions. 
#                             Higher scores indicate a greater level of comorbidity.
# Chronic Disease Score: A summary score based on the presence and complexity 
#                         of prescription medications for select chronic conditions. 
#                       A high score in decades the patient has severe chronic diseases. 
#                       -9 indicates a missing value.

#loading the data
if (file.exists("data.csv")){
  qol<-read.csv("data.csv")
  
}else{
  qol<-read.csv("https://umich.instructure.com/files/481332/download?download_frd=1")
  write.csv(qol,"data.csv", row.names = FALSE)
  }



#1 Summarize and visualize the data using summary, str, pairs.panels, ggplot.
summary(qol)
str(qol)

# install.packages("psych")
library(psych)
cols=c("INTERVIEWDATE","AGE","RACE_ETHNICITY","SEX")
pairs.panels(qol[,cols])

# data which is in -9 are actually missing data. 
library(ggplot2)
ggplot() +
  geom_point(aes(x = qol$ID, y = qol$CHRONICDISEASESCORE))

# remove missing values
qol<-qol[!qol$CHRONICDISEASESCORE==-9, ]
summary(qol$CHRONICDISEASESCORE)

# After removing missing data, we have data ranges from 0 to 5
ggplot(qol,aes(x = ID, y = CHRONICDISEASESCORE)) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(qol,aes(x = AGE, y = CHRONICDISEASESCORE)) +
  geom_point()+
  geom_smooth(method='lm')

ggplot(qol,aes(x = QOL_Q_01, y = CHRONICDISEASESCORE)) +
  geom_point()+
  geom_smooth(method='lm')


hist(qol$CHRONICDISEASESCORE, main = "Histogram for CHRONICDISEASESCORE" ,breaks = 20)

# ===========================correlation plot=======================================
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
feat_names=c(colnames(qol)[29:40],"CHRONICDISEASESCORE") #full features 7:41
cormat <- round(cor(qol[feat_names]),2)
library(reshape2)

# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()


ggheatmap <-ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# Print the heatmap
print(ggheatmap)
# ========================================================================================
# independent features doesn't have a strong correlation.
# which means we won't have multicolinearity problem


# creating random training and test datasets
qol<-qol[order(qol$ID), ]
# Remove ID (col=1) # the clinical Diagnosis (col=41) will be handled later
qol <- qol[ , -1]

# qol_train<-qol[1:2114, ]
# qol_test<-qol[2115:2214, ]

# random split into training and testing data
set.seed(1234)
train_index <- sample(seq_len(nrow(qol)), size = 0.8*nrow(qol))
qol_train<-qol[train_index, ]
qol_test<-qol[-train_index, ]

# check features
summary(qol_train)
colnames(qol_train)



fit1<-lm(CHRONICDISEASESCORE~., data=qol_train)
summary(fit1)


# plot QQ and residual plot
plot(fit1, which = 1:4)

library(fBasics)
normalTest(fit1$residuals)
# according to the residual error plot, there is no hetero.. problem
# shapiro thers W is close to 1 which means the data is close to normal distribution


# Half-normal plot for leverages
# install.packages("faraway")
library(faraway)
halfnorm(lm.influence(fit1)$hat, nlab = 2, ylab="Leverages")

qol_train[c(57,1151),]
summary(qol_train)


# predict outcome of the new data and measure the R2
qol.p<-predict(fit1, qol_test)
plot(qol.p)
plot(qol_test$CHRONICDISEASESCORE)

# evaluation
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}
RSQUARE(qol_test$CHRONICDISEASESCORE,qol.p)


# ======================using step to improve the performance============================
step(fit1,direction = "backward")

fit2 = step(fit1,k=2,direction = "backward",deta) 
# the multiple of the number of degrees of freedom used for the penalty. 
# Only k = 2 gives the genuine AIC: k = log(n) is sometimes referred to as BIC or SBC.
summary(fit2)

# ==================using regression tree model to improve the performances=================

#install.packages("rpart")
library(rpart)
qol.rpart<-rpart(CHRONICDISEASESCORE~., data=qol_train)
qol.rpart


## install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(qol.rpart, digits=3)

rpart.plot(qol.rpart, digits = 4, fallen.leaves = T, type=3, extra=101)

library(rattle)
fancyRpartPlot(qol.rpart, cex = 0.8)
qol.p<-predict(qol.rpart, qol_test)


# evaluation
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}
RSQUARE(qol_test$CHRONICDISEASESCORE,qol.p)

summary(qol.p)
summary(qol_test$CHRONICDISEASESCORE)
cor(qol.p, qol_test$CHRONICDISEASESCORE)


MAE<-function(obs, pred){
  mean(abs(obs-pred))
}
MAE(qol_test$CHRONICDISEASESCORE, qol.p)

mean_estimator=mean(qol_test$CHRONICDISEASESCORE)
mean_estimator
MAE(qol_test$CHRONICDISEASESCORE, mean_estimator)
# ==================using M5P to imporve model=================


#install.packages("RWeka")
# Sometimes RWeka installations may be off a bit, see:
# http://stackoverflow.com/questions/41878226/using-rweka-m5p-in-rstudio-yields-java-lang-noclassdeffounderror-no-uib-cipr-ma
path_dir=Sys.getenv("WEKA_HOME") # where does it point to? Maybe some obscure path?
## [1] ""
# if yes, correct the variable:
Sys.setenv(WEKA_HOME=path_dir)
library(RWeka)
# WPM("list-packages", "installed")
qol.m5<-M5P(CHRONICDISEASESCORE~., data=qol_train)
qol.m5

#summary of the model
summary(qol.m5)
# get prediction
qol.p.m5<-predict(qol.m5, qol_test)
# summary of the predictionm
summary(qol.p.m5)
# get correlation between estimation and truth
cor(qol.p.m5, qol_test$CHRONICDISEASESCORE)
# get MAE
MAE(qol_test$CHRONICDISEASESCORE, qol.p.m5)


RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}
RSQUARE(qol_test$CHRONICDISEASESCORE,qol.p.m5)

