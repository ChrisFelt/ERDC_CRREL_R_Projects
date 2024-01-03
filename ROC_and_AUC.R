#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# FILE: ROC_and_AUC.R
# AUTHORS: Christopher Felt ERDC-CRREL-NH
# DATE CREATED: 05 August 2020
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
#
# PURPOSE
# =======
# Generate logistic regression plots and ROC curves for VWC and Object_Visibility 
# 
# SOURCE: https://github.com/StatQuest/roc_and_auc_demo/blob/master/roc_and_auc_demo.R 
#
#

#----------------------------------------------------------------------
# Load required libraries.
#----------------------------------------------------------------------
library(pROC) # install with install.packages("pROC")
library(randomForest) # install with install.packages("randomForest")


#----------------------------------
# sunRiseTimes$meanOfMeans
#----------------------------------

# Set plot filenames
plot_names <- c("sunRiseTimes$plastic_deep", "sunRiseTimes$metal_deep", "sunRiseTimes$plastic_shallow", "sunRiseTimes$metal_shallow")

# generate and print plots for each object
for(i in 1:length(plot_names)){
  
  ## First, generate logistic regression plot
  # define continuous independent variable (in this case, mean VWC in the morning ~3 hours after sunrise)
  VWC <- sunRiseTimes$meanOfMeans
  
  # define dichotomous dependent variable (in this case, I check if the object is distinguishable from the background)
  Object_Visibility <- ifelse(sunRiseTimes[,(46 + i)] >= 0.25, 
                              yes=1, no=0)
  
  # open graphics device and set filename
  png((file = paste0(oDir, "/glm_", plot_names[i], ".png")))
  
  # plot the data
  plot(x=VWC, y=Object_Visibility, ylab="Object Visibility", xlab="Soil Moisture (VWC)")
  
  
  # fit a logistic regression to the data...
  glm.fit=glm(Object_Visibility ~ VWC, family=binomial)
  
  # customize the plot
  # Graphing inspiration SOURCE: https://sites.google.com/site/daishizuka/toolkits/plotting-logistic-regression-in-r
  curve(predict(glm.fit,data.frame(VWC=x),type="resp"), col = "green", lwd = 2, add=TRUE, 
        ylab="Object Visibility", xlab="Soil Moisture (VWC)", xlim=c(0.18, 0.30), ylim=c(0.0, 1.0))
  
  # turn off graphics device
  dev.off()
  
  
  ## draw ROC and AUC using pROC
  # open graphics device and define ROC curve filename
  png((file = paste0(oDir, "/", plot_names[i], ".png")))
  
  # initial plot
  roc(Object_Visibility, glm.fit$fitted.values, plot=TRUE)
  
  # set plot aspect ratio to square
  par(pty = "s") 
  roc(Object_Visibility, glm.fit$fitted.values, plot=TRUE)
  
  # NOTE: By default, roc() uses specificity on the x-axis and the values range
  # from 1 to 0. This makes the graph look like what we would expect, but the
  # x-axis itself might induce a headache. To use 1-specificity (i.e. the 
  # False Alarm Rate) on the x-axis, set "legacy.axes" to TRUE.
  # customize the plot
  roc(Object_Visibility, glm.fit$fitted.values, print.thres = 0.04, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Alarm Rate", ylab="Probability of Detection", col="#377eb8", lwd=4)

  # modify plot by object depth/composition and add AUC
  if(i == 1){
    plot(smooth(test, method="density"), col="red", lwd=2, legacy.axes = TRUE, xlab="FAR", ylab=expression('P'['d']), 
         cex.lab = 1.5, print.auc=TRUE, print.auc.cex = 1.5)}
  if(i == 2){
    plot(smooth(test, method="density"), col="blue", lwd=2, legacy.axes = TRUE, xlab="FAR", ylab=expression('P'['d']), 
         cex.lab = 1.5, print.auc=TRUE, print.auc.cex = 1.5)}
  if(i == 3){
    plot(smooth(test, method="density"), col="orange", lwd=2, legacy.axes = TRUE, xlab="FAR", ylab=expression('P'['d']), 
         cex.lab = 1.5, print.auc=TRUE, print.auc.cex = 1.5)}
  if(i == 4){
    plot(smooth(test, method="density"), col="green", lwd=2, legacy.axes = TRUE, xlab="FAR", ylab=expression('P'['d']), 
         cex.lab = 1.5, print.auc=TRUE, print.auc.cex = 1.5)}
  
  # turn off graphics device
  dev.off()
  
} # end for (i in 1:length...) condition

