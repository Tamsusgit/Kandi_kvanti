########################################
## Imaging spectroscopy 2023 practical 5
########################################

# In this part of the practical we will classify two target tree species
# Acacia mearnsii and Eucalyptus from the other species, using all hyperspectral
# bands as predictors.

# Clean the environment and the memory
rm(list = ls())
graphics.off()

# set working directory
setwd("/Practical5")

# uncomment if you're missing any of the packages
#install.packages("caret")
#install.packages("terra")
#install.packages("tidyverse")

# Load packages
library(terra)
library(caret)
library(tidyverse)

# read tree data
df = read_csv("./data/trees_full.csv")

# Lets see how many pixels we have for each tree species
table(df$Sp_Name)

# There are so many species that we want to combine some of them. In this case 
# we only try to separate Acacia mearnsii and Eucalyptus spp. from any other species.
df$Sp_Name[!df$Sp_Name %in% c("Acacia mearnsii", "Eucalyptus sp.")] = "other"
df$Sp_Name = as.factor(df$Sp_Name)
df = droplevels(df)
df = df[,-1]

# Lets see what the frequencies look like now 
table(df$Sp_Name)

# Partitioning of the data into training and testing datasets is a fundamental
# task in most of the classification workflows. We should always test the model
# with a data that was not used for the training of the model. This ensures that
# the model has not "seen" the test data and learned it's features. Running tests
# with independent dataset makes it possible to see how the model performs on a
# new, unseen data.

# Partition the data into training and testing datasets
set.seed(42)
trainIndex <- createDataPartition(df$Sp_Name, p = .7, 
                                  list = FALSE, 
                                  times = 1)
dtrain <- df[trainIndex,]
dtest  <- df[-trainIndex,]

### Defining the fitcontrol
# Method: defines the cross validation technique used  during the model building.
# cv is a simple cross validation where the data is divided into parts. The
# number determines to how many parts the data is divided. The model is then
# trained by leaving one of the data partitions out and using rest to train the model
#
# At least 10-CV is a good option, but we'll keep it smaller to reduce computing time.
fitControl <- trainControl(
  method = "cv",
  number = 2, # 2-fold-CV
  verboseIter = TRUE
)

# Train the classification model
svm_rbf <- train(Sp_Name ~., data = dtrain, 
                 method = "svmRadial", # options: svmRadial rf svmLinear
                 trControl = fitControl,
                 tuneLength = 10,
                 allowParallel = TRUE)

# Check the results
svm_rbf

# Predict
test_pred_svm_rbf <- predict(svm_rbf, newdata = dtest)

# Calculate confusion matrix
confusionMatrix(test_pred_svm_rbf, dtest$Sp_Name, mode="prec_recall")

# Task: try one other classification algorithm (svmLinear or rf [random forest])
#
# Report the results of the best model. Answer also the following questions.
#
# Which of the species had the highest precision?
#
# Which of the species had the highest recall?
#
# Note!
# Precision is the same as user's accuracy used commonly in remote sensing studies
# Recall is the as producer's accuracy used commonly in remote sensing studies
# Classic wikipedia reference to precision and recall:
# https://en.wikipedia.org/wiki/Precision_and_recall
#
# If a species has high precision and low recall then how this will impact
# your results when you make a map using this model?
#
# If a species has low precision and high recall then how this will impact
# your results when you make a map using this model?


# Finally, use the model to make predictions on a raster data.
# Open small subset of the aisa eagle data
aisa <- rast("data/hs/mosaic/aisa_eagle.tif")

# The predictor names have to be same as in the model. Get the predictor names
# (band names) from the data table and rename raster layers
n <- colnames(dtrain)
n <- n[-1]
names(aisa) <- n

# plot the raster data and add it to your report
# (note that all the non-tree pixels have been masked with canopy height model)
plotRGB(aisa, r = 55, g = 35, b = 20, stretch = "lin", smooth=F)

# use the classification model to predict tree species in the raster
pred <- terra::predict(aisa, svm_rbf, na.rm=TRUE)

# plot the prediction and add the map to your report
plot(pred, col=rainbow(3))

# Training a model can take a lot of time, so it's good to know how to save them.
# This is how you can export models as R-objects for later use.
dir.create("./data/models/", showWarnings = FALSE)
saveRDS(svm_rbf, "./data/models/svm_rbf.rds")

# and this is how you can open them
my_model <- readRDS("data/models/svm_rbf.rds")
my_model

## End of the practical ##


