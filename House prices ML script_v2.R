if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")
if(!require(funModeling)) install.packages("funModeling", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")


set.seed(1, sample.kind="Rounding")

# NE PAS OUBLIER CHEMINS ONLINE!!!

train_set <- fread(input = "./data/train.csv", sep = ",", nrows = -1, header = TRUE, check.names = TRUE, blank.lines.skip = TRUE, data.table = TRUE, strip.white = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", "N/A", "NULL"))
test_set <- fread(input = "./data/test.csv", sep = ",", nrows = -1, header = TRUE, check.names = TRUE, blank.lines.skip = TRUE, data.table = TRUE, strip.white = TRUE, stringsAsFactors = FALSE, na.strings = c("NA", "N/A", "NULL"))

#test_set$SalePrice <- 0 # creation of the SalePrice column missing from the test_set
#full_set <- rbind(train_set, test_set) # assembling the two sets for visualization purposes

###-----Data exploration, visualization, and treatment-----

summary(train_set) # get summary statistics
NA_Zeros <- df_status(train_set) # information about NAs and zeros

NAs <- ggplot(filter(NA_Zeros,q_na > 5), aes(reorder(variable, p_na),p_na)) +
  ggtitle("NAs variables") + 
  xlab("Variables") + 
  ylab("%") +
  geom_bar(stat = "identity") +
  coord_flip()
NAs # Plotting of NAs to have a visual on variables to potentially discard

Zeros <- ggplot(filter(NA_Zeros,q_zeros > 0), aes(reorder(variable, p_zeros),p_zeros)) +
  ggtitle("Proportion of zeros") +
  ylab("%") +
  xlab("Variables") +
  geom_bar(stat = "identity") +
  coord_flip()
Zeros # Plotting of Zeros to have a visual on variables to potentially discard

zerovar_names <- nearZeroVar(train_set, names=TRUE) # Checking for low variance variables
zerovar_index <- nearZeroVar(train_set)

train_set <- train_set %>% select (-all_of(zerovar_index), -PoolQC, -Fence, -Alley, -FireplaceQu) # filterout variables with too many NAs, with low Var and lot of zeros
test_set <- test_set %>% select (-all_of(zerovar_index), -PoolQC, -Fence, -Alley, -FireplaceQu)


# Full NA treatment - Using mostly the most common value as replacement
Mostcommon <- function(x) {
  sort(table(x), decreasing = TRUE)[1]
}

train_set[is.na(KitchenQual), KitchenQual := Mostcommon(train_set$KitchenQual)]
train_set[is.na(GarageFinish) & GarageType == "Detchd", ':=' 
         (GarageFinish = "Fin",
           GarageCars = 1,
           GarageArea = 360,
           GarageYrBlt = YearBuilt)] 
train_set[is.na(GarageFinish), GarageFinish := "None"]
train_set[is.na(GarageType), GarageType := "None"]
train_set[is.na(GarageYrBlt), GarageYrBlt := 0]
train_set[is.na(BsmtExposure) & BsmtFinType1 == "Unf" , BsmtExposure := "No"]
train_set[is.na(BsmtExposure), BsmtExposure := "None"]
train_set[is.na(BsmtFinType1), BsmtFinType1 := "None"]
train_set[is.na(BsmtQual), BsmtQual := Mostcommon(train_set$BsmtQual)]
train_set[is.na(Electrical), Electrical := Mostcommon(train_set$Electrical)]
train_set[is.na(BsmtFullBath),':=' (BsmtFullBath = 0, BsmtHalfBath = 0)] 
train_set[is.na(MSZoning), MSZoning := Mostcommon(train_set$MSZoning)]
train_set[is.na(Electrical) , Electrical  := "Mix"]
train_set[is.na(Exterior1st),':=' (Exterior1st = Mostcommon(train_set$Exterior1st),Exterior2nd = Mostcommon(train_set$Exterior2nd))]
train_set[is.na(MasVnrType),':=' (MasVnrType = "None", MasVnrArea = 0)]
train_set[is.na(SaleType), SaleType := Mostcommon(train_set$SaleType)]
train_set[, LotFrontage := replace(LotFrontage, is.na(LotFrontage), median(LotFrontage, na.rm=TRUE)), by=.(Neighborhood)] # Using median frontage in neighborhood


##----- Recoding 

train_set[,KitchenQual:=ordered(KitchenQual, levels = c("Po","Fa","TA","Gd","Ex"))]
train_set[,GarageFinish:=ordered(GarageFinish, levels = c("None","Unf","RFn","Fin"))]
train_set[,ExterQual:=ordered(ExterQual, levels = c("Po","Fa","TA","Gd","Ex"))]
train_set[,ExterCond:=ordered(ExterCond, levels = c("Po","Fa","TA","Gd","Ex"))]
train_set[,BsmtExposure:=ordered(BsmtExposure, levels = c("None","No","Mn","Av","Gd"))]
train_set[,BsmtFinType1:=ordered(BsmtFinType1, levels = c("None","Unf","LwQ","Rec","BLQ","ALQ","GLQ"))]
train_set[,Electrical:=ordered(Electrical, levels = c("FuseP","Mix","FuseF","FuseA","SBrkr"))]
train_set[is.na(Electrical) , Electrical  := "Mix"] # One electrical should be badly encoded so I replace it manually


###-----Correlations

numeric_matrix_train <- train_set %>% dplyr::select(where(is.numeric))
correlation_train <- cor(numeric_matrix_train, use = "everything")
corrplot_train <- corrplot(correlation_train, method = "circle", type = 'lower', diag = TRUE, title = "Correlation matrix", tl.cex = 0.7, tl.col = "deepskyblue4", tl.srt = 30)

###-----Removal of highly correlated variables
train_set <- train_set %>% select(-TotRmsAbvGrd, -TotalBsmtSF, -GarageArea, -YearRemodAdd, -GarageYrBlt, -BsmtFinSF1)
test_set <- test_set %>% select(-TotRmsAbvGrd, -TotalBsmtSF, -GarageArea, -YearRemodAdd, -GarageYrBlt, -BsmtFinSF1)

final_corr <- cor(train_set %>% dplyr::select(where(is.numeric)),  use = "everything")
final_corrplot <- corrplot(final_corr,  method = "circle", type = 'lower', diag = TRUE, title = "Corrected correlation matrix", tl.cex = 0.7, tl.col = "deepskyblue4", tl.srt = 30)


###-----Sales prices plots

ggplot(train_set, aes(OverallQual,SalePrice)) +  # As expected, the overall quality of the house influences the price heavily
  geom_point() +
  geom_smooth(method=lm, color = "deepskyblue4") + 
  xlab("Overall Quality") +
  ylab("Sale Price")

ggplot(train_set, aes(X1stFlrSF,SalePrice)) +  # As expected, the first floor surface of the house influences the price heavily
  geom_point() +
  geom_smooth(method=lm, color = "deepskyblue4") + 
  xlab("First Floor ft²") +
  ylab("Sale Price")

#####----- Modeling

train_index <- createDataPartition(train_set$Id, times = 1, p = 0.9, list=FALSE) # First, don't forget to split the train set in two parts for training and testing
train_subset <- train_set[train_index,]
test_subset <- train_set[-train_index,]

##---- Linear Model

Linear_matrix <- train_subset %>% dplyr::select(where(is.numeric)) # The first model is a simple regression, done only on numerical data
lm_fit <- lm(data = Linear_matrix, formula = SalePrice ~ .)
summary(lm_fit) # R² of 0.80 is a decent but perfectible score.

prediction_lm <- predict(lm_fit, test_subset)
RMSE(log(test_subset$SalePrice), log(prediction_lm)) # 0.145 isn't bad but can surely be perfected as said above.

##---- Random Forest

random_forest <- randomForest(SalePrice ~ ., data = train_subset)
prediction_rf <- predict(random_forest, test_subset)
RMSE(log(test_subset$SalePrice), log(prediction_rf)) # 0.105 is now a very interesting result!


##---- Final model and prediction

rforest_final <- randomForest(SalePrice ~ . , data = train_set)
rforest_final
prediction_final <- predict(rforest_final, test_set) # Can't tell the RMSE as the SalePrice isn't existing in the official test_set of the Kaggle project.


###--- Prepare submission file

prediction_final <- as.data.frame(prediction_final)

names(prediction_final) <- "SalePrice"
submission_file <- test_set %>% select(Id) %>% cbind(prediction_final) 
write.csv(submission_file, file = "Kaggleprediction_v1.csv", row.names = FALSE)


