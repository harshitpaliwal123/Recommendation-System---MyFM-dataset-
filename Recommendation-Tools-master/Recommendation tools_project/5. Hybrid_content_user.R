#####################
### Hybrid RecSys ###
#####################
library("recommenderlab")
library("tm")
library("SnowballC")
library("dbscan")
library("proxy")





### Split train - Test ###
set.seed(2)


train_rows = sample(1:nrow(user_artists_transform_matrix), 0.7*nrow(user_artists_transform_matrix))

train <- as(user_artists_transform_matrix, "matrix")[train_rows,]
test <- as(user_artists_transform_matrix, "matrix")[-train_rows,]


### Compute individual models ###
CBTFIDF <- ContentBased(fin_matrix, test, 3, 10, onlyNew=F)
IB <- UserBasedCF(train, test, 3, 10, onlyNew=F)

### Transform results to lists (to be able to use the rowMeans function) ###
CBTFIDF_list <- as.list(CBTFIDF$prediction)
IB_list <- as.list(IB$prediction)

####################
### Compute Mean ###
####################
hybrid <- rowMeans(cbind(as.numeric(CBTFIDF_list), as.numeric(IB_list)), na.rm=T)

### Transform list back to matrix with correct number of dimensions ###
Hybrid_prediction <- matrix(hybrid, nrow=nrow(test), ncol=ncol(test))
rownames(Hybrid_prediction) <- rownames(test)
colnames(Hybrid_prediction) <- colnames(test)

### Evaluate ###
# RSME

RSME(Hybrid_prediction, test)

# Classification

Classification(Hybrid_prediction, test, threshold=5)

