####Hybrid Recommendation Systems:cluster based and content based

#Load the data from content and clutserbased

# Split train - Test data from clusterbased
set.seed(2)
train_rows = sample(1:nrow(user_artists_transform_new), 0.7*nrow(user_artists_transform_new))

train_content <- as(user_artists_transform_new, "matrix")[train_rows,]
test_content <- as(user_artists_transform_new, "matrix")[-train_rows,]

### Compute individual models ###
#Contentbased
Contentbased <- ContentBased(fin_matrix, test_content, 3, 10, onlyNew=F)
Contentbased$topN
content_pred = Contentbased$prediction

#Clusterbased
clusterbased <- ClusterBasedCF(user_artists_transform_new,  3, 150, 75, onlyNew=T)
clusterbased$topN
cluster_prediction = clusterbased$prediction

Cluster_pred = subset(cluster_prediction, row.names(cluster_prediction) %in% row.names(test_content))
Cluster_pred1 = as(Cluster_pred,"matrix")

### Transform results to lists (to be able to use the rowMeans function) ###
content_list <- as.list(content_pred)
cluster_list <- as.list(Cluster_pred1)

####################
### Compute Mean ###
####################
hybrid <- rowMeans(cbind(as.numeric(content_list), as.numeric(cluster_list)), na.rm=T)

### Transform list back to matrix with correct number of dimensions ###
Hybrid_prediction <- matrix(hybrid, nrow=nrow(test_content), ncol=ncol(test_content))
rownames(Hybrid_prediction) <- rownames(test_content)
colnames(Hybrid_prediction) <- colnames(test_content)

### Evaluate the Metrics for Prediction accuracy and Classification accuracy ###
# Prediction accuracy
RSME(Hybrid_prediction, test_content)
MAE(Hybrid_prediction, test_content)

# Classification
Classification(Hybrid_prediction, test_content, threshold=6)



