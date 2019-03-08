
####Functions for user based CF using pearson correlation

meandiff <- function(data,i){
  data[i,] - mean(data[i,],na.rm=TRUE)
}
 
 #userbased CF for multiple users
 UserBasedCF_pearson <- function(train_data, test_data, N, NN, onlyNew=TRUE){
   
   ### similarity ###
   similarity_matrix <- matrix(, nrow = nrow(test_data), ncol = nrow(train_data), 
                               dimnames = list(rownames(test_data), rownames(train_data)))
   
   for (i in rownames(test_data)){
     for (j in rownames(train_data)){
       sim = sum(meandiff(test_data,i) * meandiff(train_data,j), na.rm = TRUE) / 
         (sqrt(sum(meandiff(test_data,i)^2,na.rm=TRUE)) * 
            sqrt(sum(meandiff(train_data,j)^2,na.rm=TRUE)))
       similarity_matrix[i,j] <- sim
     }
   } 
   print("similarity calculation done")
   ### Nearest Neighbors ###
   similarity_matrix_NN <- similarity_matrix
   
   for (k in 1:nrow(similarity_matrix_NN)){
     crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
     similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], NA)
   }
   
   print("Nearest Neighbor selection done")
   ### Prediction ###
   # Prepare
   prediction <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
   prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                         dimnames=list(rownames(test_data), colnames(test_data)))
   
   TopN <- matrix(, nrow=nrow(test_data), ncol=N, dimnames=list(rownames(test_data)))
   ### Numerator ###
   for (u in rownames(test_data)){
     similarity_vector <- na.omit(similarity_matrix_NN[u, ])
     
     NN_norm <- train_data[rownames(train_data) %in% names(similarity_vector),]
     
     CM <- colMeans(train_data, na.rm=TRUE)
     for (l in 1:ncol(NN_norm)){
       NN_norm[,l] <- NN_norm[,l] - CM[l]
     }
     NN_norm[is.na(NN_norm)] <- 0
     
     # Numerator
     Num = similarity_vector %*% NN_norm
     
     #Prediction
     prediction[u, ] =  mean(test_data[u, ], na.rm=TRUE)  + (Num/sum(similarity_vector, na.rm=TRUE))
     
     
     if (onlyNew == TRUE){
       unseen <- names(test_data[u, is.na(test_data[u,])])
       prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
     }else{
       prediction2[u, ] <- prediction[u, ]
     }
     
     TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
     
   }
   
   print("Prediction done")
   
   res <- list(prediction, TopN)
   names(res) <- c('prediction', 'topN')
   
   return(res)
 }
 
 #Pearson function for one user
 
 UserBasedCFOneUser_Pearson <- function(dataset, user, N, NN, onlyNew=TRUE){
   
   ### similarity ###
   similarity_vect <- vector(, nrow(dataset))
   names(similarity_vect) <- rownames(dataset)
   for (i in rownames(dataset)){
     if (i != user){
       #sim <- sum(dataset[user, ]*dataset[i,], na.rm=TRUE)/sqrt(sum(dataset[user, ]^2, na.rm=TRUE) * sum(dataset[i, ]^2, na.rm=TRUE))
       sim = sum(meandiff(dataset,user) * meandiff(dataset,i), na.rm = TRUE) / 
         (sqrt(sum(meandiff(dataset,user)^2,na.rm=TRUE)) * 
            sqrt(sum(meandiff(dataset,i)^2,na.rm=TRUE)))
       similarity_vect[i] <- sim
     }
   }
   
   ### Nearest Neighbors ###
   crit_val <- -sort(-similarity_vect)[NN]
   similarity_vect <- na.omit(ifelse(similarity_vect >= crit_val, similarity_vect, NA))
   
   ### Prediction ###
   # Prepare
   NN_norm <- dataset[rownames(dataset) %in% names(similarity_vect),]
   CM <- colMeans(dataset, na.rm=TRUE)
   for (l in 1:ncol(NN_norm)){
     NN_norm[,l] <- NN_norm[,l] - CM[l]
   }
   NN_norm[is.na(NN_norm)] <- 0
   
   # Numerator
   Num = similarity_vect %*% NN_norm
   
   #Prediction
   prediction = mean(dataset[user, ], na.rm=TRUE) + (Num/sum(similarity_vect, na.rm=TRUE))
   names(prediction) = colnames(dataset)
   
   if (onlyNew == TRUE){
     unseen <- names(dataset[user, is.na(dataset[user,])])
     prediction <- prediction[names(prediction) %in% unseen]
   }
   TopN <- head(-sort(-prediction), N)
   
   return(TopN)
 }
 
 #Cosine for multiple users
 UserBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
   
   ### similarity ###
   similarity_matrix <- matrix(, nrow = nrow(test_data), ncol = nrow(train_data), 
                               dimnames = list(rownames(test_data), rownames(train_data)))
   
   for (i in rownames(test_data)){
     for (j in rownames(train_data)){
       sim <- sum(test_data[i, ]*train_data[j,], na.rm=TRUE)/sqrt(sum(test_data[i, ]^2, na.rm=TRUE) * sum(train_data[j, ]^2, na.rm=TRUE))
       similarity_matrix[i,j] <- sim
     }
   } 
   print("similarity calculation done")
   ### Nearest Neighbors ###
   similarity_matrix_NN <- similarity_matrix
   
   for (k in 1:nrow(similarity_matrix_NN)){
     crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
     similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], NA)
   }
   
   print("Nearest Neighbor selection done")
   ### Prediction ###
   # Prepare
   prediction <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
   prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                         dimnames=list(rownames(test_data), colnames(test_data)))
   
   TopN <- matrix(, nrow=nrow(test_data), ncol=N, dimnames=list(rownames(test_data)))
   ### Numerator ###
   for (u in rownames(test_data)){
     similarity_vector <- na.omit(similarity_matrix_NN[u, ])
     
     NN_norm <- train_data[rownames(train_data) %in% names(similarity_vector),]
     
     CM <- colMeans(train_data, na.rm=TRUE)
     for (l in 1:ncol(NN_norm)){
       NN_norm[,l] <- NN_norm[,l] - CM[l]
     }
     NN_norm[is.na(NN_norm)] <- 0
     
     # Numerator
     Num = similarity_vector %*% NN_norm
     
     #Prediction
     prediction[u, ] =  mean(test_data[u, ], na.rm=TRUE)  + (Num/sum(similarity_vector, na.rm=TRUE))
     
     
     if (onlyNew == TRUE){
       unseen <- names(test_data[u, is.na(test_data[u,])])
       prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
     }else{
       prediction2[u, ] <- prediction[u, ]
     }
     
     TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
     
   }
   
   print("Prediction done")
   
   res <- list(prediction, TopN)
   names(res) <- c('prediction', 'topN')
   
   return(res)
 }
 
 #Cosine for single user
 UserBasedCFOneUser <- function(dataset, user, N, NN, onlyNew=TRUE){
   
   ### similarity ###
   similarity_vect <- vector(, nrow(dataset))
   names(similarity_vect) <- rownames(dataset)
   for (i in rownames(dataset)){
     if (i != user){
       sim <- sum(dataset[user, ]*dataset[i,], na.rm=TRUE)/sqrt(sum(dataset[user, ]^2, na.rm=TRUE) * sum(dataset[i, ]^2, na.rm=TRUE))
       similarity_vect[i] <- sim
     }
   }
   
   ### Nearest Neighbors ###
   crit_val <- -sort(-similarity_vect)[NN]
   similarity_vect <- na.omit(ifelse(similarity_vect >= crit_val, similarity_vect, NA))
   
   ### Prediction ###
   # Prepare
   NN_norm <- dataset[rownames(dataset) %in% names(similarity_vect),]
   CM <- colMeans(dataset, na.rm=TRUE)
   for (l in 1:ncol(NN_norm)){
     NN_norm[,l] <- NN_norm[,l] - CM[l]
   }
   NN_norm[is.na(NN_norm)] <- 0
   
   # Numerator
   Num = similarity_vect %*% NN_norm
   
   #Prediction
   prediction = mean(dataset[user, ], na.rm=TRUE) + (Num/sum(similarity_vect, na.rm=TRUE))
   names(prediction) = colnames(dataset)
   
   if (onlyNew == TRUE){
     unseen <- names(dataset[user, is.na(dataset[user,])])
     prediction <- prediction[names(prediction) %in% unseen]
   }
   TopN <- head(-sort(-prediction), N)
   
   return(TopN)
 }
 
 #Item based using pearson correlation
 meandiff2 <- function(data,i){
   data[,i] - mean(data[,i],na.rm=TRUE)
 }
 
 
 ItemBasedCF_pearson <- function(train_data, test_data, N, NN, onlyNew=TRUE){
   similarity_matrix = matrix(, ncol=ncol(test_data), nrow=ncol(train_data), dimnames = list(colnames(test_data), colnames(train_data)))
   for (i in colnames(test_data)){
     for (j in colnames(train_data)){
       sim = sum(meandiff2(test_data,i) * meandiff2(train_data,j), na.rm = TRUE) /
         (sqrt(sum(meandiff2(test_data,i)^2,na.rm=TRUE)) *
            sqrt(sum(meandiff2(train_data,j)^2,na.rm=TRUE)))
       similarity_matrix[i,j] <- sim
     }
   }
   print("Similarity calculation done")
   
   # Nearest Neighbor
   similarity_matrix_NN <- similarity_matrix
   
   for (k in 1:ncol(similarity_matrix_NN)){
     crit_val <- -sort(-similarity_matrix_NN[,k])[NN]
     similarity_matrix_NN[,k] <- ifelse(similarity_matrix_NN[,k] >= crit_val, similarity_matrix_NN[,k], NA)
   }
   similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
   
   train_data[is.na(train_data)] <- 0
   
   test_data2 <- test_data
   test_data2[is.na(test_data2)] <- 0
   
   print("Nearest neighbor selection done")
   
   ### Prediction ###
   prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
   prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                         dimnames=list(rownames(test_data), colnames(test_data)))
   TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
   
   for (u in rownames(test_data)){
     # Numerator
     Num <-  test_data2[u, ] %*% similarity_matrix_NN
     
     # Denominator
     Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
     
     # Prediction
     prediction[u, ] <- Num/Denom
     
     if (onlyNew == TRUE){
       unseen <- names(test_data[u, is.na(test_data[u,])])
       prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
     }else{
       prediction2[u, ] <- prediction[u, ]
     }
     
     TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
     
   }
   
   print("Prediction done")
   
   res <- list(prediction, TopN)
   names(res) <- c('prediction', 'topN')
   
   return(res)
 }
 
 #Item based using cosine correlation
 
 ItemBasedCF <- function(train_data, test_data, N, NN, onlyNew=TRUE){
   # Similarity
   
   similarity_matrix <- as.matrix(simil(t(train_data), method="cosine"))
   
   print("Similarity calculation done")
   # Nearest Neighbor
   similarity_matrix_NN <- similarity_matrix
   
   for (k in 1:ncol(similarity_matrix_NN)){
     crit_val <- -sort(-similarity_matrix_NN[,k])[NN]
     similarity_matrix_NN[,k] <- ifelse(similarity_matrix_NN[,k] >= crit_val, similarity_matrix_NN[,k], NA)
   }
   similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
   
   train_data[is.na(train_data)] <- 0
   
   test_data2 <- test_data
   test_data2[is.na(test_data2)] <- 0
   
   print("Nearest neighbor selection done")
   
   ### Prediction ###
   prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
   prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                         dimnames=list(rownames(test_data), colnames(test_data)))
   TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
   
   for (u in rownames(test_data)){
     # Numerator
     Num <-  test_data2[u, ] %*% similarity_matrix_NN
     
     # Denominator
     Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
     
     # Prediction
     prediction[u, ] <- Num/Denom
     
     if (onlyNew == TRUE){
       unseen <- names(test_data[u, is.na(test_data[u,])])
       prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
     }else{
       prediction2[u, ] <- prediction[u, ]
     }
     
     TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
     
   }
   
   print("Prediction done")
   
   res <- list(prediction, TopN)
   names(res) <- c('prediction', 'topN')
   
   return(res)
 }
 
 #Clusterbased collaborative filtering function
 
 ClusterBasedCF <- function(data, N, centers, iter, onlyNew=TRUE){
   
   data2 <- data
   
   # fill with average product rating
   colmeans <- colMeans(data2, na.rm=TRUE)
   
   for (j in colnames(data2)){
     data2[, j] <- ifelse(is.na(data2[ ,j]), colmeans[j], data2[, j])
   }
   
   km <- kmeans(data2, centers=centers, iter.max=iter)
   
   head(km$cluster)
   head(km$centers)
   
   
   # Statistics of the groups
   tab <- table(km$cluster)
   
   # Assign users to groups
   RES <- cbind(data, as.data.frame(km$cluster))
   
   # Calculate average ratings for everi cluster
   aggregation <- aggregate(RES, list(RES$"km$cluster"), mean, na.rm=T)
   aggregation <- aggregation[,-1]
   
   # Make a prediction
   users <- as.data.frame(RES$"km$cluster")
   users <- cbind(users, rownames(RES))
   colnames(users) <- c("km$cluster", 'rn')
   
   
   prediction = merge(users, aggregation, by="km$cluster")
   rownames(prediction) <- prediction$rn
   
   prediction  <- prediction[order(rownames(prediction)), -1:-2]
   
   prediction2 <- matrix(, nrow=nrow(prediction), ncol(prediction), 
                         dimnames=list(rownames(prediction), colnames(prediction)))
   colnames(prediction2) <- colnames(prediction)
   rownames(prediction2) <- rownames(prediction)
   
   for (u in rownames(prediction)){
     if (onlyNew == TRUE){
       unseen <- names(data[u, is.na(data[u,])])
       
       prediction2[u, ] <- as.numeric(t(ifelse(colnames(prediction) %in% unseen, prediction[u, ], as.numeric(NA))))
     }else{
       prediction2[u, ] <- prediction[u, ]
     }
   }
   
   # TopN
   TopN <- t(apply(prediction, 1, function(x) names(head(sort(x, decreasing=TRUE), 5))))
   
   print("Prediction done")
   
   res <- list(prediction, TopN)
   names(res) <- c('prediction', 'topN')
   
   return(res)
 } 

 ContentBased <- function(product_data, test_data, N, NN, onlyNew=TRUE){
   
   # Similarity calculation (stolen from user-based CF)
   similarity_matrix <- as.matrix(simil(product_data, method="cosine"))
   
   print("Similarity calculation done")
   
   # Set Nearest neighbors (stolen from user-based CF)
   similarity_matrix_NN <- similarity_matrix
   
   for (k in 1:nrow(similarity_matrix_NN)){
     crit_val <- -sort(-similarity_matrix_NN[k,])[NN]
     similarity_matrix_NN[k,] <- ifelse(similarity_matrix_NN[k,] >= crit_val, similarity_matrix_NN[k,], 0)
   }
   
   similarity_matrix_NN[is.na(similarity_matrix_NN)] <- 0
   test_data2 <- test_data
   test_data2[is.na(test_data2)] <- 0
   
   print("Nearest neighbor selection done")
   
   ### Prediction (stolen from item-based CF) ###
   prediction <- matrix(, nrow=nrow(test_data), ncol=ncol(test_data), 
                        dimnames=list(rownames(test_data), colnames(test_data)))
   prediction2 <- matrix(, nrow=nrow(test_data), ncol(test_data), 
                         dimnames=list(rownames(test_data), colnames(test_data)))
   TopN <- matrix(, nrow=nrow(test_data), N, dimnames=list(rownames(test_data)))
   
   for (u in rownames(test_data)){
     # Numerator
     Num <-  test_data2[u, ] %*% similarity_matrix_NN
     
     # Denominator
     Denom <- colSums(similarity_matrix_NN, na.rm=TRUE)
     
     # Prediction
     prediction[u, ] <- Num/Denom
     
     if (onlyNew == TRUE){
       unseen <- names(test_data[u, is.na(test_data[u,])])
       prediction2[u, ] <- ifelse(colnames(prediction) %in% unseen, prediction[u, ], NA)
     }else{
       prediction2[u, ] <- prediction[u, ]
     }
     
     TopN[u, ] <- names(-sort(-prediction2[u, ])[1:N])
     
   }
   
   print("Prediction done")
   
   res <- list(prediction, TopN)
   names(res) <- c('prediction', 'topN')
   
   return(res)
 }
 
 
 #####Evaluation Metrics
 
 ### Prediction Accuracy ###
 ###########################
 
 RSME <- function(prediction, real){
   
   if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
     RSME = sqrt( sum( (prediction - real)^2 , na.rm = TRUE ) / (nrow(prediction) * ncol(prediction)) )
     return(RSME)
   }else{
     return("Dimension of prediction are not equal to dimension of real")
   }
 }
 
 MAE <- function(prediction, real){
   
   if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
     RSME = sum( Mod(prediction - real) , na.rm = TRUE ) / (nrow(prediction) * ncol(prediction)) 
     return(RSME)
   }else{
     return("Dimension of prediction are not equal to dimension of real")
   }
 }
 
#########Classification accuracy#########
 
 Classification <- function(prediction, real, threshold=NA, TopN=NA){
   if (nrow(prediction) == nrow(real) & ncol(prediction) == ncol(real)){
     # Threshold #
     if (!is.na(threshold)){
       TP = sum(ifelse(prediction >= threshold & real >= threshold, 1, 0), na.rm=T)
       FP = sum(ifelse(prediction >= threshold & real < threshold, 1, 0), na.rm=T)
       FN = sum(ifelse(prediction < threshold & real >= threshold, 1, 0), na.rm=T)
       Recall = TP/(TP+FN)
       Precision = TP/(TP+FP)
       F1 = 2 * ((Precision * Recall) / (Precision + Recall))
       Class_Thres = list(Recall, Precision, F1)
       names(Class_Thres) = c("Recall","Precision","F1")
     }
     if (!is.na(TopN)){
       TP = vector(, length = nrow(prediction))
       FP = vector(, length = nrow(prediction))
       FN = vector(, length = nrow(prediction))
       
       for (u in nrow(prediction)){
         threshold_pred = -sort(-prediction[u, ])[TopN]
         threshold_real = -sort(-real[u, ])[TopN]
         TP[u] = sum(ifelse(prediction[u, ] >= threshold_pred & real[u, ] >= threshold_real, 1, 0), na.rm=T)
         FP[u] = sum(ifelse(prediction[u, ] >= threshold_pred & real[u, ] < threshold_real, 1, 0), na.rm=T)
         FN[u] = sum(ifelse(prediction[u, ] < threshold_pred & real[u, ] >= threshold_real, 1, 0), na.rm=T)
       }
       TP = sum(TP[u])
       FP = sum(FP[u])
       FN = sum(FN[])
       Recall = TP/(TP+FN)
       Precision = TP/(TP+FP)
       F1 = 2 * ((Precision * Recall) / (Precision + Recall))
       Class_TopN = list(Recall, Precision, F1)
       names(Class_TopN) = c("Recall", "Precision", "F1")
     }
     
     if (!is.na(threshold) & !is.na(TopN)){
       Class = list(Class_Thres, Class_TopN)
       names(Class) = c("Threshold", "TopN")
     }else if (!is.na(threshold) & is.na(TopN)) {
       Class = Class_Thres
     }else if (is.na(threshold) & !is.na(TopN)) {
       Class = Class_TopN
     }else{
       Class = "You have to specify the 'Threshold' or 'TopN' parameter!"
     }
     return(Class)  
   }else{
     return("Dimension of prediction are not equal to dimension of real")
   }
 }
 
####### Ranking accuracy########
 
 AUC <- function(real, prediction, threshold){
   
   pred <- ifelse(prediction >= threshold, 1, 0)
   real <- ifelse(real >= threshold, 1, 0)
   
   real[is.na(real)] <- 0
   pred[is.na(pred)] <- 0
   
   ROC <- roc(factor(prediction), factor(real))
   
   plot(ROC)
   
   AUC <- auc(ROC)
   return(AUC)
 }
 
 NDCG <- function(real, prediction, TopN){
   for (u in rownames(real)){
     
     # compute ranking 
     rank <- sort(-rank(prediction[u,]))[1:TopN]
     
     
     # Create NDCG vector
     if ( u == rownames(real)[1]){
       NDCG_vect <- Evaluation.NDCG(rank, real[u, names(rank)])
     }else{
       NDCG_vect <- rbind(NDCG_vect, Evaluation.NDCG(rank, real[u, names(rank)]))
     }
   }
   
   # Compute avarege NDCG
   NDCG_vect[is.na(NDCG_vect)] <- 0
   NDCG <- colMeans(NDCG_vect, na.rm=T)
   names(NDCG) <- "NDCG"
   return(NDCG)
 }
 
 