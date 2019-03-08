install.packages("arules")
install.packages("recommenderlab")
install.packages("lazy")
install.packages("Rmpfr")
install.packages("mlr")

library(mlr)
library(recommenderlab)
library(tidyr)
library(dplyr)

#######################################Collaborative Filtering###################################################################

#Reading the data
user_artists = read.table("user_artists.dat",header = TRUE,sep='\t')
mean(user_artists$weight)
sd(user_artists$weight)


#Checking the normalisation and skewness of data

normaliseddata = dnorm(user_artists$weight,mean=745.2439,sd=3751.322)

hist(normaliseddata,prob = TRUE)

max(user_artists$weight)
min(user_artists$weight)


#Calculating a new column with the log values for optimisation
user_artists$log_weight = log(user_artists$weight) 

#Using linear model to find the coefficent
model = lm(log_weight ~ weight, user_artists)

summary(model)

#Adding the coefficeint value to the log value to get a uniformed weightage distribution
user_artists$log_weight = log(user_artists$weight) + 5.389e+00
  
#plotting histogram
hist(user_artists$log_weight)
str(user_artists)

#deleting the weight column
user_artists_updated = user_artists
user_artists_updated$weight = NULL

#filtering the users for normalisation and weights > 10 and number of users > 5

artists_subset =  user_artists_updated %>% filter(log_weight > 10) %>% group_by(artistID) %>% summarise(Totalusers = n_distinct(userID)) %>% filter(Totalusers > 5)


#subsetting the data with the values from the filtered users
user_artists_updated1 = subset(user_artists_updated, artistID %in% artists_subset$artistID)

length(unique(user_artists_updated1$artistID))
length(unique(user_artists_updated1$userID))

#Spreading the data
user_artists_transform = spread(user_artists_updated1,artistID,log_weight)


#Converting the row names to indices
row.names(user_artists_transform) <- user_artists_transform$userID


#min(transpose_matrix)
user_artists_transform[,1] = NULL

row.names(user_artists_transform)
names(user_artists_transform)

#converting the data into matrix
user_artists_transform_matrix = as(user_artists_transform,"matrix")


# Number of users and artists
nrow(user_artists_transform_matrix)
ncol(user_artists_transform_matrix)

# Min, max and average rating for the artists
min(user_artists_transform_matrix, na.rm=TRUE)
max(user_artists_transform_matrix, na.rm=TRUE)
mean(user_artists_transform_matrix, na.rm=TRUE)


hist(user_artists_transform_matrix)
t = table(is.na(user_artists_transform_matrix))
t

# sparsity 
t[2]/(t[1]+t[2])
#98.18% sparse data


#Split the data into test and train

train_id = sample(1:nrow(user_artists_transform_matrix), 0.7 * nrow(user_artists_transform_matrix))
test_id <- setdiff(1:nrow(user_artists_transform_matrix), train_id)

train_data = user_artists_transform_matrix[train_id,]
test_data = user_artists_transform_matrix[test_id,]


#Cosine correlation for single user

UserBasedCFOneUser(user_artists_transform_matrix,'6',3,10,onlyNew = TRUE)

#Cosine correlation for multiple user

prediction_cosine = UserBasedCF(train_data,test_data,3,15,onlyNew = TRUE)
prediction_cosine_preddata = prediction_cosine$prediction
prediction_cosine_topN = prediction_cosine$topN

write.csv(prediction_cosine_preddata,file = "prediction_cosine_preddata.csv")
write.csv(prediction_cosine_topN,file= "prediction_cosine_topN.csv")

#Pearson correlation for single user
UserBasedCFOneUser_Pearson(user_artists_transform_matrix,'6',3,10,onlyNew = TRUE)


#Pearson correlation for multiple user
prediction_pearson = UserBasedCF_pearson(train_data,test_data,3,15,onlyNew = TRUE)
prediction_pearson_preddata <- as.data.frame(prediction_pearson$prediction)

TopN_pearson <- as.data.frame(prediction_pearson$topN)

write.csv(prediction_pearson_preddata,file ="prediction_pearson_preddata.csv")
write.csv(TopN_pearson,file = "TopN_pearson.csv")

#Using recommender lab

train = as(train_data,"realRatingMatrix")
test = as(test_data,"realRatingMatrix")

recommenderRegistry$get_entry("UBCF", dataType="realRatingMatrix")

recom_Userbased <- Recommender(train, method = "UBCF")

pred_recomm <- predict(recom_Userbased, test, n = 10)

getList(pred_recomm)
pred_recomm@ratings

pred_recomm_data = unlist(pred_recomm)

#########Item based collaborative filtering###########

#Item based collaborative filtering using pearson
prediction_item_pearson = ItemBasedCF_pearson(train_data,test_data,3, 10, onlyNew=TRUE)
prediction_item_pearson_data = prediction_item_pearson$prediction
prediction_item_pearson_TopN = prediction_item_pearson$top
prediction_item_pearson_TopN

write.csv(prediction_item_pearson_data,file= "prediction_item_pearson_data.csv")
write.csv(prediction_item_pearson_TopN,file="prediction_item_pearson_topN.csv")

#Item based collaborative filtering using cosine
prediction_item_cosine = ItemBasedCF(train_data,test_data,3, 15, onlyNew=TRUE)
prediction_item_cosine_data = prediction_item_cosine$prediction
prediction_item_cosine_TopN = prediction_item_cosine$topN
prediction_item_pearson_TopN

write.csv(prediction_item_cosine_data,file="prediction_item_cosine_data.csv")
write.csv(prediction_item_cosine_TopN,file = "prediction_item_cosine_TopN.csv")


#########################################Evaluation Metrics##################

######Userbased#######
##Prediction accuracy with the results from pearson
RSME(prediction_pearson$prediction,test_data)
MAE(prediction_pearson$prediction,test_data)

##Prediction accuracy with the results from cosine
RSME(prediction_cosine$prediction,test_data)
MAE(prediction_cosine$prediction,test_data)

######itembased#######
##Prediction accuracy with the results from pearson
RSME(prediction_item_pearson$prediction,test_data)
MAE(prediction_item_pearson$prediction,test_data)

##Prediction accuracy with the results from cosine
RSME(prediction_item_cosine$prediction,test_data)
MAE(prediction_item_cosine$prediction,test_data)


###userbased classification accuracy
##Classification accuracy with pearson
Classification(prediction_pearson$prediction, test_data, threshold=5, TopN=10)

##Classification accuracy with cosine
Classification(prediction_cosine$prediction, test_data, threshold=5, TopN=10)

###itembased classification accuracy
##Classification accuracy with pearson
Classification(prediction_item_pearson$prediction, test_data, threshold=5, TopN=20)

##Classification accuracy with cosine
Classification(prediction_item_cosine$prediction, test_data, threshold=6, TopN=10)

######Ranking accuracy
install.packages("AUC")

library(AUC)

install.packages("StatRank")
library(StatRank)

####Ranking accuracy user based
#Using pearson function
NDCG(test_data, prediction_pearson$prediction, 5)
AUC(test_data, prediction_pearson$prediction, 5)

#Using cosine function
NDCG(test_data, prediction_cosine$prediction, 5)
AUC(test_data, prediction_cosine$prediction, 5)

####Ranking accuracy item based
#Using pearson function
NDCG(test_data, prediction_item_pearson$prediction, 5)
AUC(test_data, prediction_item_pearson$prediction, 5)

#Using cosine function
NDCG(test_data, prediction_item_cosine$prediction, 5)
AUC(test_data, prediction_item_cosine$prediction, 10)


#######################Cluster based collaborative filtering ###########################

#Cluster based collaborative filtering on the matrix
Prediction_cluster_matrix = ClusterBasedCF(user_artists_transform_matrix, 3, 150, 75, onlyNew=TRUE)
Prediction_cluster_prediction_matrix = as.data.frame(Prediction_cluster_matrix$prediction)
Prediction_cluster_topN_matrix = as.data.frame(Prediction_cluster_matrix$topN)
Prediction_cluster_topN_matrix

Prediction_cluster_prediction_matrixtest = subset(Prediction_cluster_prediction_matrix, row.names(Prediction_cluster_prediction_matrix) %in% row.names(test_data))

dim(test_data)
row.names(test_data)
row.names(Prediction_cluster_prediction_matrix)


#Evaluation metrics- Clusterbased

#Prediction accuracy2
RSME(Prediction_cluster_prediction_matrixtest,test_data1)


#Classfication accuracy
Classification(Prediction_cluster_prediction_matrixtest,test_data1,threshold = 10,TopN = NA)




#######Prediction using recommender lab overall
algorithms <- list(
  POPULAR = list(name = "POPULAR", param = NULL),
  IBCF = list(name = "IBCF", param = NULL),
  UBCF = list(name = "UBCF", param = NULL),
  SVD = list(name = "SVD", param = NULL)
)

user_artists_transform_matrix1 = as(user_artists_transform_matrix ,"realRatingMatrix")

es <- evaluationScheme(user_artists_transform_matrix1, method="split", train=0.7,  k=1, given=-1)
es

ev <- evaluate(es, algorithms, type="ratings")
ev

avg(ev)


#Classification accuracy using recommender lab
es1 <- evaluationScheme(user_artists_transform_matrix1, method="split", train=0.7,  k=1, given=-1, goodRating = 10)
es1

ev1 <- evaluate(es1, algorithms, type="topNList")

avg(ev1)
