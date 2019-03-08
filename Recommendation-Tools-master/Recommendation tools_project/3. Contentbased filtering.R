##########################################################################################
# CONTENT BASED FILTERING
##########################################################################################

library(readr)
library(dplyr)
library(tidyverse)
library(tidyr)

# import user_taggedartists file
user_taggedartists <-  read.table("user_taggedartists.dat", header=TRUE) %>% select(userID, artistID, tagID)

# import tags file
tags <-  read.delim("tags.dat",header=TRUE) 




# import arts file
art <- read_delim("artists.dat", delim = "\t") %>% select(id, name)
art$name <- iconv(art$name, from = "UTF-8", to = "ASCII//TRANSLIT")


# extract count of tags for each group of artists and tagID

tags_counts <- arrange(summarise(group_by(user_taggedartists, tagID), 
                                 TotalUsers = length(unique(userID)) ), desc(TotalUsers) )

#length(unique(user_taggedartists$tagID))
tag_top200 <- tags_counts

# Take top 200 tags
tag_top200 <- arrange(tag_top200, tagID)

# subset tags which is having top 200 
tag_top200$Names <- subset(tags, tagID %in% tag_top200$tagID)$tagValue

# Selecting the Top 200 Tags based on Maximum number of users

tag_top200 <- arrange(tag_top200, desc(TotalUsers))


toptags <- subset(user_taggedartists, tagID %in% tag_top200$tagID)

#Selecting only those Artists which are used by CF Reccomendation Systems
testart <- subset(user_taggedartists, artistID %in% user_artists$artistID)
testart1 <- subset(testart, artistID %in% user_artists_updated1$artistID)

# Deleting couple of issue records.
user_artists_updated2 <- user_artists_updated1[!user_artists_updated1$artistID  == "5533",]
user_artists_updated3 <- user_artists_updated2[!user_artists_updated2$artistID  == "4941" ,]






#tag_top200 <- tags_counts[1:200,]

summarized_tag <- summarise(group_by(toptags, artistID, tagID ), Count = length(tagID) )

summarized_tag <- subset(summarized_tag, artistID  %in%  user_artists_updated2$artistID)


# Creating the base Matrix

matrix <- spread(summarized_tag, tagID, Count)

row.names(matrix) <- matrix$artistID

matrix[,][is.na(matrix[,])] <- 0

ag_artistID <- as.vector(matrix$artistID)
ag_mat <- as.matrix(matrix[,2:ncol(matrix)])
rm(matrix)

ntags <- length(as.vector(ag_mat))
ntags

sum(!is.na(as.vector(ag_mat)) ) / ntags
1 - sum(!is.na(as.vector(ag_mat)) ) / ntags

# Creating the Final Base Matrix for Content Based RS

fin_matrix <- ag_mat

fin_matrix[,][is.na(fin_matrix[,])] <- 0
fin_matrix[,][fin_matrix[,] > 0] <- 1


nrow(fin_matrix)
ncol(fin_matrix)

##########Updating original user based matrix
user_artists_updated2 <- user_artists_updated1[!user_artists_updated1$artistID  == "5533",]
user_artists_updated3 <- user_artists_updated2[!user_artists_updated2$artistID  == "4941" ,]



user_artists_new =spread(user_artists_updated3,artistID,log_weight)

#Converting the row names to indices
row.names(user_artists_new) <- user_artists_new$userID


#min(transpose_matrix)
user_artists_new[,1] = NULL

#row.names(user_artists_transform)
#names(user_artists_transform)

#converting the data into matrix
user_artists_transform_new = as(user_artists_new,"matrix")

write.csv(user_artists_transform_new , file = "user_artists_transform_new.csv")
write.csv(fin_matrix,file="content_matrix.csv")

CB_updated <- ContentBased(fin_matrix, user_artists_transform_new, 3, 10, onlyNew=T)
CB_updated_pred = CB_updated

CB_updated$prediction
CB_updated$topN

##############################################################  
### Quantitative Evauation & comparison with item-based CF ###
##############################################################

# Load Models

# Split train - Test
set.seed(2)
train_rows = sample(1:nrow(user_artists_transform_new), 0.7*nrow(user_artists_transform_new))

train_content <- as(user_artists_transform_new, "matrix")[train_rows,]
test_content <- as(user_artists_transform_new, "matrix")[-train_rows,]


# Score Models

ptm <- proc.time()
CB <- ContentBased(fin_matrix, test_content, 3, 10, onlyNew=T)
Time <- (proc.time() - ptm)
Time

### Prediction Accuracy ###
###########################

# RSME Content-based
RSME(CB$prediction, test_content)

MAE(CB$prediction, test_content)

### Classification Accuracy ###
##############################

# Recall/precision Content-based
Classification(CB$prediction, test_content, threshold=3)




#####################################################################################
#####################################################################################
############### Qualitative Results :

library(recommenderlab)
art_sim <- similarity(as(fin_matrix, "binaryRatingMatrix"), method = "cosine",
                      which = "users")

# convert to an R matrix
art_sim <- as(art_sim, "matrix")

# round to 3 digit precision
art_sim[][] <- round(art_sim[][],3)

# # name rows + cols according to artistID for easy retrieval
colnames(art_sim) <- ag_artistID
rownames(art_sim) <- ag_artistID


##############################################################
# set number of similar artists to recommend
n_recommended <- 5

# randomly select a user
artist <- sample(ag_artistID, 1)

# get name of artist from artist list
a_name <- art[art$id == artist,]$name

# fetch their recommendations: this returns a named vector sorted by similarity
# the names of the items are the artist IDs
arecs <- sort(art_sim[as.character(artist),], decreasing = TRUE)[1:n_recommended]

# extract the artist IDs and convert to numeric
arecs_IDs <- as.numeric(names(arecs))

# create list of artist names from artist ID's in list
arec_names <- art[art$id %in% arecs_IDs,]$name

# create a heading for the list of similar artists
table_head <- sprintf("Artists Similar to %s", a_name)

# display the list of similar artists
library(knitr)
kable(arec_names, col.names = table_head)

###################################################################################
# Generate a Top N Artist List by Genre

set.seed(42)

# set rownames = artistID's for easy retrieval - DON'T NEED THIS LINE OF CODE IN SHINY
rownames(ag_mat) <- ag_artistID

# extract the genre tagIDs from matrix and convert to numeric
tagIDs <- as.numeric(colnames(ag_mat))

# set number of artists to recommend
n_recommended <- 5

# randomly select a genre
tagID <- sample(tagIDs, 1)

# get name of genre from tagID list
g_name <- tags[tags$tagID == tagID,]$tagValue

# fetch the top N artists:
# the names of the items are the artist IDs
g_arecs <- sort(ag_mat[,as.character(tagID)], decreasing = TRUE)[1:n_recommended]

# extract the artist IDs and convert to numeric
g_arecs_IDs <- as.numeric(names(g_arecs))

# create list of artist names from artist ID's in list
g_arec_names <- art[art$id %in% g_arecs_IDs,]$name

# create a heading for the list of similar artists
table_head <- sprintf("Top Artists in %s genre:", g_name)

# display the list of similar artists
kable(g_arec_names, col.names = table_head)

####################################################################################
###################################################################################

