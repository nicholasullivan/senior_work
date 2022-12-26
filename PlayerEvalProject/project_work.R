#' 
#' Cleaning data
#' 

library(tidyverse)

salaries <- read.csv("salaries.csv")
stats <- rbind(read.csv("batters.csv"), read.csv("pitchers.csv"))

salaries <- salaries %>%
  janitor::clean_names() %>%
  rename("player" = "i_player") %>%
  select(-c(years, total_value, average_annual)) %>%
  mutate(salary = as.numeric(gsub(",", "",substr(salary, 2, nchar(salary)-1))))

tail(salaries)

stats <- stats %>%
  janitor::clean_names() %>%
  select(player_name, ba:takes)
head(stats)

pitchers <- inner_join(salaries, stats, by = c("player" = "player_name")) %>%
  filter(position %in% c("RHP","LHP","RHP/OF")) %>%
  filter(!(player == "Ohtani, Shohei" & ba == 0.257))

batters <- inner_join(salaries, stats, by = c("player" = "player_name")) %>%
  filter(!position %in% c("RHP","LHP")) %>%
  filter(!(player == "Ohtani,  Shohei" & ba == 0.207)) %>%
  select(-c(spin_rate, velocity, effective_speed))

tail(batters)


#' 
#' Implementing K-means on sample data set
#' 

#install.packages("factoextra")
library(factoextra)
iris

iris_labels <- iris$Species
iris_data <- iris[1:4]

# Scale data
iris_data_scale <- scale(iris_data)
iris_data_scale

# Euclidean Distance calculation
iris_data <- dist(iris_data_scale)

# Calculate how many clusters needed
# wss = Within Sum Squares
fviz_nbclust(iris_data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# K-means
?kmeans
km_output <- kmeans(iris_data_scale, centers = 3, nstart = 100)
km_output

# Visualize the clustering algorithm results
km_clusters <- km_output$cluster
rownames(iris_data_scale) <- paste(iris$Species, 1:dim(iris)[1], sep = "_")
iris_data_scale
fviz_cluster(list(data = iris_data_scale, cluster = km_clusters))
table(km_clusters, iris$Species)



#' 
#' K-Means baseball data
#' 
library(factoextra)

head(batters)

labs <- batters$salary
dat <- batters[,c(5:18)] %>%
  mutate(SwStrPct = whiffs/(swings+takes)*100) %>%
  select(-whiffs, -swings, -takes, -iso)
head(dat)
dat_scaled <- scale(dat)
head(dat_scaled)

# Calculate how many clusters needed
# wss = Within Sum Squares
fviz_nbclust(dat_scaled, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
# other methods are gap_stat and silhouette
# should go 2 or 3 clusters

km_output <- kmeans(dat_scaled, centers = 3, nstart = 100)
km_output #cluster 2 is the group with stronger performance

# Visualize the clustering algorithm results
km_clusters <- km_output$cluster
rownames(dat_scaled) <- paste(labs, 1:dim(batters)[1], sep = "_")

fviz_cluster(list(data = dat_scaled, cluster = km_clusters), repel = T)
#DIM1 = first 'principal component' and DIM2 the second
#numbers in parentheses are the amounts of variability defined by each component

km_output$centers # cluster 2 is better players

batters$cluster <- km_clusters
ggplot(batters, aes(factor(cluster), salary, fill = factor(cluster))) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  xlab("Cluster") +
  ylab("Salary")

batters %>%
  mutate(SwStrPct = whiffs/(swings+takes)*100) %>%
  select(-whiffs, -swings, -takes) %>%
  filter(cluster == 2 & salary > 1000000 & salary <5000000)
#These are players worth targeting
##players lower than 1000000 salary are all so because of minimal time in the
##league but may still be worth looking at in case a guy slips through like maybe Bichette
batters %>%
  mutate(SwStrPct = whiffs/(swings+takes)*100) %>%
  select(-whiffs, -swings, -takes) %>%
  filter(cluster %in% c(1,3) & salary > 7500000) %>%
  arrange(salary)
#these players are an initial list of potentially overvalued players
#there may be outliers in here worth understanding more about


#' 
#' Hierarchical clustering
#' 

library(cluster)

dat <- batters[,c(5:18)] %>%
  mutate(SwStrPct = whiffs/(swings+takes)*100) %>%
  select(-whiffs, -swings, -takes, -iso)
dat_scaled <- scale(dat)

#define linkage methods
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

#calculate agglomerative coefficient for each clustering linkage method
ac <- function(x) {
  agnes(dat_scaled, method = x)$ac
}
sapply(m, ac)
#so Ward is the best method here

#determine number of clusters
gap_stat <- clusGap(dat_scaled, FUN = hcut, K.max = 10)
fviz_gap_stat(gap_stat)
#going with 4 clusters
#this also supports the thumb rule in below dendrogram

# Finding distance matrix
dist_mat <- dist(dat_scaled)

# Fitting Hierarchical clustering Model to training dataset
hierarchical <- hclust(dist_mat, method = "ward.D2")

# Plotting dendrogram
plot(hierarchical, cex = 0.6)

#splitting tree into 4 clusters
fit <- cutree(hierarchical, k = 4)
table(fit)
rect.hclust(hierarchical, k = 4, border = "green")

#plotting box plot
batters$cluster <- fit
ggplot(batters, aes(factor(cluster), salary, fill = factor(cluster))) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.6, alpha=0.9) +
  xlab("Cluster") +
  ylab("Salary")

#mean stats by cluster
aggregate(batters[,4:18], by=list(cluster = batters$cluster), mean)

#final analysis
batters %>%
  mutate(SwStrPct = whiffs/(swings+takes)*100) %>%
  select(-whiffs, -swings, -takes) %>%
  filter((cluster == 1 & salary < 7500000) |
           (cluster == 2 & salary < 3000000))
#These are players worth targeting but I must go through list to determine which
#players are actually on signed contracts and not arb deals

#Cron, Winker, Robert assuming he's going to increase value in his prime, Lowrie
#examples to avoid: Pujols (retired), arbitration players, players who already signed deals with massive salary increases(Tatis), etc.



#' 
#' SVM classification methods
#' 

library(MASS)
library(e1071)
library(tidyverse)
library(caret)

head(batters)
summary(batters)

#groups divided into 0-1mil, 1-5mil, 5-15mil, 15-25mil, 25mil and up
groups = c(0,1000000,5000000,15000000,25000000,100000000)

dat <- batters[,c(4:18)] %>%
  mutate(SwStrPct = whiffs/(swings+takes)*100) %>%
  dplyr::select(-whiffs, -swings, -takes, -iso) %>%
  mutate(group = cut(salary, breaks = groups, labels = c(1:5))) %>%
  dplyr::select(-salary)
head(dat)


# preds <- batters[,c(5:18)] %>%
#   mutate(SwStrPct = whiffs/(swings+takes)*100) %>%
#   dplyr::select(-whiffs, -swings, -takes, -iso) %>%
#   scale()
# y <- batters %>%
#   mutate(group = cut(salary, breaks = groups, labels = c(1:5))) %>%
#   dplyr::select(salary, group)

#Testing for normality
shapiro.test(dat$ba)

#ba, babip, woba, xba, launch_speed, launch_angle, SwStrPct all have 
#a p-value above 0.05 meaning the data are not significantly different
#from a normal distribution. Thus we can assume normality with those
#variables, which is ideal with SVM

# norm_preds <- preds[,c(1,2,4,6,9:11)]
# norm_dat <- dat[,c(1,2,4,6,9:12)]

ggplot(dat, aes(ba, SwStrPct, col = group, size = batters$salary)) +
  geom_point() +
  theme(legend.position = "none")

#LDA
lda <- lda(group ~ ., data = dat, type = "C-Classification", cost = 10)
pred_lda <- predict(lda, dat)
cmatrix_lda <- table(pred_lda$class, dat$group)
cmatrix_lda
confusionMatrix(cmatrix_lda)
# 48.74% accuracy
# 39.71% when only using normalized data (4 less predictors)
# excluded slg, xwoba, hits, abs in normal data

#QDA
qda <- qda(group ~ ., data = dat, type = "C-Classification", cost = 10)
pred_qda <- predict(qda, dat)
cmatrix_qda <- table(pred_qda$class, dat$group)
confusionMatrix(cmatrix_qda)
#64.26% accuracy
#48.74% accuracy only normalized features

#SVM
svm <- svm(group ~ ., data = dat, type = "C-classification", cost = 10)
summary(svm)
plot(svm, data = dat, xwoba ~ ba)
pred_svm <- predict(svm, dat)
cmatrix_svm <- table(pred_svm, dat$group)
confusionMatrix(cmatrix_svm)
#82.31% accuracy
#75.45% accuracy with only normalized

#bar graph of accuracy values

normal_preds <- c(.3971, .4874, .7545)
all_preds <- c(.4874, .6426, .8231)
labels <- c("LDA", "QDA", "SVM")
dd <- data.frame(labels, normal_preds, all_preds) %>%
  pivot_longer(cols = -labels) %>%
  rename(Data = name)
ggplot(dd, aes(x = labels, y = value, fill = Data)) +
  geom_col(position = "dodge") +
  labs(x = "Vector Machine Method", y = "Accuracy",
       title = "Accuracy Scores of Vector Machine Methods")

#scaling the data didn't change the accuracy results at all (auto-scaled)
#only normalized predictors lowered accuracy some but was still decent
#I would rather use more complicated model unless I had some reason to think
#it overfits the data
#SVM is clearly the best as expected, and what I would choose to use

# svm_iris <- svm(Species ~ ., dat = iris, type = "C-classification", cost = 10)
# plot(svm_iris, data = iris, Sepal.Length ~ Sepal.Width)
# fpred <- predict(svm_iris, iris)
# cmatrix <- table(pred, iris$Species)
# confusionMatrix(cmatrix)


#' 
#' The non-100 percent accuracy could be because some players skills don't reflect their salary range
#' which is ideal in a situation in which we are trying to find players that don't fit their group 
#' for the better
#' 
#' In the confusion mat we are looking for the people in the bottom left corner
#' 

batters %>%
  mutate(group = cut(salary, breaks = groups, labels = c(1:5)),
         predicted = pred_svm) %>%
  filter(group == 1 & predicted == 3)
#Mountcastle best value - potential trade target
#Next groupings we see Lowrie again, as well as Toro, Will Smith, Grisham, Murphy,
#Solak, Vaughn, Hays, Adames, Reynolds, and Verdugo


#This may also be used to classify future players into a salary range based on performance


#Need to use regression to predict actual salary values


