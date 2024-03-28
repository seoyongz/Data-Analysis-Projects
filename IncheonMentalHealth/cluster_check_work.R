
##########################################################################
## In this file, 
## Work1 : Clustering items with latent positions
## Work2 : Determine the number of clusters using following methods:
#### Method 1. Silhouette Score
#### Method 2. Elbow method
## Work3 : Check the cluster validity using following method:
#### Method 1. using gamma parameter in LSIRM
##########################################################################
library(cluster) ## Silhouette Score
library(stats) ## Elbow method


setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
elem_data5 = read.csv("elem_data5.csv")
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
load("elem_lsirm_fit1.RData")

## Work 1 : Clustering with spec_clust function ngroup = 1:13
###########################################################################
setwd("/Users/seoyoung/Desktop/my_git/DA_Projects/IncheonMentalHealth")
source("spec_clust.R")

nitem = ncol(elem_data5[,-1])
ngroup = 13

# Save the cluster group for each clustering(1:13)
item_clust_df = as.data.frame(matrix(NA, nrow=nitem, ncol=ngroup)) 
colnames(item_clust_df) = paste0("G", 1:13)
elem_clust = list()
sse = rep(NA, ngroup)

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/plots")
for(n in 1:ngroup){
  
  # the number of cluster is n
  clust_tmp = spec_clust(elem_lsirm_fit1, k=n)
  
  # save the clustering result in list
  elem_clust[[n]] = clust_tmp
  
  # plot the clustering result and save the plot
  clust_tmp$p
  ggsave(paste0("cluster", n, ".jpg"), width=20, height=20, units=c("cm"))
  
  # save the clustering result in data.frame
  for(i in 1:n){
    
    # item index which belongs to i-th cluster
    clust_i = (elem_clust$clust[i, ])$items 
    clust_i = as.numeric(strsplit(clust_i, ", ")[[1]])
    
    item_clust_df[clust_i, n] = i
  }
  
  # For Elbow method
  sse[n] = clust_tmp$tot.withinss
}


## Work 2 : Determine the number of clusters
###########################################################################

# Method 1 : Silhouette score
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
load("elem_lsirm_fit1.RData")

w_est = elem_lsirm_fit1$w_estimate
dist_matrix = proxy::simil(w_est, w_est, method="Euclidean")

silhouette_score = rep(NA, ngroup)
for(n in 1:ngroup){
  silhouette_result = silhouette(item_clust_df[, n], dist_matrix)
  silhouette_score[n] = mean(silhouette_result[, "sil_width"])
}


# Method 2 : Elbow method

elbow_point = elbowMethod(sse)

# 그래프로 SSE 시각화
plot(x=1:ngroup, y=sse, type="b", pch=19, frame=FALSE, xlab="Number of clusters", ylab="SSE", main="Elbow Method for Spectral Clustering")
abline(v=elbow_point, col="red", lty=2)



## Work3 : Check the cluster validity using gamma parameter
# Whether gamma=0 or not

## Fit LSIRM within cluster G5
item_names = colnames(elem_data5)[, -1]
elem_clust_data = list()
elem_clust_lsirm_fit = list()
for(k in ngroup){
  elem_clustered = elem_data5[, item_clust_df$G5 == k]
  elem_clust_lsirm_fit[[k]] = lsirm1pl(elem_clustered, spikenslab = T)
}
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
save(elem_clust_lsirm_fit, "elem_clust_lsirm_fit.RData")



## result : gamma check with confidence interval
HPDinterval(as.mcmc(elem_clust_lsirm_fit[[1]]$gamma))
HPDinterval(as.mcmc(elem_clust_lsirm_fit[[2]]$gamma))
HPDinterval(as.mcmc(elem_clust_lsirm_fit[[3]]$gamma))
HPDinterval(as.mcmc(elem_clust_lsirm_fit[[4]]$gamma))
HPDinterval(as.mcmc(elem_clust_lsirm_fit[[5]]$gamma))






# Task 2 : 초등 각각 5개 그룹에서 Rasch 돌리기
# load("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit/elem_clust_lsirm_fit.RData")
# 
# ## Rasch fitting
# ngroup = 5
# elem_rasch_fit = list()
# for(i in 1:ngroup){
#   elem_rasch_fit[[i]] = rasch(data = elem_clust_data[[i]])
# }
# save(elem_rasch_fit, file="/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/Rasch/elem_rasch_fit.RData")
# 
# factor.scores(elem_clust1_rasch_fit)
#   
# ## result save
# elem_rasch_theta = list()
# for(i in 1:ngroup){
#   elem_rasch_theta[[i]] =  factor.scores(elem_rasch_fit[[i]])[[1]]
# }
# save(elem_rasch_theta, file="/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/Rasch/elem_rasch_theta.RData")
# 
# load("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/Rasch/elem_rasch_theta.RData")
# par(mfrow=c(1,5))
# for(i in 1:ngroup){
#   hist(rep(elem_rasch_theta[[i]]$"z1", elem_rasch_theta[[i]]$"Obs"), breaks=20)
# }



# Task 3 : 초등 각 그룹 안에서 regression 돌려서 insight 얻는 예시
## 학교 생활과 행복도와의 관계
SAD_school = rowSums(elem_lsirm_data3[,c("C01SAD01", "C01SAD02", "C01SAD03")])
SAD_friend = rowSums(elem_lsirm_data3[,c("C01SAD04", "C01SAD05", "C01SAD06", "C01SAD07")])
SAD_teacher = rowSums(elem_lsirm_data3[,c("C01SAD08", "C01SAD09", "C01SAD10", "C01SAD11")])

glm_fit_HP1 = glm(elem_lsirm_data3[,"C01HP01"] ~ SAD_school + SAD_friend + SAD_teacher, family="binomial")
glm_fit_HP2 = glm(elem_lsirm_data3[,"C01HP02"] ~ SAD_school + SAD_friend + SAD_teacher, family="binomial")
glm_fit_HP3 = glm(elem_lsirm_data3[,"C01HP03"] ~ SAD_school + SAD_friend + SAD_teacher, family="binomial")
summary(glm_fit_HP1)
summary(glm_fit_HP2)
summary(glm_fit_HP3)


glm_fit_ST1 = glm(elem_lsirm_data3[,"C01ST01"] ~ SAD_school + SAD_friend + SAD_teacher, family="binomial")
glm_fit_ST3 = glm(elem_lsirm_data3[,"C01ST03"] ~ SAD_school + SAD_friend + SAD_teacher, family="binomial")
glm_fit_ST5 = glm(elem_lsirm_data3[,"C01ST05"] ~ SAD_school + SAD_friend + SAD_teacher, family="binomial")
summary(glm_fit_ST1)
summary(glm_fit_ST3)
summary(glm_fit_ST5)





##------------------------------------------------------------------------------------------------##
# Task 1 : cluster 별 regression

## group 1)
C01EM = grep("^C01EM[0-9]+$", colnames(elem_lsirm_data3), value = T)
C01SS = grep("^C01SS[0-9]+$", colnames(elem_lsirm_data3), value = T)
EM_score = rowSums(elem_lsirm_data3[,C01EM])
SS_score = rowSums(elem_lsirm_data3[,C01SS])

plot(jitter(EM_score), jitter(SS_score))
lm_SS_EM = lm(SS_score ~ EM_score)
summary(lm_SS_EM)
cor(SS_score, EM_score) # 0.6488856



## group 2) 
C01SH = grep("^C01SH[0-9]+$", colnames(elem_lsirm_data3), value = T)
C01DH = grep("^C01DH[0-9]+$", colnames(elem_lsirm_data3), value = T)
SH_score = rowSums(elem_lsirm_data3[,C01SH])
DH_score = rowSums(elem_lsirm_data3[,C01DH])

plot(jitter(DH_score), jitter(SH_score))
lm_SH_DH = lm(SH_score ~ DH_score)
summary(lm_SH_DH)
cor(SH_score, DH_score) # 0.5882901



## group 4)
C01SAD = grep("^C01SAD[0-9]+$", colnames(elem_lsirm_data3), value = T)
C01HP = grep("^C01HP[0-9]+$", colnames(elem_lsirm_data3), value = T)
SAD_score = rowSums(elem_lsirm_data3[,C01SAD])
HP_score = rowSums(elem_lsirm_data3[,c(C01HP, "C01FD01")])

plot(jitter(SAD_score), jitter(HP_score))
lm_HP_SAD = lm(HP_score ~ SAD_score)
summary(lm_HP_SAD)
cor(HP_score, SAD_score) # 0.5426793


## group 5)
C01SPD = grep("^C01SPD[0-9]+$", colnames(elem_lsirm_data3), value = T)
SPD_score = rowSums(elem_lsirm_data3[,C01SPD])

glm_SPD_ST2= glm(elem_lsirm_data3[,"C01ST02"] ~ SPD_score, family="binomial")
glm_SPD_ST3= glm(elem_lsirm_data3[,"C01ST03"] ~ SPD_score, family="binomial")

summary(glm_SPD_ST2)
summary(glm_SPD_ST3)
