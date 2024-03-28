

## C01SIV 빼고 lsirm fitting

elem_lsirm_fit1107 = lsirm1pl(data = elem_lsirm_data3)
setwd("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit")
save(elem_lsirm_fit1107, file = "elem_lsirm_fit1107.RData")
ngroup = 5

spec_clust(elem_lsirm_fit1107, k=ngroup)


# Work 1 : 초등데이터 5개의 cluster 별로 lsirm 돌려서 gamma가 0 나오는지 확인

## cluster별 lsirm fitting할 데이터 만들기 (ngroup = 5)
elem_clust_var = list()
elem_clust_var[[1]] = colnames(elem_lsirm_data3)[c(51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71)]
elem_clust_var[[2]] = colnames(elem_lsirm_data3)[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)]
elem_clust_var[[3]] = colnames(elem_lsirm_data3)[c(38, 39, 40, 41, 42, 43, 46, 47, 48, 49)]
elem_clust_var[[4]] = colnames(elem_lsirm_data3)[c(23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37)]
elem_clust_var[[5]] = colnames(elem_lsirm_data3)[c(44, 45, 50, 72, 73, 74, 75)]


elem_clust_data = list()
for(i in 1:ngroup){
  elem_clust_data[[i]] = subset(elem_lsirm_data3, select=elem_clust_var[[i]])
}



## lsirm fitting
elem_clust_lsirm_fit = list()
for(i in 1:ngroup){
  elem_clust_lsirm_fit[[i]] = lsirm1pl(data = elem_clust_data[[i]], spikenslab = T)
}
save(elem_clust_lsirm_fit, file = "/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit/elem_clust_lsirm_fit.RData")
plot(elem_clust_lsirm_fit[[5]])




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
