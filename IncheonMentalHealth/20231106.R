library(ltm) # Rasch
library(lsirm12pl)
library(coda) # CI
library(Rcpp)
library(RcppArmadillo)


## 1109 아가과 미팅준비
elem_data4 = read.csv("/Users/seoyoung/Desktop/Team5/elem_data1026.csv")
elem_lsirm_data = as.matrix(subset(elem_data4, select = -c(C01SID)))
elem_lsirm_data2 = as.matrix(subset(elem_data4, select = -c(C01SID, C01CE1, C01CE2, C01DH06)))
elem_lsirm_data3 = as.matrix(subset(elem_data4, select = -c(C01SID, C01CE1, C01CE2, C01DH06, C01SIV01, C01SIV02, C01SIV03)))




## C01SIV 빼고 lsirm fitting

elem_lsirm_fit1107 = lsirm1pl(data = elem_lsirm_data3)
setwd("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit")
save(elem_lsirm_fit1107, file = "elem_lsirm_fit1107.RData")
ngroup = 5

spec_clust(elem_lsirm_fit1107, k=ngroup)


# Task 1 : 초등데이터 5개의 cluster 별로 lsirm 돌려서 gamma가 0 나오는지 확인

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
## 11/9
# Task 1 : cluster 별 regression
# Task 2 : 줄인 문항으로 Rasch model 돌리고 theta값 확인



### Task 1
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





### Task 2

## Rasch fitting -  문항 축소전
sourceCpp("/Users/seoyoung/Desktop/Team5/Incheon_project/rasch/hw4.cpp")

nsamp = 10000
nburn = 3000
nthin = 5
pr_beta_mu = 0
pr_beta_sd = 1
pr_theta_mu = 0
pr_theta_sd = 1
jump_beta = 0.2
jump_theta = 0.04
a_pri=0.001
b_pri=0.001

elem_rasch_fit = list()
ngroup = 5
for(i in 1:ngroup){
  elem_rasch_fit[[i]] = rasch_cpp(data = elem_clust_data[[i]], nsamp = nsamp, nburn = nburn, nthin=nthin,
                                  pr_beta_mu = pr_beta_mu, pr_beta_sd = pr_beta_sd, 
                                  pr_theta_mu = pr_theta_mu, pr_theta_sd = pr_theta_sd,
                                  jump_beta = jump_beta, jump_theta = jump_theta,
                                  a_pri = a_pri, b_pri = b_pri)
}
save(elem_rasch_fit, file="/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/Rasch/elem_rasch_fit.RData")

elem_rasch_fit[[1]]$accept_b
elem_rasch_fit[[2]]$accept_b
elem_rasch_fit[[3]]$accept_b
elem_rasch_fit[[4]]$accept_b
elem_rasch_fit[[5]]$accept_b

elem_rasch_fit[[1]]$accept_t
elem_rasch_fit[[2]]$accept_t
elem_rasch_fit[[3]]$accept_t
elem_rasch_fit[[4]]$accept_t
elem_rasch_fit[[5]]$accept_t

## traceplot
par(mfrow=c(2,5))
for(i in 1:ngroup){
  ts.plot(elem_rasch_fit[[i]]$beta[,1])
}
for(i in 1:ngroup){
  ts.plot(elem_rasch_fit[[i]]$theta[,1])
}

## theta values
load("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/Rasch/elem_rasch_fit.RData")
elem_rasch_theta = list()
for(i in 1:ngroup){
  elem_rasch_theta[[i]] = elem_rasch_fit[[i]]$theta
}

## theta histogram
par(mfrow=c(2,3))
for(i in 1:ngroup){
  hist(elem_rasch_theta[[i]], breaks=40, main = paste0("Respondent score for Group", i), xlab = "score values")
  print(summary(elem_rasch_theta[[i]]))
}

summary(elem_rasch_theta[[1]])
elem_rasch_theta[[1]][1:5]
elem_rasch_theta[[2]][1:5]
elem_rasch_theta[[3]][1:5]
str(elem_rasch_theta[[1]])


## 문항 축소
elem_reduced_clust_var = list()
elem_reduced_clust_var[[1]] = c("C01EM01", "C01EM02", "C01EM03", "C01EM08", "C01SS03","C01SS04","C01SS08","C01SS11")
elem_reduced_clust_var[[2]] = c("C01DGT02c", "C01SH02", "C01SH04", "C01SH07", "C01SH10", "C01DH02", "C01DH03", "C01DH07", "C01DH08", "C01DH10", "C01DH11")
elem_reduced_clust_var[[3]] = c("C01FD04", "C01FD05", "C01FD06", "C01ST04", "C01ST06", "C01ST07")
elem_reduced_clust_var[[4]] = c("C01SAD02", "C01SAD04", "C01SAD05", "C01SAD11", "C01HP02", "C01HP03", "C01FD01")
elem_reduced_clust_var[[5]] = c("C01ST02", "C01ST03", "C01SPD02", "C01SPD04")

# elem_reduced_var = c(elem_reduced_group1, elem_reduced_group2, elem_reduced_group3, elem_reduced_group4, elem_reduced_group5)
# elem_reduced_data = subset(elem_lsirm_data3, select=elem_reduced_var)

elem_reduced_clust_data = list()
for(i in 1:ngroup){
  elem_reduced_clust_data[[i]] = subset(elem_lsirm_data3, select = elem_reduced_clust_var[[i]])
}



## Rasch fitting -  문항 축소후


sourceCpp("/Users/seoyoung/Desktop/Team5/Incheon_project/rasch/hw4.cpp")
elem_reduced_rasch_fit = list()

nsamp = 10000
nburn = 3000
nthin = 5
pr_beta_mu = 0
pr_beta_sd = 1
pr_theta_mu = 0
pr_theta_sd = 1
jump_beta = 0.2
jump_theta = 0.04
a_pri=0.001
b_pri=0.001
for(i in 1:ngroup){
  elem_reduced_rasch_fit[[i]] = rasch_cpp(data = elem_reduced_clust_data[[i]], nsamp = nsamp, nburn = nburn, nthin=nthin,
                                          pr_beta_mu = pr_beta_mu, pr_beta_sd = pr_beta_sd, 
                                          pr_theta_mu = pr_theta_mu, pr_theta_sd = pr_theta_sd,
                                          jump_beta = jump_beta, jump_theta = jump_theta,
                                          a_pri = a_pri, b_pri = b_pri)
}
save(elem_reduced_rasch_fit, file="/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/Rasch/elem_reduced_rasch_fit.RData")



elem_reduced_rasch_fit[[1]]$accept_b
elem_reduced_rasch_fit[[2]]$accept_b
elem_reduced_rasch_fit[[3]]$accept_b
elem_reduced_rasch_fit[[4]]$accept_b
elem_reduced_rasch_fit[[5]]$accept_b

elem_reduced_rasch_fit[[1]]$accept_t
elem_reduced_rasch_fit[[2]]$accept_t
elem_reduced_rasch_fit[[3]]$accept_t
elem_reduced_rasch_fit[[4]]$accept_t
elem_reduced_rasch_fit[[5]]$accept_t

## traceplot
par(mfrow=c(2,5))
for(i in 1:ngroup){
  ts.plot(elem_reduced_rasch_fit[[i]]$beta[,1])
}
for(i in 1:ngroup){
  ts.plot(elem_reduced_rasch_fit[[i]]$theta[,1])
}


## theta values
elem_reduced_rasch_theta = list()
for(i in 1:ngroup){
  elem_reduced_rasch_theta[[i]] = colMeans(elem_reduced_rasch_fit[[i]]$theta)
}

# scaling
elem_reduced_rasch_theta_scaled = list()
for(i in 1:ngroup){
  elem_reduced_rasch_theta_scaled[[i]] = scale(elem_reduced_rasch_theta[[i]], center=min(elem_reduced_rasch_theta[[i]]), scale=max(elem_reduced_rasch_theta[[i]]) - min(elem_reduced_rasch_theta[[i]]))
}

## theta histogram
par(mfrow=c(1,2))
for(i in 1:ngroup){
  hist(elem_reduced_rasch_theta_scaled[[i]], breaks=100, main = paste0("Respondent score for Group", i), xlab = "scale", ylab="")
  # print(summary(elem_reduced_rasch_theta[[i]]))
}
for(i in 1:ngroup){
  print(summary(elem_reduced_rasch_theta_scaled[[i]]))
}


