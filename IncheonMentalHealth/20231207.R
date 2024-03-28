
ngroup = 5
elem_data4 = read.csv("/Users/seoyoung/Desktop/Team5/elem_data1026.csv")
head(elem_data4)
elem_lsirm_data3 = as.matrix(subset(elem_data4, select = -c(C01SID, C01CE1, C01CE2, C01SIV01, C01SIV02, C01SIV03)))

##------------------------------------------------------------------------------------------------##
## 11/9
# Task 1 : 문항 그룹별 regression
# Task 2 : 줄이기 전 문항 / 줄인 문항으로 Rasch model 돌리고 theta값 확인


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









elem_reduced_clust_data = list()
for(i in 1:ngroup){
  elem_reduced_clust_data[[i]] = subset(elem_lsirm_data3, select = elem_reduced_clust_var[[i]])
}



load("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/Rasch/elem_reduced_rasch_fit.RData")

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





# 유형화하기
group_type = as.data.frame(matrix(NA, nrow = nrow(elem_data4), ncol = ngroup))
colnames(group_type) = c("group1", "group2", "group3", "group4", "group5")
group_type$group1 = ifelse(elem_reduced_rasch_theta_scaled[[1]]<0.5, 0, 1)
group_type$group2 = ifelse(elem_reduced_rasch_theta_scaled[[2]]<0.5, 0, 1)
group_type$group3 = ifelse(elem_reduced_rasch_theta_scaled[[3]]<0.5, 0, 1)
group_type$group4 = ifelse(elem_reduced_rasch_theta_scaled[[4]]<0.5, 0, 1)
group_type$group5 = ifelse(elem_reduced_rasch_theta_scaled[[5]]<0.5, 0, 1)
head(group_type)


## group별 count
group_type
library("dplyr")
group_type_count = group_type %>% group_by(group1, group2, group3, group4, group5) %>% summarize(count=n())
group_type_count = as.data.frame(group_type_count)


## 각 group에서 예시 뽑기
## count 가장 많은거 (1,1,1,1,1) # 437
group_type[(group_type$group1==1 & group_type$group2==1 & group_type$group3==1 & group_type$group4==1 & group_type$group5==1),] # 19, 26, 35, 42, 45 번째 응답자
elem_lsirm_data3[c(19, 26, 35, 42, 45), ]

## count 가장 적은거 (1,0,0,0,1) # 16
group_type[(group_type$group1==1 & group_type$group2==0 & group_type$group3==0 & group_type$group4==0 & group_type$group5==1),] # 189, 211번째 응답자
elem_lsirm_data3[c(189, 211, 332, 670, 679), ]

cbind(elem_reduced_rasch_theta_scaled[[1]][c(19, 26, 35, 42, 45)],
elem_reduced_rasch_theta_scaled[[2]][c(19, 26, 35, 42, 45)],
elem_reduced_rasch_theta_scaled[[3]][c(19, 26, 35, 42, 45)],
elem_reduced_rasch_theta_scaled[[4]][c(19, 26, 35, 42, 45)],
elem_reduced_rasch_theta_scaled[[5]][c(19, 26, 35, 42, 45)])


cbind(elem_reduced_rasch_theta_scaled[[1]][c(189, 211, 332, 670, 679)],
elem_reduced_rasch_theta_scaled[[2]][c(189, 211, 332, 670, 679)],
elem_reduced_rasch_theta_scaled[[3]][c(189, 211, 332, 670, 679)],
elem_reduced_rasch_theta_scaled[[4]][c(189, 211, 332, 670, 679)],
elem_reduced_rasch_theta_scaled[[5]][c(189, 211, 332, 670, 679)])


rasch_df = as.data.frame(cbind(elem_reduced_rasch_theta_scaled[[1]],
      elem_reduced_rasch_theta_scaled[[2]],
      elem_reduced_rasch_theta_scaled[[3]],
      elem_reduced_rasch_theta_scaled[[4]],
      elem_reduced_rasch_theta_scaled[[5]]))

head(rasch_df)

# case 1
rasch_df[rasch_df$V1 > 0.67 & rasch_df$V2 > 0.67 & rasch_df$V3 > 0.67 & rasch_df$V4 > 0.67 & rasch_df$V5 > 0.67,] 
rasch_df[c(238, 441, 680, 1541, 1561, 1949),]
elem_data4[c(238, 441, 680),c("C01EM01",	"C01EM02",	"C01EM03",	"C01EM04",	"C01EM05",	"C01EM06",	"C01EM07",	"C01EM08",	"C01EM09",	"C01SS01",	"C01SS02",	"C01SS03",	"C01SS04",	"C01SS05",	"C01SS06",	"C01SS07",	"C01SS08",	"C01SS09",	"C01SS10",	"C01SS11",	"C01SS12",	
                              "C01DGT02c",	"C01SH01",	"C01SH02",	"C01SH03",	"C01SH04",	"C01SH05",	"C01SH06",	"C01SH07",	"C01SH08",	"C01SH09",	"C01SH10",	"C01DH01",	"C01DH02",	"C01DH03",	"C01DH04",	"C01DH05",	"C01DH06",	"C01DH07",	"C01DH08",	"C01DH09",	"C01DH10",	"C01DH11",	"C01DH12",	
                              "C01FD02",	"C01FD03",	"C01FD04",	"C01FD05",	"C01FD06",	"C01ST01",	"C01ST04",	"C01ST05",	"C01ST06",	"C01ST07",
                              "C01SAD01",	"C01SAD02",	"C01SAD03",	"C01SAD04",	"C01SAD05",	"C01SAD06",	"C01SAD07",	"C01SAD08",	"C01SAD09",	"C01SAD10",	"C01SAD11",	"C01HP01",	"C01HP02",	"C01HP03",	"C01FD01",	
                              "C01ST02",	"C01ST03",	"C01ST08",	"C01SPD01",	"C01SPD02",	"C01SPD03",	"C01SPD04")]

# case 2
rasch_df[rasch_df$V1 > 0.6 & rasch_df$V2 < 0.5 & rasch_df$V3 < 0.5 & rasch_df$V4 < 0.5 & rasch_df$V5 > 0.6,] 
elem_data4[c(670, 771, 1419), c("C01EM01",	"C01EM02",	"C01EM03",	"C01EM04",	"C01EM05",	"C01EM06",	"C01EM07",	"C01EM08",	"C01EM09",	"C01SS01",	"C01SS02",	"C01SS03",	"C01SS04",	"C01SS05",	"C01SS06",	"C01SS07",	"C01SS08",	"C01SS09",	"C01SS10",	"C01SS11",	"C01SS12",	
"C01DGT02c",	"C01SH01",	"C01SH02",	"C01SH03",	"C01SH04",	"C01SH05",	"C01SH06",	"C01SH07",	"C01SH08",	"C01SH09",	"C01SH10",	"C01DH01",	"C01DH02",	"C01DH03",	"C01DH04",	"C01DH05",	"C01DH06",	"C01DH07",	"C01DH08",	"C01DH09",	"C01DH10",	"C01DH11",	"C01DH12",	
"C01FD02",	"C01FD03",	"C01FD04",	"C01FD05",	"C01FD06",	"C01ST01",	"C01ST04",	"C01ST05",	"C01ST06",	"C01ST07",
"C01SAD01",	"C01SAD02",	"C01SAD03",	"C01SAD04",	"C01SAD05",	"C01SAD06",	"C01SAD07",	"C01SAD08",	"C01SAD09",	"C01SAD10",	"C01SAD11",	"C01HP01",	"C01HP02",	"C01HP03",	"C01FD01",	
"C01ST02",	"C01ST03",	"C01ST08",	"C01SPD01",	"C01SPD02",	"C01SPD03",	"C01SPD04")]



## theta histogram
par(mfrow=c(1,2))
for(i in 1:ngroup){
  hist(elem_reduced_rasch_theta_scaled[[i]], breaks=100, main = paste0("Respondent score for Group", i), xlab = "scale", ylab="")
  # print(summary(elem_reduced_rasch_theta[[i]]))
}
for(i in 1:ngroup){
  print(summary(elem_reduced_rasch_theta_scaled[[i]]))
}