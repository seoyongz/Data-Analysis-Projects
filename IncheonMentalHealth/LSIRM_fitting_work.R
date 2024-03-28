library(lsirm12pl)
# library(coda) # CI
library(Rcpp)
library(RcppArmadillo)


## Data load
##########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
elem_data3 = read.csv("elem_data3.csv")
elem_lsirm_data1 = as.matrix(subset(elem_data4, select = -c(C01SID)))
elem_lsirm_data2 = as.matrix(subset(elem_data4, select = -c(C01SID, C01CE1, C01CE2, C01DH06)))
elem_lsirm_data3 = as.matrix(subset(elem_data4, select = -c(C01SID, C01CE1, C01CE2, C01DH06, C01SIV01, C01SIV02, C01SIV03)))


# Work1 : Variable Select
##########################################################################
covariate_var = c("C01SEX","C01STDTc", "C01PTTc", "C01BKTc", "C01ACVTc", "P01FINCM", "P01FJOB", "P01MJOB")

## 포함할 카테코리에 해당하는 변수이름 저장
C01SH = grep("^C01SH[0-9]+$", colnames(elem_data3), value = T)
C01DH = grep("^C01DH[0-9]+$", colnames(elem_data3), value = T)
C01SIV = grep("^C01SIV[0-9]+$", colnames(elem_data3), value = T)
C01SAD = grep("^C01SAD[0-9]+$", colnames(elem_data3), value = T)
C01HP = grep("^C01HP[0-9]+$", colnames(elem_data3), value = T)
C01FD = grep("^C01FD[0-9]+$", colnames(elem_data3), value = T)
C01ST = grep("^C01ST[0-9]+$", colnames(elem_data3), value = T)
C01EM = grep("^C01EM[0-9]+$", colnames(elem_data3), value = T)
C01SS = grep("^C01SS[0-9]+$", colnames(elem_data3), value = T)
C01SPD = grep("^C01SPD[0-9]+$", colnames(elem_data3), value = T)
item_group = c(C01SH, C01DH, C01SIV, C01SAD, C01HP, C01FD, C01ST, C01EM, C01SS, C01SPD)
item_group = c(C01SH, C01DH, C01SAD, C01HP, C01FD, C01ST, C01EM, C01SS, C01SPD)

## 모형 fitting에 포함할 변수들 정의
item_var = c("C01SID", "C01DGT02c", "C01CE1", "C01CE2", item_group)
elem_data4 = subset(elem_data3, select = item_var)
colnames(elem_data4)
head(elem_data4)

colSums(is.na(elem_data4))
elem_data4 = na.omit(elem_data4)
dim(elem_data4)

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
write.csv(elem_data4, "elem_data4.csv", row.names=F)


## Work 2-1 : LSIRM fitting
##########################################################################
elem_lsirm_fit1 = lsirm1pl(data = elem_lsirm_data1)
elem_lsirm_fit2 = lsirm1pl(data = elem_lsirm_data2)
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
save(elem_lsirm_fit1, file = "elem_lsirm_fit1_1027.RData")
save(elem_lsirm_fit2, file = "elem_lsirm_fit2_1027.RData")



## Work 2-2 : Model Convergence Check
##########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
load("elem_lsirm_fit1_1027.RData")
load("elem_lsirm_fit2_1027.RData")

par(mfrow=c(2,2))
ts.plot(elem_lsirm_fit1$beta[, 1])
ts.plot(elem_lsirm_fit1$theta[ 3])
ts.plot(elem_lsirm_fit1$z[, 2, 2])
ts.plot(elem_lsirm_fit1$w[, 1, 2])

ts.plot(elem_lsirm_fit2$beta[, 1])
ts.plot(elem_lsirm_fit2$theta[, 3])
ts.plot(elem_lsirm_fit2$z[, 2, 2])
ts.plot(elem_lsirm_fit2$w[, 1, 2])




## Work 2-3 : Latent positions Check
###########################################################################
plot(elem_lsirm_fit1)
plot(elem_lsirm_fit2)


# Plot only item positions (not respondents')
elem_lsirm_fit1_item = data.frame()
elem_lsirm_fit1_item$w1 = elem_lsirm_fit1$w_estimate[, 1]
elem_lsirm_fit1_item$w2 = elem_lsirm_fit1$w_estimate[, 2]
elem_lsirm_fit1_item$item = colnames(elem_lsirm_data)

ggplot(data = elem_lsirm_fit1_item) +
  geom_point(aes(x = w1, y = w2), color="red") +
  geom_text_repel(aes(x = w1, y = w2, label = item), size = 3)


elem_lsirm_fit2_item = data.frame()
elem_lsirm_fit2_item$w1 = elem_lsirm_fit2$w_estimate[, 1]
elem_lsirm_fit2_item$w2 = elem_lsirm_fit2$w_estimate[, 2]
elem_lsirm_fit2_item$item = colnames(elem_lsirm_data)

ggplot(data = elem_lsirm_fit2_item) +
  geom_point(aes(x = w1, y = w2), color="red") +
  geom_text_repel(aes(x = w1, y = w2, label = item), size = 3)









## Work 3-1 : Change the data structure to list
#######################################################################
elem_list_data = list()
count = 1
for(name in unique(elem_data4$C01SID)){
  elem_tmp = elem_data4[elem_data4$C01SID == name,]
  elem_tmp = as.matrix(subset(elem_tmp, select = -c(C01SID)))
  elem_list_data[[count]] = elem_tmp
  count = count + 1
}
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
save(elem_list_data, file = "elem_list_data1031.RData")




## Work 3-2 : LSIRM fitting for each school
########################################################################
elem_lsirm_fit_school = list()
count = 1
for(data in elem_list_data){
  elem_lsirm_fit_school[[count]] = lsirm1pl(data)
  count = count + 1
}
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
save(elem_lsirm_fit_school, file = ("elem_lsirm_fit_school.RData"))



## Work 3-3 : Latent positions Check
########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
load("elem_lsirm_fit_school.RData")

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/plots")
count = 1
for(data in elem_lsirm_fit_school){

  df_w = data.frame(w1 = data$w_estimate[, 1], w2 = data$w_estimate[, 2])

  ggplot(df_w) + 
    geom_point(mapping = aes(w1, w2), color = "red") +
    geom_text(aes(x = w1, y = w2, label = 1:nrow(df_w)))
    ggtitle(paste0("Item Latent Positions in school", count))

  ggsave(paste0("school", count, ".jpg"), width = 20, height = 15, units = c("cm"))
  count = count + 1
}






## C01SIV 빼고 lsirm fitting

elem_lsirm_fit1107 = lsirm1pl(data = elem_lsirm_data3)
setwd("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit")
save(elem_lsirm_fit1107, file = "elem_lsirm_fit1107.RData")
ngroup = 5

spec_clust(elem_lsirm_fit1107, k=ngroup)






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













