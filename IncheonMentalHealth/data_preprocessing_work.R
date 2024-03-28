### elementary data preprocessed -> saved as "/Users/seoyoung/Desktop/Team5/elem_data1022.csv"
### lsirm fitting -> "/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit/elem_lsirm_fit.RData
### lsirm fitting for each school


library(lsirm12pl)
library(ggplot2)

setwd("/Users/seoyoung/Desktop/Team5/데이터 CSV(1010)")

elem_data = read.csv("(Data3)인천종단_초등데이터수정본(1010).csv", header=T, fileEncoding = "CP949", encoding = "UTF-8", na.strings = c(-999,"", " "))
head(elem_data)
summary(elem_data)


## 필요없는 column 제거
# C01ID : student ID
# C01GR : 값이 1만 있어서 열 제거
# C01SNAME : C01SID 와 중복된 의미이므로 열 제거
# C01QSD : 설문 시작 시각
# C01QED : 설문 시작 시각
# C01Note 
elem_data2 = subset(elem_data, select = -c(C01ID, C01GR, C01SNAME, C01QSD, C01QED, C01NOTE, X.32, X.33))
head(elem_data2)

# C01CPLTE : Complete / not complete
elem_data2$C01CPLTE = ifelse(elem_data2$C01CPLTE == "완료", 1, 0)

# C01CSNT : 
# elem_data2[elem_data2$C01CSNT == "N" & is.na(elem_data2$C01STDT)==0, ]
elem_data2$C01CSNT = ifelse(elem_data2$C01CSNT == "Y", 1, 0)



### Preprocessing

## missing
## 설문 완료하지 않은 행 제거
elem_data3 = elem_data2[elem_data2$C01CPLTE == 1,]
dim(elem_data3)   ## 2207 row
head(elem_data3)
summary(elem_data3)
elem_data3 = subset(elem_data3, select=-c(C01CPLTE, C01CSNT))



####################################################################################
####################################################################################
# NA가 많은 column
sort(colSums(is.na(elem_data3)), decreasing=T) 



####################################################################################
###################### 전체 대상으로 lsirm 돌리기 
####################################################################################
# binary coding 
elem_data3$C01SEX = ifelse(elem_data3$C01SEX == 1, 1, 0)
elem_data3$C01CE1 = ifelse(elem_data3$C01CE1 == 1, 0, 1)
elem_data3$C01CE2 = ifelse(elem_data3$C01CE2 == 1, 0, 1)
elem_data3$C01DH06 = ifelse(elem_data3$C01DH06 <= 3, 0, 1)
elem_data3$C01SIV01 = ifelse(elem_data3$C01SIV01 <= 3, 0, 1)
elem_data3$C01SIV02 = ifelse(elem_data3$C01SIV02 <= 3, 0, 1)
elem_data3$C01SIV03 = ifelse(elem_data3$C01SIV03 <= 3, 0, 1)
# elem_data3$C01STDT = ifelse(elem_data3$C01STDT <= 3, 0, 1)

# grep("^C01FRN", colnames(elem_data), value = T) ## , "C01FRNc", "C01FRNOLc" 친구 수 열 없음
covariate_var = c("C01SEX","C01STDTc", "C01PTTc", "C01BKTc", "C01ACVTc", "P01FINCM", "P01FJOB", "P01MJOB")

C01SH = grep("^C01SH[0-9]+$", colnames(elem_data3), value = T)
C01DH = grep("^C01DH[0-9]+$", colnames(elem_data3), value = T)
C01SIV = grep("^C01SIV[0-9]+$", colnames(elem_data3), value = T)
# C01FRL = grep("^C01FRL", colnames(elem_data3), value = T)   # 얘도 없음
# C01TRL = grep("^C01TRL", colnames(elem_data3), value = T)   # 얘도 없음
C01SAD = grep("^C01SAD[0-9]+$", colnames(elem_data3), value = T)
C01HP = grep("^C01HP[0-9]+$", colnames(elem_data3), value = T)
C01FD = grep("^C01FD[0-9]+$", colnames(elem_data3), value = T)
C01ST = grep("^C01ST[0-9]+$", colnames(elem_data3), value = T)
C01EM = grep("^C01EM[0-9]+$", colnames(elem_data3), value = T)
C01SS = grep("^C01SS[0-9]+$", colnames(elem_data3), value = T)
C01SPD = grep("^C01SPD[0-9]+$", colnames(elem_data3), value = T)
item_group = c(C01SH, C01DH, C01SIV, C01SAD, C01HP, C01FD, C01ST, C01EM, C01SS,C01SPD)
item_group = c(C01SH, C01DH, C01SAD, C01HP, C01FD, C01ST, C01EM, C01SS,C01SPD)

# item_var = c("C01SID", "C01DGT02c", "C01HE1", "C01HE2", "C01HE3", "C01CE1", "C01CE2", covariate_var, item_group)
item_var = c("C01SID", "C01DGT02c", "C01CE1", "C01CE2", item_group)


elem_data4 = subset(elem_data3, select=item_var)
colnames(elem_data4)
head(elem_data4)

# colSums(is.na(elem_data4))
elem_data4 = na.omit(elem_data4)
dim(elem_data4)












# 
# 
# attach(elem_data4)
# for(i in unique(P01FJOB)){
#   elem_data4[, paste0("P01FJOB_", i)] = 0
#   elem_data4[elem_data4$P01FJOB == i, paste0("P01FJOB_", i)] = 1
# }
# for(i in unique(P01MJOB)){
#   elem_data4[, paste0("P01MJOB_", i)] = 0
#   elem_data4[elem_data4$P01MJOB == i, paste0("P01MJOB_", i)] = 1
# }
# detach(elem_data4)
# 
# 
# elem_data4 = elem_data4[elem_data4$P01FINCM < 12,]
# elem_data4$P01FINCM_1 = ifelse(elem_data4$P01FINCM <= 3, 1, 0)
# elem_data4$P01FINCM_2 = ifelse(elem_data4$P01FINCM > 4 & elem_data4$P01FINCM <= 7, 1, 0)
# elem_data4$P01FINCM_3 = ifelse(elem_data4$P01FINCM > 7, 1, 0)
# 
# elem_data4 = subset(elem_data4, select=-c(P01MJOB, P01FJOB, P01FINCM))
# 
# head(elem_data4)
# dim(elem_data4)
# colnames(elem_data4)

# write.csv(elem_data4, "/Users/seoyoung/Desktop/Team5/elem_data1022.csv", row.names=F)
write.csv(elem_data4, "/Users/seoyoung/Desktop/Team5/elem_data1107.csv", row.names=F)
elem_data4 = read.csv("/Users/seoyoung/Desktop/Team5/elem_data1026.csv")


## LSIRM fitting

elem_lsirm_data = as.matrix(subset(elem_data4, select = -c(C01SID)))
elem_lsirm_data2 = as.matrix(subset(elem_data4, select = -c(C01SID, C01CE1, C01CE2, C01DH06)))
head(elem_lsirm_data)
summary(elem_lsirm_data)
# sum(is.na(elem_lsirm_data))

elem_lsirm_fit = lsirm1pl(data = elem_lsirm_data)
elem_lsirm_fit2 = lsirm1pl(data = elem_lsirm_data2)

setwd("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit")
save(elem_lsirm_fit, file = "elem_lsirm_fit1027.RData")
save(elem_lsirm_fit2, file = "elem_lsirm_fit2_1027.RData")

plot(elem_lsirm_fit2)


elem_lsirm_fit_item = data.frame(w1=elem_lsirm_fit$w_estimate[,1], w2=elem_lsirm_fit$w_estimate[,2], item=colnames(elem_lsirm_data))
ggplot(data=elem_lsirm_fit_item) +
  geom_point(aes(x=w1, y=w2), color="red") +
  geom_text_repel(aes(x=w1, y=w2, label = item), size=3)

elem_lsirm_fit_item2 = data.frame(w1=elem_lsirm_fit2$w_estimate[,1], w2=elem_lsirm_fit2$w_estimate[,2], item=colnames(elem_lsirm_data2))
ggplot(data=elem_lsirm_fit_item2) +
  geom_point(aes(x=w1, y=w2), color="red") +
  geom_text_repel(aes(x=w1, y=w2, label = item), size=3)

load("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit/elem_lsirm_fit.RData")
par(mfrow=c(2,2))

ts.plot(elem_lsirm_fit2$beta[,1])
ts.plot(elem_lsirm_fit2$theta[,3])
ts.plot(elem_lsirm_fit2$z[,2,2])
ts.plot(elem_lsirm_fit2$w[,1,2])









####################################################################################
###################### 학교별로 lsirm 돌리기 
####################################################################################
for(name in unique(elem_data3$C01SID)){
  
  # LSIRM fitting
  elem_tmp = elem_data4[elem_data3$C01SID == name,]
  elem_tmp = as.matrix(subset(elem_tmp, select = -c(C01SID)))
  fit_tmp = lsirm1pl(data = elem_tmp)
  save(fit_tmp, file = paste0("lsirm_fit", name, ".RData"))
  
}

####################################################################################
###################### 학교별로 lsirm 돌리기 
####################################################################################
elem_list_data = list()
count = 1
for(name in unique(elem_data4$C01SID)){
  
  elem_tmp = elem_data4[elem_data4$C01SID == name,]
  elem_tmp = as.matrix(subset(elem_tmp, select = -c(C01SID)))
  elem_list_data[[count]] = elem_tmp
  count = count + 1
  
}
elem_list_data
save(elem_list_data, file = "/Users/seoyoung/Desktop/Team5/elem_list_data1031.RData")

load("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit/lsirm_fit101.RData")
par(mfrow=c(1,1))

elem_fit_files = list.files("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit")
for(file in elem_fit_files){
  load(paste0("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit/", file))
  file_name = substr(file, 1, 12)
  
  df_w = data.frame(w1 = fit_tmp$w_estimate[,1], w2 = fit_tmp$w_estimate[,2])
  ggplot(df_w) + 
    geom_point(mapping=aes(w1, w2), color="red") +
    geom_text(aes(x = w1, y = w2, label= rownames(df_w)))
    ggtitle(file_name)
  
  ggsave(paste0("/Users/seoyoung/Desktop/Team5/Incheon_project/plot/LSIRM/w_estimate/", file_name, ".jpg"), width=20, height=15, units=c("cm"))
}










