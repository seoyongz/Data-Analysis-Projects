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
# dim(elem_data2) ## 2407 row
# elem_data3 = na.omit(elem_data2)
# dim(elem_data2) ## 48 row..
# dim(na.omit(elem_data2))
# colSums(is.na(elem_data2))

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
elem_data3$C01STDT = ifelse(elem_data3$C01STDT <= 3, 0, 1)

grep("^C01FRN", colnames(elem_data3), value = T) ## , "C01FRNc", "C01FRNOLc" 친구 수 없음
covariate_var = c("C01STDTc", "C01PTTc", "C01BKTc", "C01ACVTc")

C01SH = grep("^C01SH", colnames(elem_data3), value = T)
C01DH = grep("^C01DH", colnames(elem_data3), value = T)
C01SIV = grep("^C01SIV", colnames(elem_data3), value = T)
C01FRL = grep("^C01FRL", colnames(elem_data3), value = T)
C01TRL = grep("^C01TRL", colnames(elem_data3), value = T)
C01HP = grep("^C01HP", colnames(elem_data3), value = T)
C01FD = grep("^C01FD", colnames(elem_data3), value = T)
C01ST = grep("^C01ST", colnames(elem_data3), value = T)
C01EM = grep("^C01EM", colnames(elem_data3), value = T)
C01SPD = grep("^C01SPD", colnames(elem_data3), value = T)
item_group = c(C01SH, C01DH, C01SIV, C01FRL, C01TRL, C01TRL, C01HP, C01FD, C01ST, C01EM, C01SPD)

item_var = c("C01SID", "C01DGT02c", "C01HE1", "C01HE2", "C01HE3", "C01CE1", "C01CE2", covariate_var, item_group)

colnames(elem_data3)[!(item_var %in% colnames(elem_data3))]
elem_data4 = subset(elem_data3, select=item_var)

## factorization
# conti_variable = c("P01HGT", "P01WGT", "BMI")
# for(i in 1:dim(elem_data3)[2]){
#   if(!(colnames(elem_data3)[i] %in% conti_variable)){
#     elem_data3[,i] = as.factor(elem_data3[,i])
#   }
# }

summary(elem_data4)
write.csv(elem_data4, "/Users/seoyoung/Desktop/Team5/elem_data1013.csv")


## LSIRM fitting
setwd("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit")
elem_lsirm_data = as.matrix(subset(elem_data4, select = -c(C01SID)))
head(elem_lsirm_data)
summary(elem_lsirm_data)
# sum(is.na(elem_lsirm_data))

elem_lsirm_fit = lsirm1pl(data = elem_lsirm_data)
save(elem_lsirm_fit, file = "elem_lsirm_fit.RData")
plot(elem_lsirm_fit)

### 패키지에 spectral clustering하는거로 grouping하기
### 같이 묶이는 애들 리스트 만들기
### 그룹별로 묶인애들말고 item들이 섞여있는 부분 봐오기
### 따로 있는 애들 (outlier)도 뭐 있는지 보기
### item별로 디스턴스 구해서 
### binary PCA 하기(할때, BSVD 로 해오기, 그거의 eigenvalue랑 eigenvector 뽑아오기) ->
### 어차피 item별로 지표를 만들것. group을 묶으려고 하는거임. 어차피 전학교를 대상으로 score를 매겨야하는 거니까 계층적모형이 의미가 있을지는 모르겠음.
### 


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
###################### 학교별로 Hierarchical lsirm 돌리기 
####################################################################################
elem_list_data = list()
count = 1
for(name in unique(elem_data3$C01SID)){
  
  elem_tmp = elem_data4[elem_data3$C01SID == name,]
  elem_tmp = as.matrix(subset(elem_tmp, select = -c(C01SID)))
  elem_list_data[[count]] = elem_tmp
  count = count + 1
  
}
elem_list_data
save(elem_list_data, file = "/Users/seoyoung/Desktop/Team5/elem_list_data1013.RData")

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



## school ID and school name
unique(elem_data[,c("C01SNAME", "C01SID")])
