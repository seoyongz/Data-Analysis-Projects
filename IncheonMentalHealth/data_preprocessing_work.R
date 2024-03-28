### elementary data preprocessed -> saved as "/Users/seoyoung/Desktop/Team5/elem_data1022.csv"
### lsirm fitting -> "/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit/elem_lsirm_fit.RData
### lsirm fitting for each school


library(lsirm12pl)
library(ggplot2)

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/original_data")
elem_data = read.csv("(Data3)인천종단_초등데이터수정본(1010).csv", header=T, fileEncoding = "CP949", encoding = "UTF-8", na.strings = c(-999,"", " "))
head(elem_data)
summary(elem_data)


## Work 1 : 필요없는 column 제거
##################################################################
# C01ID(student ID) : 
# C01GR : 값이 1만 있어서 열 제거
# C01SNAME(school name) : C01SID(school ID)와 중복된 의미이므로 열 제거
# C01QSD(설문 시작 시각)
# C01QED(설문 종료 시각)
# C01Note 
##################################################################
elem_data2 = subset(elem_data, select = -c(C01ID, C01GR, C01SNAME, C01QSD, C01QED, C01NOTE, X.32, X.33))
head(elem_data2)



## Work 2 : Binary recoding
##################################################################
# C01CPLTE : Complete / not complete
elem_data2$C01CPLTE = ifelse(elem_data2$C01CPLTE == "완료", 1, 0)

# C01CSNT : 
# elem_data2[elem_data2$C01CSNT == "N" & is.na(elem_data2$C01STDT)==0, ]
elem_data2$C01CSNT = ifelse(elem_data2$C01CSNT == "Y", 1, 0)

# C01SEX : 성별 1, 2으로 저장되어있던걸 -> 1, 0으로 recoded
elem_data3$C01SEX = ifelse(elem_data3$C01SEX == 1, 1, 0)

# C01CE : 상담경험
elem_data3$C01CE1 = ifelse(elem_data3$C01CE1 == 1, 0, 1)
elem_data3$C01CE2 = ifelse(elem_data3$C01CE2 == 1, 0, 1)

# C01DH : 생활습관
elem_data3$C01DH06 = ifelse(elem_data3$C01DH06 <= 3, 0, 1)

# C01SIV : 학교생활 참여도
elem_data3$C01SIV01 = ifelse(elem_data3$C01SIV01 <= 3, 0, 1)
elem_data3$C01SIV02 = ifelse(elem_data3$C01SIV02 <= 3, 0, 1)
elem_data3$C01SIV03 = ifelse(elem_data3$C01SIV03 <= 3, 0, 1)

# C01STDT : 자기주도 공부시간
elem_data3$C01STDT = ifelse(elem_data3$C01STDT <= 3, 0, 1)


## Work3 : Missing Value
##################################################################
## 설문 완료하지 않은 행 제거
elem_data3 = elem_data2[elem_data2$C01CPLTE == 1, ]
elem_data3 = subset(elem_data3, select = -c(C01CPLTE, C01CSNT))

dim(elem_data3)   ## 2207 row
head(elem_data3)
summary(elem_data3)



####################################################################################
####################################################################################
# NA가 많은 column
sort(colSums(is.na(elem_data3)), decreasing=T) 

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
write.csv(elem_data3, "/Users/seoyoung/Desktop/Team5/elem_data3.csv", row.names=F)








## Work3 : Other Covariates check
##################################################################



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



















