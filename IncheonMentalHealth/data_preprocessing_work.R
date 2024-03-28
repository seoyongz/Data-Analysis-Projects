
##################################################################
## In this file, data is preprocessed.
## preprocessed elementary data : elem_data4
## saved in "/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data"
##################################################################


library(lsirm12pl)
library(ggplot2)

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/raw_data")
elem_data = read.csv("(Data3)인천종단_초등데이터수정본(1010).csv", header=T, fileEncoding = "CP949", encoding = "UTF-8", na.strings = c(-999,"", " "))
head(elem_data)
summary(elem_data)


## Work 1 : Delete unnecessary columns
##################################################################
# C01ID(student ID) : 
# C01GR : this column has unique value 1
# C01SNAME(school name) : same meaning with C01SID(school ID)
# C01QSD(Question Started Time)
# C01QED(Question Ended Time)
# C01Note 
# X.32, X.33 : unknown columns
##################################################################
elem_data2 = subset(elem_data, select = -c(C01ID, C01GR, C01SNAME, C01QSD, C01QED, C01NOTE, X.32, X.33))
head(elem_data2)
summary(elem_data2)


## Work2 : Missing Value
##################################################################
## Remove survey incomplete rows
unique(elem_data2$C01CPLTE)  ## 완료, 설문진행중, 미실시
elem_data3 = elem_data2[elem_data2$C01CPLTE == "완료", ]
elem_data3 = subset(elem_data3, select = -c(C01CPLTE, C01CSNT))   # C01CSNT : agree/disagree

sum(is.na(elem_data3))  # missing : 12226
colSums(is.na(elem_data3))  
# only following columns have missing :
# C01DE1_1  C01DE2_1  C01DE3_1  C01DE4_1  C01DE5_1  C01DE6_1   
# C01AFS1  C01AFS1_1 C01AFS2_1 C01HE1_1  C01HE1_2  C01HE1_3
# P01HGT  P01WGT  P01FINCM   P01FJOB  P01MJOB   BMI     BMI_c 

dim(elem_data3)   ## 2207 row
head(elem_data3)
summary(elem_data3)



## Work 3 : Binary recoding
##################################################################
elem_data4 = elem_data3

# C01SEX : 1(M), 2(F) -> 1(M), 0(F)
elem_data4$C01SEX = ifelse(elem_data4$C01SEX == 1, 1, 0)

# C01CE(Councel Experience)
table(elem_data4$C01CE1)
table(elem_data4$C01CE2)
elem_data4$C01CE1 = ifelse(elem_data4$C01CE1 == 1, 0, 1)
elem_data4$C01CE2 = ifelse(elem_data4$C01CE2 == 1, 0, 1)

# C01DH06(Doing Habits) : reverse coding
table(elem_data4$C01DH06)
elem_data4$C01DH06 = ifelse(elem_data4$C01DH06 <= 2, 1, 0)

# C01SIV(School Activity Participation) 
elem_data4$C01SIV01 = ifelse(elem_data4$C01SIV01 <= 3, 0, 1)
elem_data4$C01SIV02 = ifelse(elem_data4$C01SIV02 <= 3, 0, 1)
elem_data4$C01SIV03 = ifelse(elem_data4$C01SIV03 <= 3, 0, 1)

summary(elem_data4)
head(elem_data)


setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
write.csv(elem_data4, "elem_data4.csv")




### Other covariates

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



















