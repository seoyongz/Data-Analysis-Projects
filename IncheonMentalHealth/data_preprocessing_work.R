
##################################################################
## In this file,
## Work 1. elementary school data is preprocessed : elem_data4
## Work 2. middle school data is preprocessed : mid_data4
## saved in "/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data"
##################################################################








#### Elementary school data preprocessing
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/raw_data")
elem_data = read.csv("(Data3)인천종단_초등데이터수정본(1010).csv", header=T, fileEncoding = "CP949", encoding = "UTF-8", na.strings = c(-999,"", " "))
head(elem_data)
summary(elem_data)


## Work 1-1 : Delete unnecessary columns
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


## Work 1-2 : Missing Value
##################################################################
## Remove survey incomplete rows
unique(elem_data2$C01CPLTE)  ## 완료, 설문진행중, 미실시
rowSums(is.na(elem_data2[elem_data2$C01CPLTE =="완료", ]))
rowSums(is.na(elem_data2[elem_data2$C01CPLTE == "설문진행중", ])) # Some resp have only 1,2,5 NA
rowSums(!is.na(elem_data2[elem_data2$C01CPLTE == "미실시", ])) # All data are NA except SID, CPLTE, CSNT
sum(!is.na(elem_data2[is.na(elem_data2$C01CPLTE), ]))  # All data are NA if C01CPLTE is NA

completed_ind = c(1:nrow(elem_data2))[elem_data2$C01CPLTE == "완료" ]
ing_ind = c(1:nrow(elem_data2))[(elem_data2$C01CPLTE == "설문진행중" & rowSums(is.na(elem_data2)) <= 5)]

elem_data3 = elem_data2[sort(c(completed_ind, ing_ind)), ]
elem_data3 = subset(elem_data3, select = -c(C01CPLTE, C01CSNT))   # C01CSNT : agree/disagree

sum(is.na(elem_data3))  # missing : 12234
colSums(is.na(elem_data3))  
# only following columns have missing :
# C01DE1_1  C01DE2_1  C01DE3_1  C01DE4_1  C01DE5_1  C01DE6_1   
# C01AFS1  C01AFS1_1 C01AFS2_1 C01HE1_1  C01HE1_2  C01HE1_3
# P01HGT  P01WGT  P01FINCM   P01FJOB  P01MJOB   BMI     BMI_c 

dim(elem_data3)   ## 2210 x 206
head(elem_data3)
summary(elem_data3)



## Work 1-3 : Binary recoding
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
write.csv(elem_data4, "elem_data4.csv", row.names=F)




# Work 1-4 : Variable Select -> elem_data5 save
##########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
elem_data4 = read.csv("elem_data4.csv")

covariate_var = c("C01SEX","C01STDTc", "P01FINCM", "P01FJOB", "P01MJOB")
item_names = colnames(elem_data4)

## Get variable names by item category
selected_items = c("C01SID")
item_category = c("SH", "DH", "CE", "SIV", "SAD", "HP", "FD", "ST", "EM", "SS", "SPD")
for(x in item_category){
  selected_items = c(selected_items, grep(paste0("^C01", x, "[0-9]+$"), item_names, value = T))
}
selected_items = c(selected_items, "C01DGT01c", "C01DGT02c")
selected_items
elem_data5 = subset(elem_data4, select = selected_items)

head(elem_data5)

sum(is.na(elem_data5))  ## no missing check
dim(elem_data5)   ## 2207 x 83

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
write.csv(elem_data5, "elem_data5.csv", row.names=F)


























#### Middle school data preprocessing

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/raw_data")
mid_data = read.csv("(Data3)인천종단_중등데이터수정본(1010).csv", header=T, encoding = "UTF-8", na.strings = c(-999,"", " "))
head(mid_data)
summary(mid_data)


## Work 2-1 : Delete unnecessary columns
##################################################################
# C01ID(student ID) 
# C01GR : this column has unique value 2
# C01SNAME(school name) : same meaning with C01SID(school ID)
# C01QSD(Question Started Time)
# C01QED(Question Ended Time)
# C01Note 
# C01RC, X.43, X.44 : unknown columns
##################################################################
mid_data2 = subset(mid_data, select = -c(C01ID, C01GR, C01SNAME, C01QSD, C01QED, C01NOTE, C01RC, X.43, X.44))
head(mid_data2)
summary(mid_data2)


## Work 2-2 : Missing Value 1
##################################################################
## Remove survey incomplete rows
unique(mid_data2$C01CPLTE)  ## 완료, 설문진행중, 미실시, NA

rowSums(is.na(mid_data2[mid_data2$C01CPLTE =="완료", ]))
rowSums(is.na(mid_data2[mid_data2$C01CPLTE == "설문진행중", ]))  # all resp have NA more than 12
rowSums(!is.na(mid_data2[mid_data2$C01CPLTE == "미실시", ])) # NA except SID, CPLTE, CSNT
sum(!is.na(mid_data2[is.na(mid_data2$C01CPLTE), ]))  # All data are NA if C01CPLTE is NA

mid_data3 = mid_data2[mid_data2$C01CPLTE %in% c("완료"),  ]
mid_data3 = subset(mid_data3, select = -c(C01CPLTE, C01CSNT))   # C01CSNT : agree/disagree

sum(is.na(mid_data3))  # missing : 15650
colSums(is.na(mid_data3)) 

dim(mid_data3)   ## 2445 x 268
head(mid_data3)
summary(mid_data3)



## Work 2-3 : Binary recoding
##################################################################
mid_data4 = mid_data3

# C01SEX : 1(M), 2(F) -> 1(M), 0(F)
unique(mid_data4$C01SEX)
mid_data4$C01SEX = ifelse(mid_data4$C01SEX == 1, 1, 0)

# C01CE(Councel Experience)
table(mid_data4$C01CE1)
table(mid_data4$C01CE2)
mid_data4$C01CE1 = ifelse(mid_data4$C01CE1 == 1, 0, 1)
mid_data4$C01CE2 = ifelse(mid_data4$C01CE2 == 1, 0, 1)

# C01DH06(Doing Habits) : reverse coding
table(mid_data4$C01DH06)
mid_data4$C01DH06 = ifelse(mid_data4$C01DH06 <= 2, 1, 0)

# C01SIV(School Activity Participation) 
mid_data4$C01SIV01 = ifelse(mid_data4$C01SIV01 <= 3, 0, 1)
mid_data4$C01SIV02 = ifelse(mid_data4$C01SIV02 <= 3, 0, 1)
mid_data4$C01SIV03 = ifelse(mid_data4$C01SIV03 <= 3, 0, 1)

summary(mid_data4)
head(mid_data)


setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
write.csv(mid_data4, "mid_data4.csv", row.names=F)




# Work 2-4 : Variable Select -> mid_data5 save
##########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
mid_data4 = read.csv("mid_data4.csv")

covariate_var = c("C01SEX","C01STDTc", "P01FINCM", "P01FJOB", "P01MJOB")
item_names = colnames(mid_data4)

## Get variable names by item category
selected_items = c("C01SID")
item_category = c("SH", "DH", "CE", "SIV", "SAD", "FRL", "TRL", "HP", "FD", "ST", "EM", "SS", "SPD")
for(x in item_category){
  selected_items = c(selected_items, grep(paste0("^C01", x, "[0-9]+$"), item_names, value = T))
}
selected_items = c(selected_items, "C01DGT01c", "C01DGT02c")
selected_items
mid_data5 = subset(mid_data4, select = selected_items)

head(mid_data5)
colSums(is.na(mid_data5))   
sum(is.na(mid_data5)) # No NA

dim(mid_data5)   ## 2404 x 91

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
write.csv(mid_data5, "mid_data5.csv", row.names=F)













