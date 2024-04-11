library(tidyr)
library(stringr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(extrafont)
theme_set(theme_grey(base_family='NanumGothic'))
font_import()


setwd("/Users/seoyoung/Desktop/IncheonAlleyWay/raw_data/02_운영데이터")
data = read.csv("2304_앨리웨이인천_일별매출.csv", header=T, skip=1)
head(data, 100)
colnames(data)
summary(data)


########### 전처리 ############
# 1열 제거 -> data2
data2 = data[, -1]
data2[1:20, 1:20]

## 알수없는 문자열 이름 바꾸기
data2[data2$세부==unique(data2$세부)[9], ]$세부 = "CAFE&DESSERT"
data2[data2$매장명=="세라젬", ]$세부 = "H&B"

data2[data2 == ""] = NA



# 데이터 세로로 붙이기 -> data3
data3 = data.frame()
for(i in seq(10, 270, 10)){
  names(data2)[i:(i+9)] = c("날짜", "요일", "평", "온도", "날씨", "미세먼지", "총매출", "영수건수", "건단가", "평효율")
  data3 = rbind(data3, cbind(data2[,1:9], data2[,i:(i+9)]))
}

# 분석 목적과 필요없는 column 삭제
data3 = subset(data3, select = -c(온도, 날씨, 미세먼지))
colSums(is.na(data3))   

# 수치 데이터 수치형으로 바꾸기 -> 공백으로 입력된 NA 구별
num_var = c("면적", "평", "총매출", "영수건수", "건단가", "평효율")
for(var in num_var){
  data3[, var] = as.numeric(gsub(",", "", data3[, var]))
}
summary(data3)

unique(data3[,c("매장명", "평", "면적")]) # 평과 면적이 같음


# 날짜 데이터 날짜형으로 바꾸기
data3$날짜 = as.Date(data3$날짜, format="%Y/%m/%d")
data3$오픈일자 = as.Date(data3$오픈일자, format="%Y.%m.%d")
data3$폐점일자 = as.Date(data3$폐점일자, format="%Y.%m.%d")

# 카테고리, 매장명 factor화
data3$카테고리 = as.factor(str_trim(data3$카테고리, side="both"))
data3$매장명 = as.factor(data3$매장명)



# Missing values

## 코드명
head(data3[is.na(data3$코드), ])
rowSums(is.na(data3[is.na(data3$코드), ]))
data4 = data3[is.na(data3$코드) != 1, ]
colSums(is.na(data4))  

## 평 : 면적이 missing 이 없으니까 column 제거
data4 = subset(data4, select = -평)
colSums(is.na(data4))  

# 폐점일자 : 아직 폐점을 하지 않은 매장 -> 폐점 여부 변수와 영업기간 변수 생성
unique(data4[is.na(data4$폐점일자), "매장명"])
unique(data4[is.na(data4$오픈일자), "매장명"])  # 오픈일자가 없음
data4[data4$매장명 == "한촌설렁탕", "오픈일자"] = min(data4[data4$매장명 == "한촌설렁탕" & is.na(data4$날짜)==0, "날짜"])

data4$closed = ifelse(is.na(data4$폐점일자), 0, 1)

data4[data4$closed == 1, "sales_period"] = difftime(data4[data4$closed == 1, "폐점일자"], data4[data4$closed == 1, "오픈일자"])

max_date = max(data4[is.na(data4$날짜)==0, "날짜"])
data4[data4$closed == 0, "sales_period"] = difftime(max_date, data4[data4$closed == 0, "오픈일자"])


# 평효율 missing 제거
colSums(is.na(data4)) 
data5 = data4[is.na(data4$평효율) == 0, ]

colSums(is.na(data5)) # 건단가, 영수건수 missing 28 remain
data5 = data5[is.na(data5$건단가) == 0, ]


names(data5) = c("code", "category", "detailed_category", "location", "floor", "floor_space", "store", "open_date", "closed_date", 
                 "date", "day", "total_sales", "sales_count", "sales_per_count",
                 "sales_per_space", "closed", "sales_period")

setwd("/Users/seoyoung/Desktop/IncheonAlleyWay/csv_data")
write.csv(data5, "data_alleyway.csv", row.names=F)


