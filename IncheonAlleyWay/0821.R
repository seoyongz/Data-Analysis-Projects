library("tidyr")
library("stringr")
library("ggplot2")
library("ggthemes")
library(extrafont)
theme_set(theme_grey(base_family='NanumGothic'))
font_import()

setwd("/Users/seoyoung/Desktop/23-1 bigdata/02_운영데이터")
data = read.csv("2304_앨리웨이인천_일별매출.csv", header=T, skip=1)
head(data, 100)


########### 전처리 ############
# 1열 제거 -> data2
data2 = data[,-1]
data2[1:20,1:20]
data2[data2$세부==unique(data2$세부)[9], ]$세부 = "CAFE&DESSERT"
data2[data2$매장명=="세라젬", ]$세부 = "H&B"



# 데이터 세로로 붙이기 -> data3
data3 = data.frame()
for(i in seq(10, 270, 10)){
  names(data2)[i:(i+9)] = c("날짜", "요일", "평", "온도", "날씨", "미세먼지", "총매출", "영수건수", "건단가", "평효율")
  data3 = rbind(data3, cbind(data2[,1:9], data2[,i:(i+9)]))
}
data3 = data3[is.na(data3$평)!=1, ] # 결측 1차 제거

colnames(data3)[colSums(is.na(data3))>0]
dim(data3[rowSums(is.na(data3))>0,])
# 온도가 결측인 행이 1382
# 코드가 결측인 행이 184
data3[is.na(data3$코드)==1,]  # 코드가 결측이면 아예 값이 다 없음
tail(data3[is.na(data3$온도)==1,], 100)


# 날짜 변환, 수치형 변환
data4 = data3[is.na(data3$온도)==0,]  # 54829행

data4$날짜 = as.Date(data4$날짜, format="%Y/%m/%d")
data4$오픈일자 = as.Date(data4$오픈일자, format="%Y.%m.%d")
data4$폐점일자 = as.Date(data4$폐점일자, format="%Y.%m.%d")
data4$면적 = as.numeric(gsub(",","",data4$면적))
data4$평 = as.numeric(gsub(",","",data4$평))
data4$총매출 = as.numeric(gsub(",","",data4$총매출))
data4$영수건수 = as.numeric(gsub(",","",data4$영수건수))
data4$건단가 = as.numeric(gsub(",","",data4$건단가))
data4$평효율 = as.numeric(gsub(",","",data4$평효율))
str_trim(i, side="both")
data4$카테고리 = as.factor(str_trim(data4$카테고리, side="both"))
data4$세부 = as.factor(str_trim(data4$세부, side="both"))
data4$위치 = as.factor(data4$위치)
data4$층 = as.factor(data4$층)
data4$날씨 = as.factor(data4$날씨)
data4$미세먼지 = as.factor(data4$미세먼지)
data4$매장명 = as.factor(data4$매장명)

summary(data4)
data4[is.na(data4$오픈일자),]


# 결측치 제거
data5 = data4[is.na(data4$오픈일자)==0,]
data5 = data5[is.na(data5$건단가)==0,]

data5[is.na(data5$평),]  # 평이랑 면적이랑 같은건지, 평효율은 어떻게 계산된건지


data5[is.na(data5$폐점일자)==1, "폐점"] = 0
data5[is.na(data5$폐점일자)==0, "폐점"] = 1

summary(data5)
dim(data5)  # 41426행


# Sorting
data5 = data5[order(data5$매장명, data5$날짜),] 
# data5 = subset(data5,select = -평)

summary(data5) 
# 카테고리 : 3개(F&B, RETAIL, SERVICE)
# 세부 : 21개
# 매장 : 74개



### EDA
names(data5)
data6 = data5
names(data6) = c("code", "category", "detailed_category", "location", "floor", "floor_space", "store", "open_date", "closed_date", 
                 "date", "day", "floor_space2", "temperature", "weather", "PM", "total_sales", "sales_count", "sales_per_count",
                 "sales_per_space", "closed")


data7 = data6[is.na(data6$sales_per_space)==0, ]
data7 = data7[data7$sales_count>0 & data7$sales_per_count>0,]
data7$good = ifelse(data7$sales_per_space>1000000, 1, 0)


store_name = unique(data6$store)[1]
for(store_name in unique(data6$store)){
  
  setwd("/Users/seoyoung/Desktop/23-1 bigdata/ppt_plots/시계열")
  coef_tmp = lm(data7[data7$store == store_name,"sales_per_space"] ~ data7[data7$store == store_name, "date"] )$coef
  data7[data7$store == store_name, "intercept"] = coef_tmp[1]
  data7[data7$store == store_name, "slope"] = coef_tmp[2]
  detailed_cat_name = unique(data6[data6$store==store_name, "detailed_category"])
  
  ggplot(data=data7[data7$store==store_name,])+
    geom_line(aes(x=date, y=sales_per_space, group=store)) +
    ggtitle(paste0(detailed_cat_name," : ", store_name)) +
    geom_hline(yintercept=1000000, color="red") + 
    geom_abline(slope=coef_tmp[2], intercept=coef_tmp[1], color="blue") +
    
    theme(plot.background = element_rect(fill = "gray91"),
          panel.background = element_rect(fill = "gray91"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(size=0.2, linetype="solid", colour="black"))
    

  
  ggsave(file=paste0(str_trim(detailed_cat_name, side="both"), "_", str_trim(store_name, side="both"),".jpg"), width=20, height=15, units=c("cm"))
}








