library("tidyr")
library("stringr")
library("ggplot2")
library("dplyr")
library("ggrepel")
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


# 카테고리별, 매장별 시계열 그래프 그리기

# for(i in unique(as.character(data6$category))){
#   setwd(paste0("/Users/seoyoung/Desktop/23-1 bigdata/EDA/시계열2/", str_trim(i, side="both")))
#   
#   for(cat_name in unique(as.character(data6[data6$category==i, ]$detailed_category))){
#     
#     for(store_name in unique(as.character(data6[data6$detailed_category==cat_name, ]$store))){
#       ggplot(data=data6[data6$store==store_name,])+
#         geom_line(aes(x=date, y=total_sales))+
#         ggtitle(paste0(cat_name," : ",store_name))
#       ggsave(file=paste0(str_trim(cat_name, side="both"), "_", str_trim(store_name, side="both"),".jpg"), width=20, height=15, units=c("cm"))
#     }
#   }
# }

table(data6$store)

# data6[is.na(data6$floor_space2), "floor_space2"] = floor_space
# data6[is.na(data6$sales_per_space), "sales_per_space"] = total_sales/(floor_sapce2/(date-open_date))
# sum(is.na(data6$sales_per_space))

## 데이터 저장
# setwd("/Users/seoyoung/Desktop/23-1 bigdata")
# write.csv(data7, "data_alleyway.csv")
# data7 = read.csv("data_alleyway.csv", header=T)
data7 = data6[is.na(data6$sales_per_space)==0, ]
data7 = data7[data7$sales_count>0 & data7$sales_per_count>0,]
data7$good = ifelse(data7$sales_per_space>1000000, 1, 0)

write.csv(data7, "data_alleyway.csv")
summary(data7)
head(data7)

plot(log(data7$sales_per_count), log(data7$sales_per_space))


# 각 매장별 시계열 그래프 저장
# for(store_name in unique(data6$store)){
#   detailed_cat_name = unique(data6[data6$store==store_name, "detailed_category"])
#   cat_name = unique(data6[data6$store==store_name, "category"])
#   setwd(paste0("/Users/seoyoung/Desktop/23-1 bigdata/ppt_plots/시계열/", str_trim(cat_name, side="both")))
# 
#   ggplot(data=data6[data6$store==store_name,])+
#     geom_line(aes(x=date, y=sales_per_space, group=store)) +
#     ggtitle(paste0(detailed_cat_name," : ", store_name)) +
#     geom_hline(yintercept=1000000, color="#E4004E")
#     # scale_x_date(date_breaks = "3 months", limits=c(min(data6$date), max(data6$date))) +
#     # scale_y_continuous(limits = c(min(na.omit(data6$sales_per_space)),max(na.omit(data6$sales_per_space))))
#   ggsave(file=paste0(str_trim(detailed_cat_name, side="both"), "_", str_trim(store_name, side="both"),".jpg"), width=20, height=15, units=c("cm"))
# }


for(store_name in unique(data6$store)){
    
    # setwd("/Users/seoyoung/Desktop/23-1 bigdata/EDA/시계열+회귀")
    coef_tmp = lm(data7[data7$store == store_name,"sales_per_space"] ~ data7[data7$store == store_name, "date"] )$coef
    data7[data7$store == store_name, "intercept"] = coef_tmp[1]
    data7[data7$store == store_name, "slope"] = coef_tmp[2]
    # detailed_cat_name = unique(data6[data6$store==store_name, "detailed_category"])

    # ggplot(data=data7[data7$store==store_name,])+
    #   geom_line(aes(x=date, y=sales_per_space, group=store)) +
    #   ggtitle(paste0(detailed_cat_name," : ", store_name)) +
    #   geom_hline(yintercept=1000000, color="red") + 
    #   geom_abline(slope=coef_tmp[2], intercept=coef_tmp[1], color="blue")
    #   
    # ggsave(file=paste0(str_trim(detailed_cat_name, side="both"), "_", str_trim(store_name, side="both"),".jpg"), width=20, height=15, units=c("cm"))
}

data7$increase = 0
data7[data7$slope>0, "increase"] = 1

head(data7)

setwd("/Users/seoyoung/Desktop/23-1 bigdata/")
write.csv(data7, file="alleyway0731.csv", fileEncoding = "euc-kr")

# 건 단가 vs. 평 효율
ggplot(data=data7) +
  geom_point(aes(x = log(sales_per_count), y = log(sales_per_space), group=detailed_category, color=detailed_category), alpha=0.7, size=0.7)+
  facet_wrap(~category)

ggplot(data=data7) +
  geom_point(aes(x = log(sales_per_count), y = log(sales_per_space), group=store, color=store), alpha=0.7, size=0.7)+
  facet_wrap(~detailed_category)




## 10-20대 / 30-40대
levels(data6$category)
unique(data6$category)
cat1020 = c("오렌즈", "니뽕내뽕", "마라공방", "버거킹", "우리할매떡볶이") ## red
cat3040 = c("모던하우스","올젠",  "폴햄키즈", "마마스부띠끄", "STCO") ## blue


# F&B
ggplot() +
  geom_line(data = data6[data6$store %in% cat1020,], aes(x=date, y=sales_per_space, group=store, colour=store), alpha = 0.7) +
  geom_line(data = data6[data6$store %in% cat3040,], aes(x=date, y=sales_per_space, group=store, colour=store), alpha = 0.7) +
  facet_wrap(~detailed_category)

  theme(legend.position="top")
  
ggplot()
  

# RETAIL
ggplot(data=data6[data6$category==cat1[2], ]) +
  geom_line(aes(x=date, y=sales_per_space, group=store, color=store), alpha = 0.5) +
  facet_wrap(~detailed_category)

# SERVICE
ggplot(data=data6[data6$category==cat1[3], ]) +
  geom_line(aes(x=date, y=sales_per_space, group=store, color=store), alpha = 0.5) +
  facet_wrap(~detailed_category)













############################################################################################
# 서울시 상권 데이터 EDA
# install.packages("ggmap")
# install.packages("raster")
# install.packages("rgeos")
# install.packages("maptools")
# install.packages("rgdal")
# 
# library(ggmap)
# library(raster)
# library(rgeos)
# library(maptools)
# library(rgdal)
# 
# setwd("/Users/seoyoung/Desktop/23-1 bigdata/외부데이터")
# seoul_store = read.csv("seoul_store.csv", header=T)
# incheon_store = read.csv("incheon_store.csv", header=T)
# head(seoul_store)
# names(seoul_store)
# summary(seoul_store)
# 
# 
# seoul_store2 = subset(seoul_store, select=c("상호명", "시군구코드", "시군구명","행정동코드","행정동명", "상권업종대분류코드", "상권업종대분류명",  "상권업종중분류코드", "상권업종중분류명", "상권업종소분류코드", "상권업종소분류명","표준산업분류명", "위도", "경도"))
# incheon_store2 = subset(incheon_store, select=c("상호명", "시군구코드", "시군구명","행정동코드","행정동명", "상권업종대분류코드", "상권업종대분류명",  "상권업종중분류코드", "상권업종중분류명", "상권업종소분류코드", "상권업종소분류명","표준산업분류명", "위도", "경도"))
# seoul_store2 = seoul_store2[seoul_store2$상권업종대분류명 %in% c("음식", "소매", "교육", "수리·개인"),]
# incheon_store2 = incheon_store2[seoul_store2$상권업종대분류명 %in% c("음식", "소매", "교육", "수리·개인"),]
# 
# 
# write.csv(seoul_store2, file = "seoul_store2.csv")
# write.csv(incheon_store2, file = "incheon_store2.csv")
# 
# 
# setwd("/Users/seoyoung/Desktop/23-1 bigdata/외부데이터/전처리완료")
# 
# seoul_store3 = read.csv("seoul_store2.csv", header=T)
# incheon_store3 = read.csv("incheon_store2.csv", header=T)
# 
# seoul_store3$상권업종대분류명 = as.factor(seoul_store3$상권업종대분류명)
# seoul_store3$상권업종중분류명 = as.factor(seoul_store3$상권업종중분류명)
# seoul_store3$상권업종소분류명 = as.factor(seoul_store3$상권업종소분류명)
# summary(seoul_store3)
# 
# unique(seoul_store2[seoul_store2$상권업종대분류명 == "음식", "상권업종소분류명"])
# seoul_store3[seoul_store3$상호명=="더하루",]
# 
# 
# 
# 
# ## ggmap 사용
# school_data = read.csv("school_data.csv", header=T, fileEncoding = "euc-kr")
# head(school_data)
# register_google(key = 'AIzaSyBShjcHkeXnUHscPw6JZ6U-a8J4dxI3SiU')
# address = school_data$주소
# address = enc2utf8(address)
# school = geocode(data = address, address, source='google')
# head(school)
# 
# 
# #서울 맵 가져오기
# seoul = get_map("Seoul, South Korea", zoom=11, maptype = "roadmap")
# incheon = get_map("Incheon, South Korea", zoom=11, maptype = "roadmap")
# ## Source : https://maps.googleapis.com/maps/api/staticmap?center=Seoul,%20South%20Korea&zoom=11&size=640x640&scale=2&maptype=roadmap&language=en-EN&key=xxx
# ## Source : https://maps.googleapis.com/maps/api/geocode/json?address=Seoul,+South+Korea&key=xxx
# #좌표 표시하고 업종별로 색 입히기
# 
# ggmap(seoul) +
#   geom_point(data=stores[stores$상권업종대분류명=="음식",], mapping=aes(x=경도, y=위도, group=상권업종소분류명, color=상권업종소분류명), size=0.2, alpha=0.4) +
#   facet_wrap(~상권업종중분류명)+
#   geom_point(school, mapping=aes(x=lon, y=lat), color='red', size=0.5)
#   
# table(stores[stores$상권업종대분류명=="음식","상권업종소분류명"])
#   
# 
# 
#   geom_point(stores[stores$상권업종소분류명 == "족발/보쌈",], mapping = aes(x = 경도, y = 위도), size=0.2, alpha=0.5, color="blue") +
#   geom_point(stores[stores$상권업종소분류명 == "마라탕/훠궈",], mapping = aes(x = 경도, y = 위도), size=0.2, alpha=0.5, color="red") 
#   
#   # for(store_name in unique(data6$store)){
#   #   detailed_cat_name = unique(data6[data6$store==store_name, "detailed_category"])
#   #   cat_name = unique(data6[data6$store==store_name, "category"])
#   #   setwd(paste0("/Users/seoyoung/Desktop/23-1 bigdata/EDA/시계열3/", str_trim(cat_name, side="both")))
#   #   
#   #   ggplot(data=data6[data6$store==store_name,])+
#   #     geom_line(aes(x=date, y=sales_per_space, group=store)) +
#   #     ggtitle(paste0(detailed_cat_name," : ", store_name)) +
#   #     geom_hline(yintercept=1000000, color="red") 
#   #     # scale_x_date(date_breaks = "3 months", limits=c(min(data6$date), max(data6$date))) +
#   #     # scale_y_continuous(limits = c(min(na.omit(data6$sales_per_space)),max(na.omit(data6$sales_per_space))))
#   #   ggsave(file=paste0(str_trim(detailed_cat_name, side="both"), "_", str_trim(store_name, side="both"),".jpg"), width=20, height=15, units=c("cm"))
#   # }
# 
#   
# ## 지도 시각화 저장
# setwd("/Users/seoyoung/Desktop/23-1 bigdata/외부데이터/인천 상권 데이터/소분류별")
# for(store in unique(stores$상권업종소분류명)){
#   
#   ggmap(incheon) +
#     geom_point(data=stores[stores$상권업종소분류명==store,], mapping=aes(x=경도, y=위도), size=0.2, alpha=0.7, color="#FF3300")
#   ggsave(file=paste0(gsub("/","+",store),".jpg"), width=17, height=17, units=c("cm"))
#     
# }
#   
# 
# 
# ## EDA
# stores = seoul_store3
# # stores = incheon_store3
# ggplot()+
#   # geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='blue') + 
#   geom_point(data=stores[stores$상권업종소분류명 == "마라탕/훠궈",], aes(x=경도, y=위도), color = "red", size=0.7, alpha=0.5) +
#   geom_point(data=stores[stores$상권업종소분류명 == "족발/보쌈",], aes(x=경도, y=위도), color = "blue", size=0.7, alpha=0.5)
#   
# sum(stores$상권업종소분류명 == "마라탕/훠궈")
# sum(stores$상권업종소분류명 == "족발/보쌈")
#   
# # 동별로 평균 연령대, 마라탕과 족발 비율 시각화
# library(dplyr)

# dong_store = stores %>% 
#   group_by(행정동명, 상권업종소분류명) %>%
#   summarise(count = n())
# 
# dong_store = as.data.frame(dong_store)
# 
# dong_store2 = dong_store %>% 
#   spread(상권업종소분류명, count)
# summary(dong_store2)
# 
# dong_store2[is.na(dong_store2)] = 0
# sum(is.na(dong_store2))
# head(dong_store2)
# dim(dong_store2)
# 
# 
# # F&B 행정동 별 matrix 만들기 
# FnB = stores[stores$상권업종대분류명=="음식",]
# FnB = FnB %>% 
#   group_by(행정동명, 상권업종소분류명) %>%
#   summarise(count = n())
# 
# FnB = as.data.frame(FnB) %>%
#   spread(상권업종소분류명, count)
# FnB[is.na(FnB)] = 0
# 
# seoul_FnB = FnB   # 425x42 (43?)
# incheon_FnB = FnB

# write.csv(seoul_FnB, file="seoul_FnB.csv")
# write.csv(incheon_FnB, file="incheon_FnB.csv")
# 
# 
# store_matrix = FnB %>% 
#     group_by(행정동명, 상권업종소분류명) %>%
#     summarise(count = n())
# 
# FnB = as.data.frame(FnB) %>%
#     spread(상권업종소분류명, count)
# FnB[is.na(FnB)] = 0
# 
# 
# dong_store = stores %>% 
#     group_by(행정동명, 상권업종소분류명) %>%
#     summarise(count = n())
# 
# 
# dong_store2 =as.data.frame(dong_store) %>%
#     spread(상권업종소분류명, count)
# dong_store2[is.na(dong_store2)] = 0





sum(data7[data7$store=="마라공방" & data7$date < "2023-01-01" & data7$date >= "2022-01-01", "total_sales"] )
sum(data7[data7$store=="올리브영" & data7$date < "2022-01-01" & data7$date >= "2021-01-01","total_sales"])


# 회귀 다시 돌려보기
naver = read.csv("/Users/seoyoung/Desktop/23-1 bigdata/외부데이터/blackkiwi_storename.csv", fileEncoding = "euc-kr")


mean_data7 = data7 %>% 
  group_by(store)%>%
  summarise(mean_sales_per_space = mean(sales_per_space),
            mean_sales_per_count = mean(sales_per_count),
            floor = unique(floor),
            category = unique(category),
            detailed_category = unique(detailed_category),
            location=unique(location),
            increase = unique(increase),
            slope = unique(slope),
            closed = unique(closed))

data8 = merge(mean_data7, naver, by="store")
data8[is.na(data8)] = 0
data8$y_100 = 0
data8[data8$mean_sales_per_space>1000000, "y_100"] = 1



# 20대 유의함 + 계수가 양수
install.packages("sjPlot")
library(sjPlot)
glm_fit = summary(glm(y_100 ~ age_20, data=data8, family=binomial))
tab_model(glm(y_100 ~ age_20, data=data8, family=binomial))



unique(data8[data8$mean_sales_per_space>1000000, "store"])
unique(data8[data8$mean_sales_per_space>1000000 &data8$mean_sales_per_space<2000000& data8$increase==1, "store"])
unique(data8[data8$mean_sales_per_space>1000000 &data8$mean_sales_per_space<2000000& data8$increase==0, "store"])
data8[data8$mean_sales_per_space<700000 & data8$increase==0, "store"]
data8[data8$mean_sales_per_space>700000 & data8$mean_sales_per_space<1000000 & data8$increase==0, "store"]

write.csv(data8, "alleyway_data8_서영.csv", fileEncoding = "euc-kr")

data8[data8$closed==1,]

head(data8,10)
data8[order(data8$mean_sales_per_space, decreasing=T),c("store", "mean_sales_per_space", "increase")]

unique(data7[data7$store %in% c("망원동티라미수", "앤티앤스", "호두당", "캠토토스트", "인생닭강정"),c("floor_space", "store")])
unique(data7[data7$floor_space<15, c("store", "floor_space")])


text_plot = data.frame(store=data8$store, x=data8$mean_sales_per_space, y=data8$slope, closed=data8$closed)
text_plot = text_plot[text_plot$store!="유어블랑",]
(text_plot[text_plot$x<700000,])[order(text_plot[text_plot$x<700000,"x"], decreasing=T),'store']


plot_closed = text_plot[text_plot$closed==1,]
plot_good = text_plot[text_plot$x>2000000,]
plot_normal = text_plot[text_plot$closed==0 & text_plot$x<=2000000,]


ggplot() +
  geom_point(data=plot_normal, mapping=aes(x=log10(x), y=y), color="grey50", size=1.5) +
  geom_point(data=plot_closed,mapping=aes(x=log10(x), y=y), color="red", size=1.5) +
  geom_point(data=plot_good, mapping=aes(x=log10(x), y=y), color="blue", size=1.5) +
  
  geom_text_repel(data=plot_normal, mapping=aes(x=log10(x), y=y, label=store),
                  family="AppleGothic", size=3, fontface="bold",
                  segment.curvature=-1e-20,
                  arrow=arrow(length=unit(0.015, "npc"))
                  ) +
  geom_text_repel(data=plot_closed, mapping=aes(x=log10(x), y=y, label=store),
                  family="AppleGothic", size=4, fontface="bold",
                  color="red",
                  segment.curvature=-1e-20,
                  arrow=arrow(length=unit(0.015, "npc"))
  ) +
  geom_text_repel(data=plot_good, mapping=aes(x=log10(x), y=y, label=store),
                  family="AppleGothic", size=4, fontface="bold",
                  color="blue",
                  segment.curvature=-1e-20,
                  arrow=arrow(length=unit(0.015, "npc"))
  ) +
  
  geom_hline(yintercept=0, color="red", lty="dashed") + 
  geom_vline(xintercept=6, color="red", lty="dashed") + 
  theme(plot.background = element_rect(fill = "gray91"),
        panel.background = element_rect(fill = "gray91"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size=0.2, linetype="solid", colour="black")) +
  xlab("log(평효율)")+
  ylab("slope")
  
  geom_text(label=store)
  

quartzFonts(sans = quartzFont(rep("AppleGothic", 4)),
              serif = quartzFont(rep("AppleMyungjo", 4)),
              mono = quartzFont(c("D2Coding", "D2CodingBold", "D2Coding", "D2CodingBold")))  
    
  ggplot() +
    geom_point(data=plot_normal, mapping=aes(x=log10(x), y=y), color="grey50", size=4) +
    geom_point(data=plot_closed,mapping=aes(x=log10(x), y=y), color="red", size=4) +
    geom_point(data=plot_good, mapping=aes(x=log10(x), y=y), color="blue", size=4) +
    
    geom_text_repel(data=text_plot, mapping=aes(x=log10(x), y=y, label=store),
                    family="sans", size=5, fontface="bold",
                    segment.curvature=-1e-20,
                    segment.size=0.2) +
    
    geom_hline(yintercept=0, color="red", lty="dashed") + 
    geom_vline(xintercept=6, color="red", lty="dashed") + 
    theme(plot.background = element_rect(fill = "gray91"),
          panel.background = element_rect(fill = "gray91"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
          #axis.line = element_line(size=0.2, linetype="solid", colour="black")
          ) +
    xlab("log(평효율)") +
    ylab("slope")

  
length(data8[data8$mean_sales_per_space>1000000 & data8$closed==0, "store"])
length(data8[data8$mean_sales_per_space<1000000 & data8$closed==0 , "store"])
length(data8[data8$closed==1 , "store"])
length(data8$store)
