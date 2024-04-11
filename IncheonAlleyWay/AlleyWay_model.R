library(dplyr)
setwd("/Users/seoyoung/Desktop/IncheonAlleyWay/csv_data")
data7 = read.csv("data_alleyway_reg.csv")[,-1]


mean_data5 = data5 %>% 
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
