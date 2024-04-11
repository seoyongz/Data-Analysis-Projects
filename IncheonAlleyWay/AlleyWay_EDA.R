
setwd("/Users/seoyoung/Desktop/IncheonAlleyWay/csv_data")
data5 = read.csv("data_alleyway.csv")[,-1]
head(data5)

data6 = data5

### EDA


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

setwd("/Users/seoyoung/Desktop/IncheonAlleyWay/csv_data")
write.csv(data7, "data_alleyway_reg.csv")

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
