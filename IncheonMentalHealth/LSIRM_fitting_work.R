
##########################################################################
## In this file, 
## Select variables and save the dataframe -> elem_data5.csv
## Fit "elem_data5" using LSIRM -> elem_lsirm_fit1.RData
## Divide "elem_data5" by school and save as list -> elem_list_data.RData
## Fit "elem_list_data" for each school -> elem_lsirm_fit_school.RData
##########################################################################

library(lsirm12pl)




## Work 2-1 : LSIRM fitting
##########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
elem_data5 = read.csv("elem_data5.csv")

elem_lsirm_fit1 = lsirm1pl(data = elem_data5[, -1], 
                           niter=15000, nburn=5000, nthin=10,
                           jump_gamma=0.01, jump_w=0.1, jump_beta=0.3)

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
save(elem_lsirm_fit1, file = "elem_lsirm_fit1.RData")



## Work 2-2 : Convergence Check
##########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
load("elem_lsirm_fit1.RData")

output = elem_lsirm_fit1
nitem = ncol(output$beta)
nresp = ncol(output$theta)

# Accetance ratio
output$accept_gamma
output$accept_beta
output$accept_theta
output$accept_z
output$accept_w


# Trace plot
ts.plot(output$gamma)

set.seed(1030)
item_ind_samp = sample(nitem, 10)
resp_ind_samp = sample(nresp, 10)

par(mfrow=c(2,5))
for(ind in item_ind_samp){
  ts.plot(output$beta[, ind])
}
for(ind in resp_ind_samp){
  ts.plot(output$theta[, ind])
}

item_ind_samp = sample(nitem, 5)
resp_ind_samp = sample(nresp, 5)
for(ind in item_ind_samp){
  ts.plot(output$w[, ind, 1])
  ts.plot(output$w[, ind, 2])
}
for(ind in resp_ind_samp){
  ts.plot(output$z[, ind, 1])
  ts.plot(output$z[, ind, 2])
}



## Work 2-3 : Latent positions Check
###########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
load("elem_lsirm_fit1.RData")

plot(elem_lsirm_fit1)

# Plot only item latent positions (not respondents')
elem_lsirm_fit1_item = data.frame(w1 = rep(NA, nitem), w2 = rep(NA, nitem))
elem_lsirm_fit1_item$w1 = elem_lsirm_fit1$w_estimate[, 1]
elem_lsirm_fit1_item$w2 = elem_lsirm_fit1$w_estimate[, 2]
elem_lsirm_fit1_item$item = colnames(elem_data5)[-1]

categ = sub("^.{3}([^0-9]*)[0-9].*$", "\\1", colnames(elem_data5[, -1]))
categ[1:4] = "DR" # Student's Daily Routine
elem_lsirm_fit1_item$item_category = categ


## Plot
ggplot(data = elem_lsirm_fit1_item) +
  geom_text_repel(aes(x = w1, y = w2, label = 1:nitem), fontface=2, size = 3) +
  theme_minimal()
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/plots")
ggsave("itemmap.jpg", width=25, height=25, units=c("cm"))

## <<Color by category>>
ggplot(data = elem_lsirm_fit1_item) +
  geom_point(aes(x = w1, y = w2, color = item_category)) +
  geom_text_repel(aes(x = w1, y = w2, label = (1:nitem)), fontface=2, size = 3) +
  labs(x="w1", y="w2", fill="Item\nCategory")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 2, byrow=T))+
  theme(legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size = 10, face="bold"),
        axis.text.x = element_text(size = 10, face="bold"),
        axis.text.y = element_text(size = 10, face="bold"),
        axis.title.x = element_text(size = 10, face="bold"),
        axis.title.y = element_text(size = 10, face="bold"),
        legend.title = element_text(size=10, face="bold"))+
  ggtitle("Latent positions of Items in elementary school")

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/plots")
ggsave("itemmap_colors.jpg", width=25, height=25, units=c("cm"))



## <<Exclude items in CE category>>
ggplot(data = elem_lsirm_fit1_item[-c(27, 28), ]) +
  geom_text_repel(aes(x = w1, y = w2, label = (1:nitem)[-c(27, 28)]), fontface=2, size = 3) +
  theme_minimal()
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/plots")
ggsave("itemmap_noCE.jpg", width=25, height=25, units=c("cm"))

  
## <<Exclude items in CE category>>
## and <<Color by category>>
ggplot(data = elem_lsirm_fit1_item[-c(27, 28), ]) +
  geom_point(aes(x = w1, y = w2, color = item_category)) +
  geom_text_repel(aes(x = w1, y = w2, label = (1:nitem)[-c(27, 28)]), fontface=2, size = 3) +
  labs(x="w1", y="w2", fill="Item\nCategory")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  guides(color = guide_legend(nrow = 2, byrow=T))+
  theme(legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size = 10, face="bold"),
        axis.text.x = element_text(size = 10, face="bold"),
        axis.text.y = element_text(size = 10, face="bold"),
        axis.title.x = element_text(size = 10, face="bold"),
        axis.title.y = element_text(size = 10, face="bold"),
        legend.title = element_text(size=10, face="bold"))+
  ggtitle("Latent positions of Items in elementary school")

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/plots")
ggsave("itemmap_colors_noCE.jpg", width=25, height=25, units=c("cm"))




## Work 3-1 : Change the data structure to list
#######################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
elem_data5 = read.csv("elem_data5.csv")

elem_list_data = list()
count = 1
school_names = unique(elem_data5$C01SID)
for(name in school_names){
  elem_tmp = elem_data5[elem_data5$C01SID == name,]
  elem_tmp = as.matrix(subset(elem_tmp, select = -c(C01SID)))
  
  elem_list_data[[count]] = elem_tmp
  count = count + 1
}
head(elem_list_data[[1]])
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
save(elem_list_data, file = "elem_list_data.RData")




## Work 3-2 : LSIRM fitting for each school
########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
load(elem_list_data, file = "elem_list_data.RData")

data = elem_list_data
elem_lsirm_fit_school = list()
count = 1
for(data in elem_list_data){
  elem_lsirm_fit_school[[count]] = lsirm1pl(data)
  count = count + 1
}
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
save(elem_lsirm_fit_school, file = ("elem_lsirm_fit_school.RData"))






## Work 3-4 : Convergence Check
########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
load("elem_lsirm_fit_school.RData")
k = 1
output = elem_lsirm_fit_school[[k]]
nitem = ncol(output$beta)
nresp = ncol(output$theta)

# Trace plot
ts.plot(output$gamma)

set.seed(1030)
item_ind_samp = sample(nitem, 10)
resp_ind_samp = sample(nresp, 10)

par(mfrow=c(2,5))
for(ind in item_ind_samp){
  ts.plot(output$beta[, ind])
}
for(ind in resp_ind_samp){
  ts.plot(output$theta[, ind])
}

item_ind_samp = sample(nitem, 5)
resp_ind_samp = sample(nresp, 5)
for(ind in item_ind_samp){
  ts.plot(output$w[, ind, 1])
  ts.plot(output$w[, ind, 2])
}
for(ind in resp_ind_samp){
  ts.plot(output$z[, ind, 1])
  ts.plot(output$z[, ind, 2])
}


## Work 3-5 : Latent positions Check
########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
load("elem_lsirm_fit_school.RData")

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/plots")
count = 1
for(data in elem_lsirm_fit_school){
  
  file_name = paste0("school", count, ".jpg")
  df_w = data.frame(w1 = data$w_estimate[, 1], w2 = data$w_estimate[, 2])

  ggplot(df_w) + 
    geom_point(mapping = aes(w1, w2), color = "red") +
    geom_text(aes(x = w1, y = w2, label = 1:nrow(df_w)))
    ggtitle(paste0("Item Latent Positions in school", count))

  ggsave(file_name, width = 20, height = 15, units = c("cm"))
  count = count + 1
}






## Work : Change the data structure to list (middle school data)
#######################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
mid_data5 = read.csv("mid_data5.csv")

mid_list_data = list()
count = 1
school_names = unique(mid_data5$C01SID)
for(name in school_names){
  mid_tmp = mid_data5[mid_data5$C01SID == name,]
  mid_tmp = as.matrix(subset(mid_tmp, select = -c(C01SID)))
  
  mid_list_data[[count]] = mid_tmp
  count = count + 1
}
head(mid_list_data[[1]])
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
save(mid_list_data, file = "mid_list_data.RData")




