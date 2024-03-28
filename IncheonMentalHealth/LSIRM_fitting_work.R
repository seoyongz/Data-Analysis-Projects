library(lsirm12pl)


## Data load
##########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
elem_data4 = read.csv("elem_data4.csv")


# Work1 : Variable Select -> elem_data5 save
##########################################################################
covariate_var = c("C01SEX","C01STDTc", "P01FINCM", "P01FJOB", "P01MJOB")
item_names = colnames(elem_data4)

## Get variable names by item category
selected_items = c("C01SID", "C01STDTc", "C01PTTc", "C01BKTc", "C01ACVTc")
item_category = c("SH", "DH", "CE", "SIV", "SAD", "HP", "FD", "ST", "EM", "SS", "SPD")
for(x in item_category){
  selected_items = c(selected_items, grep(paste0("^C01", x, "[0-9]+$"), item_names, value = T))
}
selected_items = c(selected_items, "C01DGT01c", "C01DGT02c")
selected_items
elem_data5 = subset(elem_data4, select = selected_items)

head(elem_data5)

sum(is.na(elem_data5))  ## no missing check
dim(elem_data5)   ## 2207 x 87

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
write.csv(elem_data5, "elem_data5.csv", row.names=F)



## Work 2-1 : LSIRM fitting
##########################################################################
elem_lsirm_fit1 = lsirm1pl(data = elem_data5[, -1])

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
save(elem_lsirm_fit1, file = "elem_lsirm_fit1_1027.RData")



## Work 2-2 : Model Convergence Check
##########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
load("elem_lsirm_fit1_1027.RData")

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
load("elem_lsirm_fit1_1027.RData")

plot(elem_lsirm_fit1)

# Plot only item latent positions (not respondents')
elem_lsirm_fit1_item = data.frame()
elem_lsirm_fit1_item$w1 = elem_lsirm_fit1$w_estimate[, 1]
elem_lsirm_fit1_item$w2 = elem_lsirm_fit1$w_estimate[, 2]
elem_lsirm_fit1_item$item = colnames(elem_data5)
categories = sub("^.{3}([^0-9]*)[0-9].*$", "\\1", colnames(elem_data5[, -1]))
elem_lsirm_fit1_item$item_category = categories


ggplot(data = elem_lsirm_fit1_item) +
  geom_point(aes(x = w1, y = w2, color = item_category),) +
  geom_text_repel(aes(x = w1, y = w2, label = item), fontface=2, size = 3) +
  theme_minimal()+
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 6, byrow=T))+
  theme(legend.key.size = unit(1.0, "cm"), 
        legend.text = element_text(size = 12, face="bold"),
        axis.text.x = element_text(size = 15, face="bold"),
        axis.text.y = element_text(size = 15, face="bold"),
        axis.title.x = element_text(size = 20, face="bold"),
        axis.title.y = element_text(size = 20, face="bold"),
        legend.title = element_text(size=15, face="bold"))

setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/plots")
ggsave("Latent positions of Items in elementary school")




ngroup = 5
spec_clust(elem_lsirm_fit1107, k=ngroup)




## Work 3-1 : Change the data structure to list
#######################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/csv_data")
elem_data5 = read.csv("elem_data5.csv")

elem_list_data = list()
count = 1
for(name in unique(elem_data5$C01SID)){
  elem_tmp = elem_data5[elem_data5$C01SID == name,]
  elem_tmp = as.matrix(subset(elem_tmp, select = -c(C01SID)))
  elem_list_data[[count]] = elem_tmp
  count = count + 1
}
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
save(elem_list_data, file = "elem_list_data1031.RData")




## Work 3-2 : LSIRM fitting for each school
########################################################################
setwd("/Users/seoyoung/Desktop/IncheonMentalHelath/RData")
load(elem_list_data, file = "elem_list_data1031.RData")

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




