### Compute similarity matrix -> item_dist_elem, Jaccard_elem, Jaccard_mid
### Spectral Clustering -> elem_SP1_group(based on dist),  elem_SP1_group(based on Jaccard)
### Binary PCA with BSVD -> 
### -> "/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/BSVD_fit/bsvd_elem_fit.RData"
### -> "/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/BSVD_fit/bsvd_mid_fit.RData"

#########################################################################################################



#### Setting
# Load the fitted lsirm data
load("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/elem_lsirm_fit/elem_lsirm_fit1027.RData")

# Load the elementary school data


# Load the school data
elem_data = read.csv("/Users/seoyoung/Desktop/Team5/elem_data1026.csv")
# elem_data = read.csv("/Users/seoyoung/Desktop/Team5/elem_data1107.csv")
elem_data = elem_data[,-1]
mid_data = read.csv("/Users/seoyoung/Desktop/Team5/Incheon_project/data3_mid.csv")

head(elem_data)
head(mid_data)

elem_data4 = read.csv("/Users/seoyoung/Desktop/Team5/elem_data1026.csv")
# elem_data4 = read.csv("/Users/seoyoung/Desktop/Team5/elem_data1107.csv")
elem_lsirm_data = as.matrix(subset(elem_data4, select = -c(C01SID)))
elem_lsirm_data2 = as.matrix(subset(elem_data4, select = -c(C01SID, C01CE1, C01CE2, C01DH06)))
elem_lsirm_data3 = as.matrix(subset(elem_data4, select = -c(C01SID, C01CE1, C01CE2, C01DH06, C01SIV01, C01SIV02, C01SIV03)))

item_names = cbind(1:length(colnames(elem_data)), colnames(elem_data))
item_names2 = cbind(1:length(colnames(elem_lsirm_data2)), colnames(elem_lsirm_data2))
item_names3 = cbind(1:length(colnames(elem_lsirm_data3)), colnames(elem_lsirm_data3))

write.csv(item_names, "/Users/seoyoung/Desktop/Team5/Incheon_project/item_list.csv")


# Compute similarity matrix based on item distance
item_dist_elem = matrix(0, nrow = dim(elem_data)[2], ncol = dim(elem_data)[2]) 
for(i in 1:dim(elem_data)[2]){
  for(j in 1:dim(elem_data)[2]){
    item_dist_elem[i, j] = 1/sqrt((elem_lsirm_fit$w_estimate[i, 1] - elem_lsirm_fit$w_estimate[j, 1])^2 + 
                                   (elem_lsirm_fit$w_estimate[i, 2] - elem_lsirm_fit$w_estimate[j, 2])^2)
  }
}
diag(item_dist_elem) = 1



# Compute the Jaccard index
elem_data = elem_lsirm_data2
nrow_elem = nrow(elem_data)
ncol_elem = ncol(elem_data)
nrow_mid = nrow(mid_data)
ncol_mid = ncol(mid_data)

Jaccard_elem = matrix(0, nrow = ncol_elem, ncol = ncol_elem)
Jaccard_mid = matrix(0, nrow = ncol_mid, ncol = ncol_mid)

for(i in 1:ncol_elem){
  for(j in 1:ncol_elem){
    Jaccard_elem[i, j] = sum(elem_data[,i] == elem_data[,j])/nrow_elem
  }
}

for(i in 1:ncol_mid){
  for(j in 1:ncol_mid){
    Jaccard_mid[i, j] = sum(mid_data[,i] == mid_data[,j])/nrow_mid
  }
}





## 1. Spectral clustering
library(kernlab)
# library(skmeans)
spec_clust(elem_lsirm_fit, k=3)
spec_clust(elem_lsirm_fit, k=4)
spec_clust(elem_lsirm_fit, k=5)
spec_clust(elem_lsirm_fit, k=6)
spec_clust(elem_lsirm_fit, k=7)


spec_clust(elem_lsirm_fit2, k=3)
spec_clust(elem_lsirm_fit2, k=4)
spec_clust(elem_lsirm_fit2, k=5)
spec_clust(elem_lsirm_fit2, k=6)
spec_clust(elem_lsirm_fit2, k=7)


# Group membership dataframe
ngroup = 3:10     # the number of clusters
elem_items = colnames(elem_data)
elem_SP1_group = data.frame(row.names = colnames(elem_data))    # based on lsirm distance
elem_SP2_group = data.frame(row.names = colnames(elem_data))    # based on Jaccard index

for(k in ngroup){
  elem_SP1_group[, paste0("k",k)] = specc(item_dist_elem, centers = k)@.Data
}
for(k in ngroup){
  elem_SP2_group[, paste0("k",k)] = specc(Jaccard_elem, centers = k)@.Data
}


# Result check
attach(elem_SP2_group)
for(k in ngroup){
  cat("For ngroup = ", k,",", "\n")
  for(i in 1:k){
    cat("group",i,":",  rownames(elem_SP2_group)[get(paste0("k", k)) == i], "\n")
  }
  cat("\n")
}
detach(elem_SP2_group)
elem_SP1_group


## 2. Laplacian matrix
library(matrixLaplacian)
library(Matrix)
library(reshape2)


# Adjacency matrix 



# # Compute laplacian matrix
# adjacency_mat = matrix(0, nrow=nrow(elem_data)+ncol(elem_data), ncol=nrow(elem_data)+ncol(elem_data))
# 
# for(i in 1:nrow(elem_data)){
#   for(j in (nrow(elem_data)+1):(nrow(elem_data)+ncol(elem_data))){
#     adjacency_mat[i, j] = ifelse(elem_data[i, j-nrow(elem_data)]==1, 1, 0)
#     adjacency_mat[j, i] = adjacency_mat[i, j]
#   }
# }
# adjacency_mat[1:10, 1976:1985]
# dim(adjacency_mat)
# 
# degree_matrix = matrix(0, nrow = nrow(adjacency_mat), ncol = ncol(adjacency_mat))
# diag(degree_matrix) = rowSums(adjacency_mat)
# 
# elem_laplacian_mat = degree_matrix - adjacency_mat
# 
# # Compute eigenvalue and eigenvector
# elem_eigen = eigen(elem_laplacian_mat)
# elem_eigenvalues = elem_eigen$values
# elem_eigenvectors = elem_eigen$vectors
# 
# print(cat("eigenvalues : ", elem_eigenvalues))
# 
# eigen_df = data.frame(x=1:length(elem_eigenvalues), y=elem_eigenvalues)
# ggplot(eigen_df[1:100,]) +
#   geom_point(aes(x=x, y=y), color="red") +
#   geom_line(aes(x=x, y=y)) + 
#   ggtitle("eigenvalue of laplacian matrix")



data3_elem_affinity_matrix_edl = item_dist_elem

## 대현오빠 코드
# Compute laplacian matrix
degree_matrix_elem_edl = Matrix(0, nrow = nrow(data3_elem_affinity_matrix_edl), ncol = ncol(data3_elem_affinity_matrix_edl))
diag(degree_matrix_elem_edl) = rowSums(data3_elem_affinity_matrix_edl)
data3_elem_laplacian_mat_affinity_edl = degree_matrix_elem_edl - data3_elem_affinity_matrix_edl


# Calculate the eigenvalues of the Laplacian matrix
eigenvalues_elem_edl <- eigen(data3_elem_laplacian_mat_affinity_edl, symmetric = TRUE)$values

# Sort the eigenvalues in ascending order
eigenvalues_sorted_elem_edl <- sort(eigenvalues_elem_edl)

# Calculate the eigengap (difference between consecutive eigenvalues)
eigengap_elem_edl <- diff(eigenvalues_sorted_elem_edl)

# Create a data frame for plotting
plot_data_elem_edl <- data.frame(
  EigenvalueIndex_elem_edl = 1:(length(eigengap_elem_edl)),
  Eigengap_elem_edl = eigengap_elem_edl
)
png(filename="data3_eigengap_plot_elem_edl.png",width=900,height=900,unit="px",bg="transparent")
# Plot the eigengap
ggplot(plot_data_elem_edl, aes(x = EigenvalueIndex_elem_edl, y = Eigengap_elem_edl)) +
  geom_line() +
  geom_point() +
  labs(title = "Eigengap Plot_elem_edl", x = "Eigenvalue Index_elem_edl", y = "Eigengap_elem_edl") +
  theme_minimal()
dev.off()











## 4. Binary PCA

# Compute the Jaccard index
nrow_elem = nrow(elem_data)
ncol_elem = ncol(elem_data)
nrow_mid = nrow(mid_data)
ncol_mid = ncol(mid_data)

Jaccard_elem = matrix(0, nrow = ncol_elem, ncol = ncol_elem)
Jaccard_mid = matrix(0, nrow = ncol_mid, ncol = ncol_mid)

for(i in 1:ncol_elem){
  for(j in 1:ncol_elem){
    Jaccard_elem[i, j] = sum(elem_data[,i] == elem_data[,j])/nrow_elem
  }
}

for(i in 1:ncol_mid){
  for(j in 1:ncol_mid){
    Jaccard_mid[i, j] = sum(mid_data[,i] == mid_data[,j])/nrow_mid
  }
}



# BSVD
library(Rcpp)
library(RcppArmadillo)
sourceCpp("/Users/seoyoung/Desktop/Meeting/My_work/My_Implementation/BSVD/bsvd_fixed1020.cpp")
Y = Jaccard_elem
# Y = Jaccard_mid
Y = elem_lsirm_data
svd_Y = svd(x=Y)

m<-dim(Y)[1]
n<-dim(Y)[2]

s20.est = var(c(Y))
t20.est = 0
mu0.est = 0
for(k in 1:n) { 
  ks = seq(1, k, length=k)
  s20.est = c(s20.est, 
              var(c((Y-svd_Y$u[,ks]%*%diag(svd_Y$d[ks],nrow=k)%*%t(svd_Y$v[,ks]) )) ) )
  t20.est = c(t20.est, var(svd_Y$d[ks])  )
  mu0.est = c(mu0.est, mean(svd_Y$d[ks]) )
}
t20.est[2] = 0

## prior for phi
nu0 = 2
s20 = mean(s20.est)

## prior for psi
eta0 = 2
t20 = mean(t20.est)

## prior for mu
mu0 = mean(mu0.est)
premu0 = 1/var(mu0.est)

## 
nrank = 5

K0 = nrank
U = matrix(0, m, n)
V = matrix(0, n, n) 
U[,seq(1, K0, length=K0)]<-svd_Y$u[,seq(1, K0, length=K0)] 
V[,seq(1, K0, length=K0)]<-svd_Y$v[,seq(1, K0, length=K0)] 
D<-diag( c(svd_Y$d[seq(1, K0, length=K0)],rep(0, n-K0))) 

# 
niter = 20000; nburn = 5000; nthin = 5; nprint = 2000; 
# niter = 1000; nburn = 500; nthin = 2; nprint = 100; 

bsvd_elem_fit = BSVD_fixed(Y = Y, U = U, V = V, D = D, nrank = 5, 
                           niter = niter, nburn = nburn, nthin = nthin, nprint = nprint,
                           premu0 = premu0, mu0 = mu0, nu0 = nu0, t20 = t20, eta0 = eta0, s20 = s20)

bsvd_mid_fit = BSVD_fixed(Y = Y, U = U, V = V, D = D, nrank = 5, 
                           niter = niter, nburn = nburn, nthin = nthin, nprint = nprint,
                           premu0 = premu0, mu0 = mu0, nu0 = nu0, t20 = t20, eta0 = eta0, s20 = s20)



save(bsvd_elem_fit, file="/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/BSVD_fit/bsvd_elem_fit1106.RData")
# save(bsvd_mid_fit, file="/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/BSVD_fit/bsvd_mid_fit1028.RData")
# 
# load("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/BSVD_fit/bsvd_elem_fit1027.RData")
# load("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/BSVD_fit/bsvd_elem_fit1027.RData")

## load BSVD result
# load("/Users/seoyoung/Desktop/Team5/Incheon_project/fit_rdata/BSVD_fit/bsvd_elem_fit.RData")


## Trace plot
fit_tmp = bsvd_mid_fit
fit_tmp = bsvd_elem_fit

par(mfrow=c(2, 3))
plot(fit_tmp$mu, type="l")
plot(1/fit_tmp$phi, type="l")
plot(fit_tmp$psi, type="l")
plot(fit_tmp$D[,1], type='l')
plot(fit_tmp$D[,2], type='l')
plot(fit_tmp$D[,3], type='l')

plot(fit_tmp$U[,1,1], type='l')
plot(fit_tmp$U[,2,3], type='l')
plot(fit_tmp$U[,3,5], type='l')
plot(fit_tmp$V[,2,1], type='l')
plot(fit_tmp$V[,4,2], type='l')
plot(fit_tmp$V[,5,5], type='l')


# eigenvalues
fit_tmp$D[which.max(fit_tmp$map), 1]
fit_tmp$D[which.max(fit_tmp$map), 2]
fit_tmp$D[which.max(fit_tmp$map), 3]
fit_tmp$D[which.max(fit_tmp$map), 4]
fit_tmp$D[which.max(fit_tmp$map), 5]



# eigenvector plot
library(ggplot2)
library(ggrepel)
PCA_vec_elem = data.frame(item = colnames(elem_lsirm_data2), v1 = fit_tmp$V[which.max(fit_tmp$map), ,1], v2=fit_tmp$V[which.max(fit_tmp$map), ,2])
PCA_vec_mid = data.frame(item = colnames(mid_data), v1 = fit_tmp$V[which.max(fit_tmp$map), ,1], v2=fit_tmp$V[which.max(fit_tmp$map), ,2])

ggplot(data=PCA_vec_elem) +
  geom_point(aes(x=v1, y=v2), color="red") +
  geom_text_repel(aes(x=v1, y=v2, label = item), size=3) + 
  ggtitle("Eigenvector of items for elementary school")

ggplot(data=PCA_vec_mid) +
  geom_point(aes(x=v1, y=v2), color="red") +
  geom_text_repel(aes(x=v1, y=v2, label = item), size=3) + 
  ggtitle("Eigenvector of items for middle school")





## Binary PCA -> Spectral Clustering
elem_PCA_tmp = list()
elem_PCA_tmp$w_estimate = fit_tmp$V[which.max(fit_tmp$map), ,1:2]
elem_PCA_tmp$z_estimate = fit_tmp$U[which.max(fit_tmp$map), ,1:2]

spec_clust(elem_PCA_tmp, k=3)
spec_clust(elem_PCA_tmp, k=4)
spec_clust(elem_PCA_tmp, k=5)
spec_clust(elem_PCA_tmp, k=6)
spec_clust(elem_PCA_tmp, k=7)



mid_PCA_tmp = list()
mid_PCA_tmp$w_estimate = fit_tmp$V[which.max(fit_tmp$map), ,1:2]
mid_PCA_tmp$z_estimate = fit_tmp$U[which.max(fit_tmp$map), ,1:2]

spec_clust(mid_PCA_tmp, k=3)
spec_clust(mid_PCA_tmp, k=4)
spec_clust(mid_PCA_tmp, k=5)
spec_clust(mid_PCA_tmp, k=6)
spec_clust(mid_PCA_tmp, k=7)





ggplot(data=elem_projected_data) +
  geom_point(aes(x=V1, y=V2), color="red") +
  geom_text_repel(aes(x=V1, y=V2, label = item), size=3) +
  ggtitle("Rotated eigenvector of items for elementary school")










## binary PCA -> covariate 으로 regression
elem_new_axis = fit_tmp$V[which.max(fit_tmp$map), ,1:2]

covariate_var = c("C01SEX","C01STDTc", "C01PTTc", "C01BKTc", "C01ACVTc", "P01FINCM", "P01FJOB", "P01MJOB")
elem_covariate_data = subset(elem_data3, select = covariate_var)
elem_x_reg = lm(elem_new_axis[,1] ~ elem_covariate_data)
dim(elem_new_axis)
dim(elem_covariate_data)
