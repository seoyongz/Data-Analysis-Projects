library(dplyr)
library(kernlab)
library(ggplot2)

spec_clust = function(result, k=5){
  item_position = result$w_estimate
  resp_position = result$z_estimate
  notation = c('w','z')
  item_scaled <- item_position %>% scale()  
  
  spectral_result <- specc(as.matrix(item_position), centers = k) #kernlab package
  spectral_result$tot.withinss
  scenters = spectral_result@centers
  spectral_result <- data.frame(cbind(group = as.numeric(spectral_result),
                                      item = 1:nrow(item_position)))
  
  
  rad = c()
  for(i in 1:k){
    inds = spectral_result[spectral_result$group == i,c("item")]
    d = dist(rbind(scenters[i,],item_position[inds,]))
    rad[i] = max(as.matrix(d)[,1])  
  }
  
  rad = data.frame(cbind(rad,scenters))
  colnames(rad) = c("rad","cx","cy")
  
  clust <- data.frame(
    spectral_result %>%
      group_by(group) %>%
      summarise(items = paste(item, collapse = ", "))
  )
  
  print(clust)
  
  df1=as.data.frame(item_position)
  df2=as.data.frame(resp_position)
  df1[,3]=notation[1]
  df2[,3]=notation[2]
  
  colnames(df1)=c('x','y','source')
  colnames(df2)=c('x','y','source')
  
  
  df=rbind(df2,df1)
  colnames(df)=c('axis1','axis2','source')
  
  max_coordinate = sapply(df[,c(1,2)], max, na.rm = TRUE)
  min_coordinate = sapply(df[,c(1,2)], min, na.rm = TRUE)
  axis_value = max(abs(c(max_coordinate,min_coordinate)))
  axis_range = c(-axis_value,axis_value)*1.1
  
  
  g_fin <- cbind(df1, spectral_result)
  ind <- max(spectral_result$group)
  
  if(ind > length(LETTERS)){
    addlabel = LETTERS
    for(U in 1:length(LETTERS)){
      for(l in 1:length(letters)){
        addlabel = c(addlabel, paste0(LETTERS[U],letters[l]))
      }
    }
    alphabet = addlabel[1:ind]
  } else{
    alphabet = LETTERS[1:ind]
  }
  ggcolor = rainbow(ind, s=.6, v=.9)[sample(1:ind, ind)]
  temp <- (ggplot(data=df, aes(x, y)) +
             geom_point(data=df2, aes(x, y), col="grey", cex=1.0) +
             geom_text(data=g_fin, aes(x, y), label=g_fin$item,
                       color=ggcolor[g_fin$group], cex=4, fontface="bold") + # number of item
             #geom_circle(aes(x0=cx,y0=cy,r=rad),data=rad,inherit.aes = F) +
             theme_bw() +
             theme(plot.margin = unit(c(1,1,1,1), "cm"),
                   axis.text=element_text(size=16),
                   axis.title=element_text(size=14,face="bold"),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   legend.title=element_blank(),
                   legend.position = c(0.9,0.9),
                   legend.text = element_text(size=16),
                   plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+
             ggtitle("Interaction Map"))
  
  return(list(p = temp,
              clust = clust))
}
