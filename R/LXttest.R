
LXttest <- function(data){

all_packages <- data.frame(installed.packages())

pack <- data.frame(c("openxlsx","dplyr"))

bioc_pack <- data.frame(c("limma"))

pack$type <- pack[,1] %in% all_packages$Package

for (i in 1:nrow(pack)){if (!requireNamespace(pack[i,1], quietly=TRUE))
  install.packages(pack[i,1],update = F,ask = F)}
rm(i)

for (i in 1:nrow(bioc_pack)){if (!requireNamespace(bioc_pack[i,1], quietly=TRUE))
  BiocManager::install (bioc_pack[i,1],update = F,ask = F) }

rm(i)

packages <- c(pack[,1],bioc_pack[,1])

for(i in packages){
  library(i, character.only = T)}

rm(i)

#-------------------------------------------------------------
  rm(list=ls())
  df0 <- read.xlsx(data)
  table(duplicated(df0[,1]))

  df <- limma::avereps(df0[,-1],df0[,1]) %>% data.frame() # 对重复的项，取其平均值，同时也有去重功能

  group <- colnames(df)
  group <- gsub("\\d+$","",group)  #去掉字符串后面的数字
  table <- data.frame(table(group))
  group <- distinct(data.frame(group))
  group <- dplyr::left_join(group,table,"group")

  df$mean1 <- NA #添加列名
  df$sd1 <- NA

  df$mean2 <- NA
  df$sd2 <- NA

  df$FoldChange <- NA
  df$pvalue <- NA

  colnames(df)[ncol(df)-5] <- paste0(group[1,1],"_mean") #改列名
  colnames(df)[ncol(df)-4] <- paste0(group[1,1],"_sd")

  colnames(df)[ncol(df)-3] <- paste0(group[2,1],"_mean")
  colnames(df)[ncol(df)-2] <- paste0(group[2,1],"_sd")

  df[ncol(df)-5] <- rowMeans(df[,c(1:group[1,2])]) # 计算row的平均值

  n1 <- ncol(df)-group[2,2]-6+1 # group2起始列
  n2 <- group[1,2]+group[2,2] %>% as.numeric() # group2起末列
  df[ncol(df)-3] <- rowMeans(df[,c(n1:n2)])

  df[ncol(df)-1] <- df[ncol(df)-3]/df[ncol(df)-5] # 计算FoldChange


  #计算sd和p值
  n1_sd <- ncol(df)-4 # 第一组sd所在的列
  n2_sd <- ncol(df)-2 # 第二组sd所在的列

  for(i in 1:nrow(df)){
    df[i,n1_sd] <- sd(df[i,c(1:group[1,2])]) # 计算第一组sd
    df[i,n2_sd] <- sd(df[i,c(n1:n2)]) # 计算第一组sd

    t_test <- t.test(df[i,c(1:group[1,2])],df[i,c(n1:n2)],
                     paired = FALSE,
                     var.equal = T, # 方差齐 （如果方差不齐 var.equal = F）
                     conf.level = 0.95)

    df[i,ncol(df)] <- t_test[["p.value"]]

    }

  write.xlsx(df,"t.test_result.xlsx",rowNames=T)

  print("The t test was successfully analyzed, and please see the xlsx file t.test_result")

}











