
LXttest <- function(data){

all_packages <- data.frame(installed.packages())

pack <- data.frame(c("openxlsx","dplyr","nortest","car"))

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
  if(dir.exists("analysis result")==F)
  dir.create("analysis result")

  df0 <- read.xlsx(data)

#去掉数据全部为0的行
  df0$sum <- rowSums(df0[,c(2:ncol(df0))])
  df0 <- dplyr::filter(df0,sum>0)
  df0 <- df0[,-ncol(df0)]

  #查看重复
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

  equal_variance <- df
  equal_variance$equal_variance <- NA
  equal_variance$normality_test <- NA

 for(i in 1:nrow(df)){
    df[i,n1_sd] <- sd(df[i,c(1:group[1,2])]) # 计算第一组sd
    df[i,n2_sd] <- sd(df[i,c(n1:n2)]) # 计算第二组sd

   test_group <- c(rep(group[1,1],group[1,2]),rep(group[2,1],group[2,2]))
   test_data <- df[i,c(1:n2)] %>% as.numeric()

 # 正态检验(SPSS 规定: 当样本含量3 ≤ n ≤ 5000时, 结果以Shapiro-Wilk为准, 当样本含量n > 5000结果以Kolmogorov-Smirnov为准)
   n <- table(test_data) %>% sum() %>% as.numeric()

   if(n<=5000){
   normality_test <- shapiro.test(test_data)  # shapiro.test(x)；x为数据，长度为3-5000。
   norm.test.p <- normality_test[["p.value"]]} else
        {normality_test <- lillie.test(test_data) #nortest包的lillie.test函数:(Kolmogorov-Smirnov)正态性检验
        norm.test.p <- normality_test[["p.value"]]}

 # 方差齐性分析
   #对于正态分布的样本，Bartlette检验极其灵敏，但是对于非正态分布的样本，检验非常不准确；
   #equal_var <- bartlett.test(test_data,test_group)
   #equal.var.p <- equal_var[["p.value"]]

   #Levene检验是一种更为稳健的检验方法，既可用于正态分布的样本，也可用于非正态分布的样本，
   #同时对比较的各组样本量可以相等或不等；是sPSs的黑默t认方差齐性检验方法。
   equal_var <- leveneTest(test_data~factor(test_group))
   equal.var.p <- equal_var[1,3]

   #前者是对原始数据的方差进行检验的，leveneTest是对方差模型的残差进行组间齐性检验.
   #一般认为是要求残差的方差齐，所以一般的统计软件都做的是leveneTest

   if(equal.var.p>0.05){
   t_test <- t.test(df[i,c(1:group[1,2])],df[i,c(n1:n2)],
                     paired = FALSE,
                     var.equal = T, # 方差齐 var.equal = T
                     conf.level = 0.95) } else
   {t_test <- t.test(df[i,c(1:group[1,2])],df[i,c(n1:n2)],
                     paired = FALSE,
                     var.equal = F, # 方差不齐 var.equal = F
                    conf.level = 0.95)}

  df[i,ncol(df)] <- t_test[["p.value"]]

  equal_variance[i,ncol(equal_variance)] <- norm.test.p
  equal_variance[i,ncol(equal_variance)-1] <- equal.var.p
  equal_variance[i,ncol(equal_variance)-2] <- t_test[["p.value"]]
  equal_variance[i,ncol(equal_variance)-4] <- sd(df[i,c(n1:n2)])
  equal_variance[i,ncol(equal_variance)-6] <- sd(df[i,c(1:group[1,2])])

  }


 df$p.adjust <- p.adjust(df[,ncol(df)],method = "BH")

 write.xlsx(df,"analysis result/t.test_result.xlsx",rowNames=T)
 write.xlsx(equal_variance,"analysis result/equal_variance_analysis(方差齐性分析).xlsx",rowNames=T)

 print("The t test was successfully analyzed, which can be seen in the fold of analysis result")
 print("equal_variance_analysis(方差齐性分析)表中: equal_variance 方差齐性检验; normality_test 正态分布检验")

}











