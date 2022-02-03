# install.packages("tidyverse")
# install.packages("rstatix")
# install.packages("gt")
# install.packages("naniar")
# install.packages("ggpubr")
# install.packages("qqplotr")
# install.packages("corrplot")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("pROC")
# install.packages("gridExtra")
# install.packages("lattice")
# install.packages("woeBinning")
# install.packages("blorr")
# install.packages("MASS")
# install.packages("xgboost")
# install.packages("woeBinning")
# install.packages("reshape2")
# install.packages("fBasics")
# install.packages("OneR")
# install.packages("randomForest")
# install.packages("mlbench")
# install.packages("caret")
# install.packages("doParallel")
# install.packages("stargazer")
# install.packages("pastecs")
# install.packages("library(scales)
# install.packages(janitor)


library(pastecs)
library(tidyverse)
library(rstatix)
library(gt)
library(scales)
library(naniar)
library(ggpubr)
library(janitor)
library(qqplotr)
library(corrplot)
library(rpart)
library(rpart.plot)
library(pROC)
library(gridExtra)
library(lattice)
library(woeBinning)
library(blorr)
library(MASS)
library(xgboost)
library(woeBinning)
library(reshape2)
library(fBasics)
library(OneR)
library(randomForest)
library(mlbench)
library(caret)
library(doParallel)
library(stargazer)




path <- "/Users/ryanlee/code/NorthwesternMSDS/MSDS498 - Capstone Modelling/Capstone/data/"
data <- paste(path,'credit_card_default.RData',sep='');

# read in data, and check missing data
raw.data <- readRDS(data)
is.null(raw.data)
str(raw.data)
summary(raw.data)
missing.data.counts <- as.data.frame(sapply(raw.data, function(x) sum(is.na(x)))) 
gt(missing.data.counts)
naniar::vis_miss(raw.data)

# get summary statistics table for variables part 1 for graph ouyput
res1 <- stat.desc(raw.data[2:14])
res1 <- as.data.frame(res1)
res1 <- round(res1, 2)
row.names.remove<- c("nbr.null")
res1 <- res1[!(row.names(res1) %in% row.names.remove), ]
Analysis <- rownames(res1)
res1 <- cbind(Analysis,res1)
gt(res1)
# get summary statistics table for variables part 2 for graph output

res2 <- stat.desc(raw.data[15:25])
res2 <- as.data.frame(res2)
res2 <- round(res2, 2)
res2 <- res2[!(row.names(res2) %in% row.names.remove), ]
Analysis <- rownames(res2)
res2 <- cbind(Analysis,res2)
gt(res2)
#####################################################################
# Data Wrangling
####################################################################
#set discrete and numeric/continous variables
discrete <- c(raw.data$DEFAULT,raw.data$SEX,raw.data$EDUCATION,raw.data$MARRIAGE)
continous <- c(raw.data$LIMIT_BAL,raw.data$BILL_AMT1,raw.data$BILL_AMT2,raw.data$BILL_AMT3,raw.data$BILL_AMT4,
               raw.data$BILL_AMT5,raw.data$BILL_AMT6,raw.data$PAY_AMT1,raw.data$PAY_AMT2,
               raw.data$PAY_AMT3,raw.data$PAY_AMT4,raw.data$PAY_AMT5.raw.data$PAY_AMT6)

discrete <- c("DEFAULT","SEX","EDUCATION","MARRIAGE")

discrete.table <-raw.data %>% 
  subset(select = discrete)

continous <- c("LIMIT_BAL","BILL_AMT1","BILL_AMT2","BILL_AMT3","BILL_AMT4",
               "BILL_AMT5","BILL_AMT6","PAY_AMT1","PAY_AMT2",
               "PAY_AMT3","PAY_AMT4","PAY_AMT5","PAY_AMT6")

continous.table <-raw.data %>% 
  subset(select = continous)

ggplot(data=raw.data, aes(x=DEFAULT)) +
  geom_bar(stat="count")


raw.data.sex <- mutate(SEX = ifelse())


s <- ggplot(raw.data, aes(x=as.factor(SEX), fill= as.factor(DEFAULT))) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) +
  xlab("SEX - 1=Male, 2=Female") +
  labs(title = "Count Plot SEX", fill = "DEFAULT") +
  ylab("Percent")


d <- ggplot(raw.data, aes(x=as.factor(DEFAULT), fill= as.factor(DEFAULT))) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) +
  xlab("DEFAULT") +
  labs(title = "Count Plot DEFAULT", fill = "DEFAULT") +
ylab("Percent")


m <- ggplot(raw.data, aes(x=as.factor(MARRIAGE), fill= as.factor(DEFAULT))) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) +
  xlab("MARRIAGE - 1=Married, 2=Single, 3=Other") +
  labs(title = "Count Plot MARRIAGE", fill = "DEFAULT")+
  ylab("Percent")

e <- ggplot(raw.data, aes(x=as.factor(EDUCATION), fill= as.factor(DEFAULT))) +
  geom_bar(aes(y = (..count..)/sum(..count..)))+
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) +
  xlab("EDUCATION - 1=Graduate School, 2=University, 3=High School, 4= Other") +
  labs(title = "Count Plot EDUCATION", fill = "DEFAULT")+
  ylab("Percent")


a <- ggplot(raw.data, aes(x=AGE, fill= as.factor(DEFAULT))) +
  stat_bin(binwidth=10) + 
  stat_bin(binwidth=10, geom="text", aes(label=round((..count..)/sum(..count..)*100,2)),
           vjust=-1.5) +
  labs(title = "Count Plot AGE", fill = "DEFAULT") +
  xlab("AGE bin")

sum.plot <- ggarrange(d,s,m,e,a)
# Sets the layout of the graphs
(layout_matrix <- matrix(c(1,1,1,1,1,1,2,2,2,2,2,2, 3,3,3,3,3,3,6,6,6,6,4,4,4,4,4,4,4, 4,5,5,5,5,5,5,5,5,5,5,6,6), nrow = 2, byrow = TRUE))
#      [,1] [,2] [,3] [,4]
# [1,]    1    1    2    2
# [2,]    4    3    3    4


grid.arrange(d, s, m, e, a, layout_matrix = layout_matrix)
# Check duplicates 
raw.data %>% 
  get_dupes()

# check counts of each variable

get_class_counts <- function(var,df) {
  frame <- df[var]
  frame %>% 
    count(df[var]) %>% 
    as_tibble() %>% 
    mutate(Percent = n / sum(n)*100) %>% 
    gt()
}
get_class_counts("DEFAULT", raw.data)
get_class_counts("SEX", raw.data)
get_class_counts("EDUCATION", raw.data)
get_class_counts("PAY_0", raw.data)
get_class_counts("PAY_2", raw.data)
get_class_counts("PAY_3", raw.data)
get_class_counts("PAY_4", raw.data)
get_class_counts("PAY_5", raw.data)
get_class_counts("PAY_6", raw.data)
get_class_counts("MARRIAGE", raw.data)


#density plot of limit_balance
ggplot(raw.data, aes(x=LIMIT_BAL)) +
  geom_density(fill = "blue")


get_dist <- function(df,var) {
  
b <- ggplot(data = df, mapping = aes_string(sample = var)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

a <- ggplot(df) +
  geom_histogram(aes_string(x = var))
c <- ggplot(df) +
  geom_boxplot(aes_string(x = var))
fig <-ggarrange(a,ggarrange(b,c,nrow = 2))
return(fig)
}


get_dist(raw.data,"LIMIT_BAL")
bill <- get_dist(raw.data,"BILL_AMT1")
bill2 <- get_dist(raw.data,"BILL_AMT2")
bill3 <- get_dist(raw.data,"BILL_AMT3")
bill4 <- get_dist(raw.data,"BILL_AMT4")
bill5 <- get_dist(raw.data,"BILL_AMT5")
bill6 <- get_dist(raw.data,"BILL_AMT6")
PAY <- get_dist(raw.data,"PAY_AMT1")
PAY2 <- get_dist(raw.data,"PAY_AMT2")
PAY3 <-get_dist(raw.data,"PAY_AMT3")
PAY4 <-get_dist(raw.data,"PAY_AMT4")
PAY5 <-get_dist(raw.data,"PAY_AMT5")
PAY6 <-get_dist(raw.data,"PAY_AMT6")



ggarrange(bill, bill2,bill3,bill4,bill5,bill6)
ggarrange(PAY, PAY2,PAY3,PAY4,PAY5,PAY6)


train <- raw.data %>% 
  subset(train == 1) %>% 
  count() %>% 
  as_tibble()

test <- raw.data %>% 
  subset(test == 1)%>% 
  count() %>% 
  as_tibble()

validate <- raw.data %>% 
  subset(validate == 1)%>% 
  count() %>% 
  as_tibble()


#generate table with train, test, validate counts
dat.ty <- c("Train", "Test", "Validate")
data.type <- tibble("Data Type" = unlist(dat.ty, use.names = FALSE))
data.numbers <- c(15180,7323,7497)
data.numbers <- tibble("Counts" = unlist(data.numbers, use.names = FALSE))
raw.data.summary <- cbind(data.type,data.numbers)
raw.data.summary %>% 
  as_tibble() %>% 
  mutate(Percent = Counts / sum(Counts)*100) %>% 
  gt()

#################################################
# Summary Stat Graphics
#################################################
names(rf.train.data.2)

as.data.frame(rf.train.data.2[,-14])


pca.data <- as.data.frame(rf.train.data.2[,-14])
pca <- prcomp(pca.data, center=TRUE, scale=TRUE)

pca2.data <- raw.data %>% 
  subset(train==1) %>% 
  subset(select = -c(u, ID,train,test,validate,data.group, DEFAULT))

pca2.data <- raw.data %>% 
  subset(train==1) %>% 
  subset(select = -c(u, ID,train,test,validate,data.group, DEFAULT))
pca2 <- prcomp(pca2.data, center=TRUE, scale=TRUE)

pca3.data <- rf.train.data.3 %>% 
  subset(select = -c(predicted, DEFAULT))
pca3 <- prcomp(pca3.data, center=TRUE)

plot(pca)
component <- pca2$x
component <- as.data.frame(component)
ggplot(component, aes(x=component$PC1,y=component$PC2, color = as_factor(raw.data$DEFAULT))) +
  geom_point()

component <- pca2$x
component <- as.data.frame(component)
ggplot(component, aes(x=component$PC1,y=component$PC2, color = as_factor(train.data$DEFAULT))) +
  geom_point()

component <- pca3$x
component <- as.data.frame(component)
ggplot(component, aes(x=component$PC1,y=component$PC2, color = as_factor(train.data$DEFAULT))) +
  geom_point()

pca3.data <- nrows  (names(pca3.data))
row.names(cqomponent) <- pca3.data.

library(factoextra)

fviz_pca_ind(pca3,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#################################################
# Feature Engineering
#################################################


data <- raw.data %>% 
  mutate(EDUCATION =ifelse(EDUCATION > 4,4,EDUCATION)) %>% 
  #mutate(SEX = SEX-1) %>% 
  mutate(MARRIAGE = ifelse(MARRIAGE ==0,3, MARRIAGE)) %>% 
  rename(PAY_1 = PAY_0) %>% 
  mutate(avg_bill_amt = rowMeans(select(.,BILL_AMT1,BILL_AMT2,BILL_AMT3,
                                        BILL_AMT4,BILL_AMT5,BILL_AMT6))) %>% 
  mutate(avg_payment_amt = rowMeans(select(., PAY_AMT1,PAY_AMT2,PAY_AMT3,
                                           PAY_AMT4,PAY_AMT5,PAY_AMT6))) %>%
  mutate(payment_ratio_1 = ifelse(PAY_AMT1 == 0,100,BILL_AMT2/PAY_AMT1)) %>% 
  mutate(payment_ratio_2 = ifelse(PAY_AMT2 == 0,100,BILL_AMT3/PAY_AMT2)) %>% 
  mutate(payment_ratio_3 = ifelse(PAY_AMT3 == 0,100,BILL_AMT4/PAY_AMT3)) %>% 
  mutate(payment_ratio_4 = ifelse(PAY_AMT4 == 0,100,BILL_AMT5/PAY_AMT4)) %>% 
  mutate(payment_ratio_5 = ifelse(PAY_AMT5 == 0,100,BILL_AMT6/PAY_AMT5)) %>% 
  mutate(avg_payment_ratio = 
           rowMeans(
             select(., payment_ratio_1,payment_ratio_2,
                    payment_ratio_3,payment_ratio_4,
                    payment_ratio_5))) %>% 
  mutate(utilization_1 = BILL_AMT1/LIMIT_BAL*100) %>% 
  mutate(utilization_2 = BILL_AMT2/LIMIT_BAL*100) %>% 
  mutate(utilization_3 = BILL_AMT3/LIMIT_BAL*100) %>% 
  mutate(utilization_4 = BILL_AMT4/LIMIT_BAL*100) %>% 
  mutate(utilization_5 = BILL_AMT5/LIMIT_BAL*100) %>% 
  mutate(utilization_6 = BILL_AMT6/LIMIT_BAL*100) %>% 
  mutate(average_utilization = 
           rowMeans(
             select(., utilization_1,utilization_2,
                    utilization_3,utilization_4,
                    utilization_5,utilization_6))) %>% 
  mutate(balance_paid_1 = BILL_AMT2-PAY_AMT1) %>% 
  mutate(balance_paid_2 = BILL_AMT3-PAY_AMT2) %>% 
  mutate(balance_paid_3 = BILL_AMT4-PAY_AMT3) %>% 
  mutate(balance_paid_4 = BILL_AMT5-PAY_AMT4) %>% 
  mutate(balance_paid_5 = BILL_AMT6-PAY_AMT5) %>% 
  mutate(avg_balance_paid = 
           rowMeans(
             select(., balance_paid_1,balance_paid_2,
                    balance_paid_3,balance_paid_4,
                    balance_paid_5))) %>%
  mutate(bal_growth_6mo = (LIMIT_BAL-BILL_AMT6)-
           (LIMIT_BAL-BILL_AMT1)) %>%
  mutate(util_growth_6mo =  utilization_6-utilization_1 ) %>% 
  mutate(max_bill_amt = pmax(BILL_AMT1, 
                               BILL_AMT2, BILL_AMT3, BILL_AMT4,
                               BILL_AMT5, BILL_AMT6)) %>% 
  mutate(max_pmt_amt = pmax(PAY_AMT1, 
                            PAY_AMT2, PAY_AMT3, PAY_AMT4,
                            PAY_AMT5, PAY_AMT6)) %>%  
  mutate(sum_PayX_vars =rowSums(select(.,PAY_1,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6))) %>% 
  mutate_at(vars(PAY_1,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6), 
            ~ replace(., . %in% -1:-2,0)) %>% 
  mutate(max_DLQ= pmax(PAY_1, PAY_2, PAY_3, PAY_4,PAY_5, PAY_6)) %>% 
  mutate(max_DLQ = ifelse(max_DLQ <= 0,0,max_DLQ)) %>% 
  #mutate(age_bin_Less_25 = ifelse(AGE <= 25,1,0)) %>% 
  #mutate(age_bin_26_and_35 = ifelse(AGE >= 26 & AGE<=35,1,0)) %>% 
  #mutate(age_bin_36_and_45 = ifelse(AGE >= 36 & AGE<=45,1,0)) %>% 
 # mutate(age_bin_greater_45 = ifelse(AGE >= 45,1,0)) %>% 
  mutate(sum_pay_AMT = rowSums(select(.,PAY_AMT1,PAY_AMT2,
                                      PAY_AMT3,PAY_AMT4,
                                      PAY_AMT5,PAY_AMT6))) %>%
  mutate(sum_bill_AMT= rowSums(select(.,BILL_AMT1,BILL_AMT2,
                                      BILL_AMT3,BILL_AMT4,
                                      BILL_AMT5,BILL_AMT6)))

library(corrr)


  names(data)
  continous <- subset(data, select= c("LIMIT_BAL","SEX","AGE","max_pmt_amt","max_DLQ",
                      "max_bill_amt","bal_growth_6mo","avg_balance_paid",
                      "avg_payment_ratio", "average_utilization","avg_payment_amt",
                      "avg_bill_amt","DEFAULT"))
  data.cor = cor(continous, method = c("spearman"))
  corrplot(data.cor, type = "lower", tl.col = "black", tl.srt = 45)
  continuous.correlation <- as.data.frame(data.cor)
  continuous.correlation$DEFAULT
  rownames(continuous.correlation)
  corr.DEFAULT <- (as.data.frame(cbind(rownames(continuous.correlation),continuous.correlation$DEFAULT)))
  colnames(corr.DEFAULT) <- c("Variable", "CorrelationToDefault")
  corr.DEFAULT$CorrelationToDefault
  corr.DEFAULT %>% 
    as_tibble() %>% 
    mutate_at(vars(CorrelationToDefault), as.numeric) %>% 
    mutate(order = abs(CorrelationToDefault)) %>% 
    arrange(desc(order)) %>% 
    select(- order) %>% 
    gt()
  
  data <-data %>% 
    mutate(max_pmt_bin = ifelse(max_pmt_amt <= 4500, "                           <=4500",
                                ifelse(max_pmt_amt > 4500 & max_pmt_amt <= 36399.6, 
                                       "max_pmt_amt > 4500 & max_pmt_amt <= 36399.6",
                                       ifelse(max_pmt_amt > 36399.6,
                                              "                           >36399",data$max_pmt_amt))))
  
  
  
  
  mosaicplot(data$DEFAULT~data$max_pmt_bin, 
             main = "MAX Payment Bin by DEFAULT Mosaic Plot",
             #sub = "Product Colors by Max DLQ",
             # xlab = "Countries",
             # ylab = "Colors",
             las = 1,
             border = "chocolate",
             shade = TRUE)
  
  myaov <- aov(data$DEFAULT~as.factor(data$max_DLQ))
  gt(tukey_hsd(myaov))
  
  
  data.bin.corr.df <- data %>% 
    #subset(select=-c(max_pmt_bin)) %>% 
    subset(select= reg.vars.clean)
  
  
  names(data)
  data.cor.bin = cor(data.bin.corr.df, method = c("spearman"))
  corrplot(data.cor.bin, type = "lower", tl.col = "black", tl.srt = 45)
  data.bin.corr.df<- as.data.frame(data.cor.bin)
  Analysis <- rownames(data.cor.bin.df)
  data.cor.bin.df <- cbind(Analysis,data.bin.corr.df$DEFAULT) 
  colnames(data.cor.bin.df) <- c("Analaysis","DEFAULT")
  data.cor.bin.df <- as.data.frame(data.cor.bin.df)
  
  write_csv((data.cor.bin.df %>%  arrange(desc(DEFAULT))),"woecorr.csv")
  data.cor.bin.df <- read_csv("../python/woecorr.csv")
  
  gt(data.cor.bin.df %>%  arrange(desc(DEFAULT)))
s#####################################################################
# WOE binning;
#####################################################################

woe.data <- data %>% 
  subset(train==1)

# age.woe <- woe.binning(df=data,target.var=c('DEFAULT'),pred.var=c('AGE'))
# woe.binning.plot(age.woe)
# woe.df <- woe.binning.deploy(df=raw.data,binning=age.woe)
# head(woe.df)
# table(woe.df$AGE.binned)
# 
# # See the WOE Binning Table
# woe.binning.table(age.woe)
# 
# 
# 
# age.tree <- woe.tree.binning(df=raw.data,target.var=c('DEFAULT'),pred.var=c('AGE'))
# 
# # WOE plot for age bins;
# woe.binning.plot(age.tree)
# # Note that we got different bins;
# 
# # Score bins on data frame;
# tree.df <- woe.binning.deploy(df=raw.data,binning=age.tree)
# head(tree.df)
# table(tree.df$AGE.binned)
# 
# # See the WOE Binning Table
# gt(as.data.frame(woe.binning.table(age.tree)))
# 
# # age bins to try: <=25, 26-35, 36-45, >=45
# 
# 
# DLQ.woe <- woe.binning(df=data,target.var=c('DEFAULT'),pred.var=c('max_DLQ'))
# woe.binning.plot(DLQ.woe)
# woe.df <- woe.binning.deploy(df=raw.data,binning=DLQ.woe)
# head(woe.df)
# table(woe.df$DLQ.binned)
# 
# # See the WOE Binning Table
# woe.binning.table(DLQ.woe)
# 
# 
# 
# DLQ.tree <- woe.tree.binning(df=data,target.var=c('DEFAULT'),pred.var=c('max_DLQ'))
# 
# # WOE plot for DLQ bins;
# woe.binning.plot(DLQ.tree)
# # Note that we got different bins;
# 
# # Score bins on data frame;
# tree.df <- woe.binning.deploy(df=raw.data,binning=DLQ.tree)
# head(tree.df)
# table(tree.df$DLQ.binned)
# 
# # See the WOE Binning Table
# gt(as.data.frame(woe.binning.table(DLQ.tree)))
# 
# 
# edu.woe <- woe.binning(df=data,target.var=c('DEFAULT'),pred.var=c('EDUCATION'))
# #woe.binning.plot(edu.woe)
# woe.df <- woe.binning.deploy(df=data,binning=edu.woe)
# woe.binning.table(edu.woe)
# edu.tree <- woe.tree.binning(df=data,target.var=c('DEFAULT'),pred.var=c('EDUCATION'))
# woe.binning.plot(edu.tree)
# tree.df <- woe.binning.deploy(df=raw.data,binning=edu.tree)
# table(tree.df$edu.binned)
# woe.binning.table(edu.tree)
# #woe.binning.plot(edu.tree)
# gt(as.data.frame(woe.binning.table(edu.tree)))
# 
# ###BINS for 1 and 3
# 
# 
# gender.woe <- woe.binning(df=data,target.var=c('DEFAULT'),pred.var=c('SEX'))
# #woe.binning.plot(gender.woe)
# woe.df <- woe.binning.deploy(df=data,binning=gender.woe)
# woe.binning.table(gender.woe)
# gender.tree <- woe.tree.binning(df=data,target.var=c('DEFAULT'),pred.var=c('SEX'))
# woe.binning.plot(gender.tree)
# tree.df <- woe.binning.deploy(df=raw.data,binning=gender.tree)
# table(tree.df$gender.binned)
# woe.binning.table(gender.tree)
# #woe.binning.plot(gender.tree)
# gt(as.data.frame(woe.binning.table(gender.tree)))
# 
# ###BINS for 1
# marriage.woe <- woe.binning(df=data,target.var=c('DEFAULT'),pred.var=c('MARRIAGE'))
# #woe.binning.plot(marriage.woe)
# woe.df <- woe.binning.deploy(df=data,binning=marriage.woe)
# woe.binning.table(marriage.woe)
# marriage.tree <- woe.tree.binning(df=data,target.var=c('DEFAULT'),pred.var=c('MARRIAGE'))
# woe.binning.plot(marriage.tree)
# tree.df <- woe.binning.deploy(df=raw.data,binning=marriage.tree)
# table(tree.df$marriage.binned)
# woe.binning.table(marriage.tree)
# #woe.binning.plot(marriage.tree)
# gt(as.data.frame(woe.binning.table(marriage.tree)))
# 
# ###BINS for 1
# 
# 
# maxbill.woe <- woe.binning(df=data,target.var=c('DEFAULT'),pred.var=c('max_bill_amt'))
# woe.binning.plot(maxbill.woe)
# woe.df <- woe.binning.deploy(df=data,binning=maxbill.woe)
# head(woe.df)
# table(woe.df$maxbill.binned)
# woe.binning.table(maxbill.woe)
# maxbill.tree <- woe.tree.binning(df=data,target.var=c('DEFAULT'),pred.var=c('max_bill_amt'))
# woe.binning.plot(maxbill.tree)
# tree.df <- woe.binning.deploy(df=raw.data,binning=maxbill.tree)
# head(tree.df)
# table(tree.df$maxbill.binned)
# gt(as.data.frame(woe.binning.table(maxbill.tree)))
# 
# avgbalpaid.woe <- woe.binning(df=data,target.var=c('DEFAULT'),pred.var=c('avg_balance_paid'))
# #woe.binning.plot(avgbalpaid.woe)
# woe.df <- woe.binning.deploy(df=data,binning=avgbalpaid.woe)
# woe.binning.table(avgbalpaid.woe)
# avgbalpaid.tree <- woe.tree.binning(df=data,target.var=c('DEFAULT'),pred.var=c('avg_balance_paid'))
# woe.binning.plot(avgbalpaid.tree)
# tree.df <- woe.binning.deploy(df=raw.data,binning=avgbalpaid.tree)
# table(tree.df$avgbalpaid.binned)
# woe.binning.table(avgbalpaid.tree)
# woe.binning.plot(avgbalpaid.tree)
# gt(as.data.frame(woe.binning.table(avgbalpaid.tree)))
# 
# limbal.woe <- woe.binning(df=data,target.var=c('DEFAULT'),pred.var=c('LIMIT_BAL'))
# #woe.binning.plot(limbal.woe)
# woe.df <- woe.binning.deploy(df=data,binning=limbal.woe)
# woe.binning.table(limbal.woe)
# limbal.tree <- woe.tree.binning(df=data,target.var=c('DEFAULT'),pred.var=c('LIMIT_BAL'))
# woe.binning.plot(limbal.tree)
# tree.df <- woe.binning.deploy(df=raw.data,binning=limbal.tree)
# table(tree.df$limbal.binned)
# woe.binning.table(limbal.tree)
# #woe.binning.plot(limbal.tree)
# gt(as.data.frame(woe.binning.table(limbal.tree)))
# 
# limbal.woe <- woe.binning(df=data,target.var=c('DEFAULT'),pred.var=c('LIMIT_BAL'))
# #woe.binning.plot(limbal.woe)
# woe.df <- woe.binning.deploy(df=data,binning=limbal.woe)
# woe.binning.table(limbal.woe)
# limbal.tree <- woe.tree.binning(df=data,target.var=c('DEFAULT'),pred.var=c('LIMIT_BAL'))
# woe.binning.plot(limbal.tree)
# tree.df <- woe.binning.deploy(df=raw.data,binning=limbal.tree)
# table(tree.df$limbal.binned)
# woe.binning.table(limbal.tree)
# #woe.binning.plot(limbal.tree)
# gt(as.data.frame(woe.binning.table(limbal.tree)))



woe.bin.custom <- function(targetVar, predVar, df) {
  woe <- woe.binning(df=df,target.var=c(targetVar),pred.var=c(predVar))
  #woe.binning.plot(limbal.woe)
  woe.df <- woe.binning.deploy(df=df,binning=woe)
  #print(woe.binning.table(woe))
  woe.tree <- woe.tree.binning(df=df,target.var=c(targetVar),pred.var=c(predVar))
  #woe.binning.plot(woe.tree)
  #tree.df <- woe.binning.deploy(df=raw.data,binning=woe.tree)
  #table(tree.df$limbal.binned)
  print(woe.binning.table(woe.tree))
  woe.binning.plot(woe.tree)
  #gt(as.data.frame(woe.binning.table(limbal.tree)))
}

for (var in names(woe.data)) {
  woe.bin.custom("DEFAULT",var,woe.data)
  
}

woe.bin.custom("DEFAULT","bal_growth_6mo",woe.data)
# data <- data %>% 
#   mutate(LIMIT_BAL_Less30000  = ifelse(LIMIT_BAL <= 30000,1,0)) %>% 
#   mutate(LIMIT_BAL_between30000and160000 = ifelse(LIMIT_BAL > 30000 & LIMIT_BAL< 160000,1,0)) %>% 
#   mutate(LIMIT_BAL_Greater160000 = ifelse(LIMIT_BAL >= 160000,1,0)) %>% 
#   mutate(SEX_lessthanorEqual1 = ifelse(SEX <= 1,1,0)) %>% 
#   mutate(SEX_greater1 = ifelse(SEX > 1,1,0)) %>% 
#   mutate(EDUCATION_lessthanorEqual1 = ifelse(EDUCATION <= 1,1,0)) %>% 
#   mutate(EDUCATION_lessthanorEqual3 = ifelse(EDUCATION > 1 & EDUCATION <= 3,1,0)) %>% 
#   mutate(EDUCATION_greater3 = ifelse(EDUCATION > 3,1,0)) %>% 
#   mutate(Marriage_lesthanorqualto1 = ifelse(MARRIAGE <= 1,1,0)) %>% 
#   mutate(Marriage_greaterthan1 = ifelse(MARRIAGE>1,1,0)) %>% 
#   mutate(age_bin_Less_25 = ifelse(AGE <= 25,1,0)) %>% 
#   mutate(age_bin_26_and_35 = ifelse(AGE > 25 & AGE<=35,1,0)) %>% 
#   mutate(age_bin_36_and_45 = ifelse(AGE > 35 & AGE<=45,1,0)) %>% 
#   mutate(age_bin_greater_45 = ifelse(AGE > 45,1,0)) %>% 
#   mutate(avg_bill_amt_lessthan0 = ifelse(avg_bill_amt <=0,1,0)) %>% 
#   mutate(avg_bill_amt_0to1159 = ifelse(avg_bill_amt >0  & LIMIT_BAL<= 1159.42833,1,0)) %>% 
#   mutate(avg_bill_amt_1159to7878 = ifelse(avg_bill_amt >1159.42833  & LIMIT_BAL<= 7878.448333,1,0)) %>%  
#   mutate(avg_bill_amt_7878to49419 = ifelse(avg_bill_amt >7878.448333  & LIMIT_BAL<= 49419.44,1,0)) %>%  
#   mutate(avg_bill_amt_greaterThan49419 = ifelse(avg_bill_amt >49419.44,1,0)) %>%  
#   mutate(avg_pymt_amt_lessthan2045 = ifelse(avg_payment_amt <=2475.33,1,0)) %>% 
#   mutate(avg_pymt_amt_2475to12935 = ifelse(avg_payment_amt >2475.33  & LIMIT_BAL<= 12935.46833,1,0)) %>%  
#   mutate(avg_pymt_amt_greaterThan12935 = ifelse(avg_payment_amt >12935.46833,1,0)) %>%  
#   mutate(avg_payment_ratio_lessthan13 = ifelse(avg_payment_ratio <=13.14667259,1,0)) %>%  
#   mutate(avg_payment_ratio_13to28 = ifelse(avg_payment_ratio >13.14667259 & 
#                                              avg_payment_ratio<=28.28176359,1,0)) %>%  
#   mutate(avg_payment_ratio_greaterThan28 = ifelse(avg_payment_ratio >28.28176359,1,0)) %>%  
#   mutate(avg_util_lessthan.10 = ifelse(average_utilization <=0.1019933333,1,0)) %>% 
#   mutate(avg_util_.1to45 = ifelse(average_utilization > 0.1019933333 &
#                                     average_utilization <=45.17598519,1,0)) %>% 
#   mutate(avg_util_greaterthan45 = ifelse(average_utilization < 45.17598519,1,0)) %>% 
#   mutate(avg_balance_paid_lessthanNeg1 = ifelse(avg_balance_paid <=-1,1,0)) %>% 
#   mutate(avg_balance_paid_neg1to48976 = ifelse(avg_balance_paid > -1 &
#                                      avg_balance_paid <48976.35,1,0)) %>% 
#   
#   mutate(avg_balance_paid_greaterThan48976 = ifelse(avg_balance_paid <= 48976.35,1,0)) %>% 
#   mutate(bal_growth_6mo_lessthaNeg11546 = ifelse(avg_balance_paid <= -11546,1,0)) %>%
#   mutate(bal_growth_6mo_neg11546to740 = ifelse(avg_balance_paid >-11546 & avg_balance_paid <= 740,1,0)) %>%
#   mutate(bal_growth_6mo_greaterThan740 = ifelse(avg_balance_paid > 740,1,0)) %>%
#   mutate(util_growth_6mo_lessthanNeg34.244 = ifelse(util_growth_6mo <= -31.24454, 1, 0)) %>% 
#   mutate(util_growth_6mo_Neg34.244toNeg0.0832 = ifelse(util_growth_6mo > -31.24454 & 
#                                                     util_growth_6mo <= -0.08321794872, 1, 0)) %>% 
#   mutate(util_growth_6mo_Neg0.0832To0 = ifelse(util_growth_6mo > -0.08321794872 & 
#                                                          util_growth_6mo <= -0, 1, 0)) %>%
#   mutate(util_growth_6mo_0to4 = ifelse(util_growth_6mo > 0 & 
#                                                  util_growth_6mo <= 4.037, 1, 0)) %>%
#   mutate(util_growth_6mo_4to24 = ifelse(util_growth_6mo > 4.037 & 
#                                          util_growth_6mo <= 24.1906, 1, 0)) %>%
#   mutate(util_growth_6mo_greaterthan = ifelse(util_growth_6mo > 24.1906,1,0)) %>%
#   mutate(max_bill_amt_lessthan0 = ifelse(max_bill_amt<= 0, 1, 0)) %>% 
#   mutate(max_bill_amt_0to3058 = ifelse(max_bill_amt > 0 & max_bill_amt <= 3058.87, 1, 0)) %>% 
#   mutate(max_bill_amt_3058to52496 = ifelse(max_bill_amt > 3058.87 &
#                                               max_bill_amt <= 52496.15, 1, 0)) %>% 
#   mutate(max_bill_amt_greaterthan52496 = ifelse(max_bill_amt > 52496.15, 1, 0)) %>% 
#   mutate(max_pmt_amt_lessthan4790 = ifelse(max_pmt_amt <= 4790, 1, 0)) %>% 
#   mutate(max_pmt_amt_4790to19000 = ifelse(max_pmt_amt > 4790 & max_pmt_amt <= 19000, 1, 0)) %>% 
#   mutate(max_pmt_amt_greaterthan19000 = ifelse(max_pmt_amt > 19000, 1, 0)) %>% 
#   mutate(max_DLQ_lessthan1 = ifelse(max_pmt_amt <= 1, 1, 0)) %>% 
#   mutate(max_DLQ_greaterthan1 = ifelse(max_pmt_amt > 1, 1, 0)) %>% 
#   mutate(sum_pay_AMT_lessthan14851 = ifelse(sum_pay_AMT <= 14851.98, 1, 0)) %>% 
#   mutate(sum_pay_AMT_14851to77612 = ifelse(sum_pay_AMT > 14851.98 & sum_pay_AMT <= 77612.81, 1, 0)) %>% 
#   mutate(sum_pay_AMT_greaterthan77612 = ifelse(sum_pay_AMT > 77612.81, 1, 0)) %>% 
#   mutate(sum_bill_AMT_lessthan0 = ifelse(sum_pay_AMT <= 0, 1, 0)) %>% 
#   mutate(sum_bill_AMT_0to6956 = ifelse(sum_pay_AMT > 0 & sum_pay_AMT <= 6956.57, 1, 0)) %>% 
#   mutate(sum_bill_AMT_6956to472701 = ifelse(sum_pay_AMT > 6956.57 & sum_pay_AMT <= 47270.69, 1, 0)) %>% 
#   mutate(sum_bill_AMT_472701 = ifelse(sum_pay_AMT > 47270.69 & sum_pay_AMT <= 296516.64, 1, 0)) %>% 
#   mutate(sum_bill_AMT_greaterthan296516 = ifelse(sum_pay_AMT > 296516.64, 1, 0))
before.woe <- data
data <- data %>% 
  mutate(LIMIT_BAL_Less30000  = ifelse(LIMIT_BAL <= 30000,1,0)) %>% 
  mutate(LIMIT_BAL_between30000and140000 = ifelse(LIMIT_BAL > 30000 & LIMIT_BAL< 140000,1,0)) %>% 
  mutate(LIMIT_BAL_Greater140000 = ifelse(LIMIT_BAL >= 160000,1,0)) %>% 
  mutate(SEX_lessthanorEqual1 = ifelse(SEX <= 1,1,0)) %>% 
  mutate(SEX_greater1 = ifelse(SEX > 1,1,0)) %>% 
  mutate(EDUCATION_lessthanorEqual1 = ifelse(EDUCATION <= 1,1,0)) %>% 
  mutate(EDUCATION_lessthanorEqual3 = ifelse(EDUCATION > 1 & EDUCATION <= 3,1,0)) %>% 
  mutate(EDUCATION_greater3 = ifelse(EDUCATION > 3,1,0)) %>% 
  mutate(Marriage_lesthanorqualto1 = ifelse(MARRIAGE <= 1,1,0)) %>% 
  mutate(Marriage_greaterthan1 = ifelse(MARRIAGE>1,1,0)) %>% 
  mutate(age_bin_Less_25 = ifelse(AGE <= 25,1,0)) %>% 
  mutate(age_bin_26_and_34 = ifelse(AGE > 25 & AGE<=34,1,0)) %>% 
  mutate(age_bin_34_and_48 = ifelse(AGE > 25 & AGE<=34,1,0)) %>% 
  mutate(age_bin_greater_48 = ifelse(AGE > 48,1,0)) %>% 
  mutate(avg_bill_amt_lessthan0 = ifelse(avg_bill_amt <=0,1,0)) %>% 
  mutate(avg_bill_amt_0to2510 = ifelse(avg_bill_amt >0  & LIMIT_BAL<= 2510.843333,1,0)) %>% 
  mutate(avg_bill_amt_2510to11861 = ifelse(avg_bill_amt >2510.843333  & LIMIT_BAL<= 11861.59333,1,0)) %>%  
  mutate(avg_bill_amt_11861to71812 = ifelse(avg_bill_amt >11861.59333  & LIMIT_BAL<= 71812.5,1,0)) %>%  
  mutate(avg_bill_amt_11861to302472 = ifelse(avg_bill_amt >71812.5  & LIMIT_BAL<= 302472.82,1,0)) %>%  
  mutate(avg_bill_amt_greaterThan302472 = ifelse(avg_bill_amt >302472.82,1,0)) %>%  
  mutate(avg_pymt_amt_lessthan1333 = ifelse(avg_payment_amt <=1333.333333,1,0)) %>% 
  mutate(avg_pymt_amt_1333to3000 = ifelse(avg_payment_amt >2475.33  & LIMIT_BAL<= 3000,1,0)) %>%  
  mutate(avg_pymt_amt_3000to12092 = ifelse(avg_payment_amt >3000  & LIMIT_BAL<= 12092.5,1,0)) %>%  
  mutate(avg_pymt_amt_greaterThan12092 = ifelse(avg_payment_amt >12092.5,1,0)) %>%  
  mutate(avg_payment_ratio_lessthan6.87946722 = ifelse(avg_payment_ratio <=6.87946722,1,0)) %>%  
  mutate(avg_payment_ratio_6.87946722to29 = ifelse(avg_payment_ratio >6.87946722 & 
                                             avg_payment_ratio<=29.06812013,1,0)) %>%  
  mutate(avg_payment_ratio_greaterThan29 = ifelse(avg_payment_ratio >29.06812013,1,0)) %>%  
  mutate(avg_util_lessthan0= ifelse(average_utilization <=0,1,0)) %>% 
  mutate(avg_util_.09to0.92 = ifelse(average_utilization > 0 &
                                    average_utilization <=51.35143137,1,0)) %>% 
  mutate(avg_util_greaterthan51 = ifelse(average_utilization < 51.35143137,1,0)) %>% 
  mutate(avg_balance_paid_lessthanNeg2 = ifelse(avg_balance_paid <=-2,1,0)) %>% 
  mutate(avg_balance_paid_neg2to11243.352 = ifelse(avg_balance_paid > -2 &
                                                 avg_balance_paid <= 11243.352,1,0)) %>% 
  mutate(avg_balance_paid_11243to38253 = ifelse(avg_balance_paid > 11243.352 &
                                                     avg_balance_paid <=38253.342,1,0)) %>% 
  mutate(avg_balance_paid_greaterThan38253 = ifelse(avg_balance_paid > 38253.342,1,0)) %>% 
  mutate(bal_growth_6mo_lessthaNeg11333 = ifelse(avg_balance_paid <= -11333.56,1,0)) %>%
  mutate(bal_growth_6mo_neg11333to734 = ifelse(avg_balance_paid >-11333.56 & avg_balance_paid <= 734.71,1,0)) %>%
  mutate(bal_growth_6mo_greaterThan734 = ifelse(avg_balance_paid > 734.71,1,0)) %>%
  mutate(util_growth_6mo_lessthanNeg66.81695167 = ifelse(util_growth_6mo <= -66.81695167, 1, 0)) %>% 
  mutate(util_growth_6mo_Neg66.816951674toNeg0.075 = ifelse(util_growth_6mo > -66.81695167 & 
                                                         util_growth_6mo <= -0.075, 1, 0)) %>% 
  mutate(util_growth_6mo_Neg0.075To0 = ifelse(util_growth_6mo > -0.075 & 
                                                 util_growth_6mo <= -0, 1, 0)) %>%
  mutate(util_growth_6mo_0to4 = ifelse(util_growth_6mo > 0 & 
                                         util_growth_6mo <= 4.053416667, 1, 0)) %>%
  mutate(util_growth_6mo_greaterthan4 = ifelse(util_growth_6mo > 4.053416667,1,0)) %>%
  mutate(max_bill_amt_lessthan0 = ifelse(max_bill_amt<= 0, 1, 0)) %>% 
  mutate(max_bill_amt_0to2265.14 = ifelse(max_bill_amt > 0 & max_bill_amt <= 2265.14, 1, 0)) %>% 
  mutate(max_bill_amt_2265.14to2500 = ifelse(max_bill_amt > 2265.14 & max_bill_amt <= 2500, 1, 0)) %>% 
  mutate(max_bill_amt_2500to54974.66 = ifelse(max_bill_amt > 2500 &
                                             max_bill_amt <= 54974.66, 1, 0)) %>% 
  mutate(max_bill_amt_greaterthan54974.66 = ifelse(max_bill_amt > 54974.66, 1, 0)) %>%
  mutate(max_pmt_amt_lessthan4500 = ifelse(max_pmt_amt <= 4500, 1, 0)) %>% 
  mutate(max_pmt_amt_4500to36399.6 = ifelse(max_pmt_amt > 4500 & max_pmt_amt <= 36399.6, 1, 0)) %>% 
  mutate(max_pmt_amt_greaterthan36399.6 = ifelse(max_pmt_amt > 36399.6, 1, 0)) %>%
  mutate(max_DLQ_lessthan1 = ifelse(max_pmt_amt <= 1, 1, 0)) %>% 
  mutate(max_DLQ_greaterthan1 = ifelse(max_pmt_amt > 1, 1, 0)) %>% 
  mutate(sum_pay_AMT_lessthan8000 = ifelse(sum_pay_AMT <= 8000, 1, 0)) %>% 
  mutate(sum_pay_AMT_8000to18000 = ifelse(sum_pay_AMT > 8000 & sum_pay_AMT <= 18000, 1, 0)) %>% 
  mutate(sum_pay_AMT_18000to72555 = ifelse(sum_pay_AMT > 18000 & sum_pay_AMT <= 72555, 1, 0)) %>% 
  mutate(sum_pay_AMT_greaterthan72555 = ifelse(sum_pay_AMT > 72555, 1, 0)) %>% 
  mutate(sum_bill_AMT_lessthan0 = ifelse(sum_pay_AMT <= 0, 1, 0)) %>% 
  mutate(sum_bill_AMT_0to15065.06 = ifelse(sum_pay_AMT > 0 & sum_pay_AMT <= 15065.06, 1, 0)) %>% 
  mutate(sum_bill_AMT_15065.06to71169.56 = ifelse(sum_pay_AMT > 15065.06 & sum_pay_AMT <= 71169.56, 1, 0)) %>% 
  mutate(sum_bill_AMT_71169.56to430875 = ifelse(sum_pay_AMT > 71169.56 & sum_pay_AMT <= 430875, 1, 0)) %>% 
  mutate(sum_bill_AMT_430875to1814836.92 = ifelse(sum_pay_AMT > 430875 & sum_pay_AMT <= 1814836.92, 1, 0)) %>% 
  mutate(sum_bill_AMT_greaterthan1814836.926 = ifelse(sum_pay_AMT > 1814836.92, 1, 0))
"age_bin_26_and_48" ,                       
"avg_util_0.92_to_51",
corrTabler <- function(listofVars,df,comparisonVar) {
  df %>% 
    subset(select=listofVars) %>% 
    corrr::correlate() %>% 
    as_tibble() %>% 
    select(term,comparisonVar)
}

names(data)
age_var_list <- c("age_bin_Less_25",                          
                  "age_bin_26_and_34" , "age_bin_34_and_48",                    
                   "age_bin_greater_48","DEFAULT" )
edu_var_list <- c("EDUCATION_lessthanorEqual1","EDUCATION_lessthanorEqual3","DEFAULT")
limbal_var_list <- 
  c("LIMIT_BAL_Less30000", "LIMIT_BAL_between30000and140000","LIMIT_BAL_Greater140000","DEFAULT")
sex_var_list <- c("SEX_greater1", "SEX_lessthanorEqual1","DEFAULT")
marriage_var_list <- c("Marriage_greaterthan1","Marriage_lesthanorqualto1","DEFAULT")
avg_bill_amt_var_list <- c("avg_bill_amt_lessthan0", "avg_bill_amt_0to2510", 
                           "avg_bill_amt_2510to11861", "avg_bill_amt_11861to71812",
                           "avg_bill_amt_11861to302472","avg_bill_amt_greaterThan302472","DEFAULT")

avg_pymt_amt_var_list <- c("avg_pymt_amt_lessthan1333", "avg_pymt_amt_1333to3000",
                           "avg_pymt_amt_3000to12092","avg_pymt_amt_greaterThan12092","DEFAULT")

avg_payment_ratio_var_list <- c("avg_payment_ratio_lessthan6.87946722",  
                                "avg_payment_ratio_6.87946722to29","avg_payment_ratio_greaterThan29","DEFAULT")

avg_util_var_list <- c("avg_util_lessthan0", "avg_util_.09to0.92","avg_util_greaterthan51",
                       "avg_util_0.92_to_51",
                       "DEFAULT")


avg_balance_paid_var_list <- c("avg_balance_paid_lessthanNeg2", "avg_balance_paid_neg2to11243.352", 
                               "avg_balance_paid_11243to38253" ,           
                               "avg_balance_paid_greaterThan38253","DEFAULT")

bal_growth_var_list <- c("bal_growth_6mo_lessthaNeg11333",           
                         "bal_growth_6mo_neg11333to734", "bal_growth_6mo_greaterThan734","DEFAULT")

util_growth_var_list <- c("util_growth_6mo_lessthanNeg66.81695167",
                          "util_growth_6mo_Neg66.816951674toNeg0.075","util_growth_6mo_Neg0.075To0",
                          "util_growth_6mo_0to4",                  
                          "util_growth_6mo_greaterthan4","DEFAULT")

max_bill_amt_var_list <- c("max_bill_amt_lessthan0",                   
                           "max_bill_amt_0to2265.14" , "max_bill_amt_2265.14to2500",               
                           "max_bill_amt_2500to54974.66", "max_bill_amt_greaterthan54974.66","DEFAULT")

max_pmt_amt <- c("max_pmt_amt_lessthan4500","max_pmt_amt_4500to36399.6",                
                 "max_pmt_amt_greaterthan36399.6","DEFAULT")


max_DLQ_var_list <- c("max_DLQ_lessthan1","max_DLQ_greaterthan1","DEFAULT")

sum_pay_AMT_var_list <- c("sum_pay_AMT_lessthan8000",                 
                          "sum_pay_AMT_8000to18000","sum_pay_AMT_18000to72555",
                         "sum_pay_AMT_greaterthan72555","DEFAULT")

sum_bill_AMT_var_list <- c( "sum_bill_AMT_lessthan0",               
                            "sum_bill_AMT_0to15065.06","sum_bill_AMT_15065.06to71169.56",        
                            "sum_bill_AMT_71169.56to430875","sum_bill_AMT_430875to1814836.92",          
                            "sum_bill_AMT_greaterthan1814836.926","DEFAULT")



educCorr <- corrTabler(edu_var_list,data,"DEFAULT") #choose 1
ageCorr <- corrTabler(age_var_list,data,"DEFAULT") #innvestigate
limbalCorr <- corrTabler(limbal_var_list,data,"DEFAULT") 
sexCorr <- corrTabler(sex_var_list,data,"DEFAULT") # choose 1
marriageCorr <- corrTabler(marriage_var_list,data,"DEFAULT") # choose 1
abgbillamt <- corrTabler(avg_bill_amt_var_list,data,"DEFAULT") ##2 avg_bill_amt_0to1159          NA     
#3 avg_bill_amt_1159to7878       NA   
avgpymtamt <- corrTabler(avg_pymt_amt_var_list,data,"DEFAULT")
avg_payment_ratioCorr <-corrTabler(avg_payment_ratio_var_list,data,"DEFAULT")
avg_utilCorr <- corrTabler(avg_util_var_list,data,"DEFAULT")

avg_balance_paidCorr <- corrTabler(avg_balance_paid_var_list,data,"DEFAULT")
bal_growthCorr <- corrTabler(bal_growth_var_list,data,"DEFAULT") #drop bal_growth_6mo_greaterThan740
util_growthCorr <- corrTabler(util_growth_var_list,data,"DEFAULT") 
## remove util_growth_6mo_lessthanNeg34.244 and util_growth_6mo_greaterthan
max_bill_amtCorr <- corrTabler(max_bill_amt_var_list,data,"DEFAULT")
max_pmt_amtCorr <- corrTabler(max_pmt_amt,data,"DEFAULT")
max_DLQCorr <- corrTabler(max_DLQ_var_list,data,"DEFAULT") ##choosee 1
sum_pay_AMTCorr <- corrTabler(sum_pay_AMT_var_list,data,"DEFAULT")
sum_bill_AMTCorr <- corrTabler(sum_bill_AMT_var_list,data,"DEFAULT")
##

subsetDF <- function(data, varlist) {
  data %>% 
    subset(select=varlist)
}

makeGraph <- function(DF) {
  new.frame <- melt(DF, id.vars="DEFAULT")
  new.frame$value <- new.frame$value+1
  new.frame$value <- ifelse(new.frame$value == 2,1,new.frame$value)
  print(new.frame)
  ggplot(new.frame,aes(x=variable,fill=as.factor(DEFAULT)))+
    geom_bar()
    # scale_fill_discrete(name="Gender",
    #                     breaks=c(1, 2),
    #                     labels=c("Male", "Female"))+
    # xlab("Beverage")+ylab("Mean Percentage")
}

edu <- subsetDF(data, edu_var_list) 
makeGraph(edu)

maxdlqdf <- subsetDF(data, max_DLQ_var_list) 
makeGraph(maxdlqdf)

maxbillqdf <- subsetDF(data, max_bill_amt_var_list) 
makeGraph(maxbillqdf)


for (col in names(data[60:61])) {
  print(col)
  newdf <- data[col]
  
  ggplot(newdf, aes(x=col, fill= as.factor(data$DEFAULT))) +
    geom_text(stat='count', aes(label=..count..), vjust=-1)
    # xlab("MARRIAGE - 1=Married, 2=Single, 3=Other") +
    # labs(title = "Count Plot MARRIAGE", fill = "DEFAULT")
}
ggplot(data, aes(x=as.factor(MARRIAGE), fill= as.factor(DEFAULT))) +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  xlab("MARRIAGE - 1=Married, 2=Single, 3=Other") +
  labs(title = "Count Plot MARRIAGE", fill = "DEFAULT")


#woe.correctionn via correlation

data <- data %>% subset(select=-c(age_bin_26_and_34,age_bin_34_and_48)) %>% 
  mutate(age_bin_26_and_48 = ifelse(AGE >26 & AGE <=48,1,0))

data <- data %>%
  mutate(avg_util_0.92_to_51 = ifelse(average_utilization >.92 & average_utilization <=51,1,0))

data.cor = cor(data, method = c("spearman"))
corrplot(data.cor, type = "lower", tl.col = "black", tl.srt = 45)

WOEBinTable <- read_csv("../python/WOEBins.csv")
gt(WOEBinTable)
names(data)

one_hot <- function(data, var) {
  
  var_enquo <- enquo(var)
  items <- data %>% pull(!!var_enquo)
  items_unique <- items %>% unique()
  
  out <- matrix(0, NROW(data), length(items_unique))
  colnames(out) <- items_unique
  
  for (i in items_unique) {
    out[, i] <- items == i
  }
  
  data %>%
    select(-!!var_enquo) %>%
    bind_cols(as.tibble(out))
}
# 
# discrete <- c("DEFAULT","SEX","EDUCATION","MARRIAGE")
# 
# ohe.tree.data <- subset(data, select=discrete)

# EDUCATION.ohe <- one_hot(ohe.tree.data, EDUCATION)
# EDUCATION.ohe<- EDUCATION.ohe %>% 
#   select(-c(DEFAULT,SEX, MARRIAGE))
# colnames(EDUCATION.ohe) <- c("2-EDUCATION","1-EDUCATION",
#                          "3-EDUCATION","4-EDUCATION",
#                          "0-EDUCATION")
# marriage.ohe <- one_hot(ohe.tree.data, MARRIAGE)
# marriage.ohe<- marriage.ohe %>% 
#   select(-c(DEFAULT,SEX, EDUCATION))
# colnames(marriage.ohe) <- c("1-MARRIAGE","2-MARRIAGE",
#                             "3-MARRIAGE")
# 
# SEX.ohe <- one_hot(ohe.tree.data, SEX)
# SEX.ohe<- SEX.ohe %>% 
#   select(-c(DEFAULT,MARRIAGE, EDUCATION))
# colnames(SEX.ohe) <- c("1-SEX","2-SEX")
# 
# data <- cbind(data,marriage.ohe,SEX.ohe,EDUCATION.ohe)







  #####################################################################
# visualizing differences between default with box and stripplot
#####################################################################
names(data)
melted<- raw.data %>% 
  pivot_longer(everything()
  )

data.melt <- data %>% 
  select(-ID)
data.melt<- melt(data = data.melt, id.vars = c("DEFAULT"))
data.melt$DEFAULT.COLOR <- ifelse(data$DEFAULT==0,"no.default","default")
ggplot(data.melt,aes(y=variable,x=value, color=DEFAULT.COLOR)) +
  geom_boxplot()

maxDLQ.boxplot <- ggplot(data,aes(x=max_DLQ, fill=DEFAULT.COLOR)) +
  geom_boxplot() +
  labs(title = "Max_DLQ BoxPlot by Default")

  avg_payment_ratio.boxplot <- ggplot(data,aes(x=avg_payment_amt, fill=DEFAULT.COLOR)) +
  geom_boxplot() +
  labs(title = "Average Payment Amount BoxPlot by Default")
  
  ggarrange(maxDLQ.boxplot,avg_payment_ratio.boxplot)

engineered
gt(as.data.frame(summary(tree.data)))

gt(basicStats(tree.data))




tree.train.vars <- c("avg_balance_paid",
                    "bal_growth_6mo",
                    "util_growth_6mo",
                    "max_bill_amt",
                    "max_pmt_amt", 
                    "average_utilization",
                    "avg_payment_ratio",
                    "avg_bill_amt",
                    "avg_payment_amt",
                    "DEFAULT",
                    "LIMIT_BAL", 
                    "max_DLQ",
                    "SEX_lessthanorEqual1",
                    "AGE",
                    "Marriage_lesthanorqualto1", 
                    "EDUCATION_lessthanorEqual1",
                    "train","test","validate")

finalClean <- c("avg_balance_paid",
                "bal_growth_6mo",
                "util_growth_6mo",
                "max_bill_amt",
                "max_pmt_amt", 
                "average_utilization",
                "avg_payment_ratio",
                "avg_bill_amt",
                "avg_payment_amt",
                "DEFAULT",
                "LIMIT_BAL", 
                "max_DLQ",
                "SEX_lessthanorEqual1",
                "AGE",
                "Marriage_lesthanorqualto1", 
                "EDUCATION_lessthanorEqual1")

finalClean2 <- c("avg_balance_paid",
                 "bal_growth_6mo",
                 "util_growth_6mo",
                 "max_bill_amt",
                 "max_pmt_amt", 
                 "average_utilization",
                 "avg_payment_ratio",
                 "avg_bill_amt",
                 "avg_payment_amt",
                 "DEFAULT",
                 "LIMIT_BAL", 
                 "max_DLQ",
                 "AGE")
tree.train.vars <- c("avg_balance_paid",
                     "bal_growth_6mo",
                     "util_growth_6mo",
                     "max_bill_amt",
                     "max_pmt_amt", 
                     "average_utilization",
                     "avg_payment_ratio",
                     "avg_bill_amt",
                     "avg_payment_amt",
                     "DEFAULT",
                     "LIMIT_BAL", 
                     "max_DLQ",
                     "SEX_lessthanorEqual1",
                     "AGE",
                     "Marriage_lesthanorqualto1", 
                     "EDUCATION_lessthanorEqual1",
                     "train","test","validate")

finalClean <- c("avg_balance_paid",
                "bal_growth_6mo",
                "util_growth_6mo",
                "max_bill_amt",
                "max_pmt_amt", 
                "average_utilization",
                "avg_payment_ratio",
                "avg_bill_amt",
                "avg_payment_amt",
                "DEFAULT",
                "LIMIT_BAL", 
                "max_DLQ",
                "SEX_lessthanorEqual1",
                "AGE",
                "Marriage_lesthanorqualto1", 
                "EDUCATION_lessthanorEqual1")

tree.vars <- c("avg_balance_paid",
               "bal_growth_6mo",
               "util_growth_6mo",
               "max_bill_amt",
               "max_pmt_amt", 
               "average_utilization",
               "avg_payment_ratio",
               "avg_bill_amt",
               "avg_payment_amt",
               "DEFAULT",
               "LIMIT_BAL", 
               "max_DLQ",
               "SEX_lessthanorEqual1",
               "AGE",
               "Marriage_lesthanorqualto1", 
               "EDUCATION_lessthanorEqual1")

reg.vars <- c( "LIMIT_BAL_Less30000",                      "SEX_lessthanorEqual1",
               "LIMIT_BAL_between30000and140000",           "LIMIT_BAL_Greater140000",                  
               "age_bin_Less_25",   "Marriage_lesthanorqualto1", "EDUCATION_lessthanorEqual1",
               "age_bin_greater_48",                        "avg_bill_amt_lessthan0",                   
               "avg_bill_amt_0to2510",                      "avg_bill_amt_2510to11861",                 
               "avg_bill_amt_11861to71812",                 "avg_bill_amt_11861to302472",               
               "avg_bill_amt_greaterThan302472",            "avg_pymt_amt_lessthan1333",                
               "avg_pymt_amt_1333to3000",                   "avg_pymt_amt_3000to12092",                 
               "avg_pymt_amt_greaterThan12092",             "avg_payment_ratio_lessthan6.87946722",     
               "avg_payment_ratio_6.87946722to29",          "avg_payment_ratio_greaterThan29",          
               "avg_util_lessthan0",                        "avg_util_.09to0.92",                       
               "avg_util_greaterthan51",                    "avg_balance_paid_lessthanNeg2",            
               "avg_balance_paid_neg2to11243.352",          "avg_balance_paid_11243to38253",           
               "avg_balance_paid_greaterThan38253",         "bal_growth_6mo_lessthaNeg11333",           
               "bal_growth_6mo_neg11333to734",              "bal_growth_6mo_greaterThan734",            
               "util_growth_6mo_lessthanNeg66.81695167",    "util_growth_6mo_Neg66.816951674toNeg0.075",
               "util_growth_6mo_Neg0.075To0",               "util_growth_6mo_0to4"        ,             
               "util_growth_6mo_greaterthan4",              "max_bill_amt_lessthan0"     ,              
               "max_bill_amt_0to2265.14",                   "max_bill_amt_2265.14to2500",               
               "max_bill_amt_2500to54974.66",               "max_bill_amt_greaterthan54974.66"  ,       
               "max_pmt_amt_lessthan4500",                  "max_pmt_amt_4500to36399.6",                
               "max_pmt_amt_greaterthan36399.6",            "max_DLQ_lessthan1"   ,                     
               "age_bin_26_and_48" ,                       
               "avg_util_0.92_to_51",
               "DEFAULT",
               "train",
               "test", "validate")


reg.vars.clean <-c( "LIMIT_BAL_Less30000",                      
                    "LIMIT_BAL_between30000and140000",           "LIMIT_BAL_Greater140000",                  
                    "age_bin_Less_25",                          
                    "age_bin_greater_48",                        "avg_bill_amt_lessthan0",                   
                    "avg_bill_amt_0to2510",                      "avg_bill_amt_2510to11861",                 
                    "avg_bill_amt_11861to71812",                 "avg_bill_amt_11861to302472",               
                    "avg_bill_amt_greaterThan302472",            "avg_pymt_amt_lessthan1333",                
                    "avg_pymt_amt_1333to3000",                   "avg_pymt_amt_3000to12092",                 
                    "avg_pymt_amt_greaterThan12092",             "avg_payment_ratio_lessthan6.87946722",     
                    "avg_payment_ratio_6.87946722to29",          "avg_payment_ratio_greaterThan29",          
                    "avg_util_lessthan0",                        "avg_util_.09to0.92",                       
                    "avg_util_greaterthan51",                    "avg_balance_paid_lessthanNeg2",            
                    "avg_balance_paid_neg2to11243.352",          "avg_balance_paid_11243to38253",           
                    "avg_balance_paid_greaterThan38253",         "bal_growth_6mo_lessthaNeg11333",           
                    "bal_growth_6mo_neg11333to734",              "bal_growth_6mo_greaterThan734",            
                    "util_growth_6mo_lessthanNeg66.81695167",    "util_growth_6mo_Neg66.816951674toNeg0.075",
                    "util_growth_6mo_Neg0.075To0",               "util_growth_6mo_0to4"        ,             
                    "util_growth_6mo_greaterthan4",              "max_bill_amt_lessthan0"     ,              
                    "max_bill_amt_0to2265.14",                   "max_bill_amt_2265.14to2500",               
                    "max_bill_amt_2500to54974.66",               "max_bill_amt_greaterthan54974.66"  ,       
                    "max_pmt_amt_lessthan4500",                  "max_pmt_amt_4500to36399.6",                
                    "max_pmt_amt_greaterthan36399.6",            "max_DLQ_lessthan1"   ,                     
                    "age_bin_26_and_48" ,                       
                    "avg_util_0.92_to_51",
                    "DEFAULT")

rf.train.data <- subset(data, select=tree.train.vars) %>% 
  subset(train == 1) %>% 
  subset(select=finalClean) %>% 
  as_tibble() 


rf.test.data <- subset(data, select=tree.train.vars) %>% 
  subset(test == 1) %>% 
  subset(select=finalClean) %>% 
  as_tibble()


validate.data <- subset(data, select=tree.train.vars) %>% 
  subset(validate == 1) %>% 
  subset(select=finalClean) %>% 
  as_tibble()

rf.train.data.2 <- rf.train.data %>% 
  subset(select=finalClean2)

rf.test.data.2 <- rf.test.data %>% 
  subset(select=finalClean2)

rf.train.data.3 <- data %>% 
  subset(select=reg.vars) %>% 
  subset(train == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble() 
rf.test.data.3 <- data %>% 
  subset(select=reg.vars) %>% 
  subset(test == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble() 
rf.validate.data.3 <- data %>% 
  subset(select=reg.vars) %>% 
  subset(validate == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble() 

lr.train.data <- data %>% 
  subset(select=reg.vars) %>% 
  subset(train == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble() 
lr.test.data <- data %>% 
  subset(select=reg.vars) %>% 
  subset(test == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble() 
lr.validate.dat <- data %>% 
  subset(select=reg.vars) %>% 
  subset(validate == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble() 

gb.train.data <- subset(data, select=tree.train.vars) %>% 
  subset(train == 1) %>% 
  subset(select=finalClean) %>% 
  as_tibble() 


gb.test.data <- subset(data, select=tree.train.vars) %>% 
  subset(test == 1) %>% 
  subset(select=finalClean) %>% 
  as_tibble()


gb.validate.data <- subset(data, select=tree.train.vars) %>% 
  subset(validate == 1) %>% 
  subset(select=finalClean) %>% 
  as_tibble()


gb.train.data$DEFAULT <- as.factor(gb.train.data$DEFAULT)
gb.test.data$DEFAULT <- as.factor(gb.test.data$DEFAULT)
gb.validate.data$DEFAULT <- as.factor(gb.validate.data$DEFAULT)

gb.train.data.2 <- gb.train.data %>% 
  subset(select=finalClean2)
gb.test.data.2 <- gb.test.data %>% 
  subset(select=finalClean2)
train.data$DEFAULT <- as.factor(train.data$DEFAULT)
test.data$DEFAULT <- as.factor(test.data$DEFAULT)
validate.data$DEFAULT <- as.factor(validate.data$DEFAULT)




tree.train.data <- data %>% 
  subset(select=reg.vars) %>% 
  subset(train == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble() 
lr.test.data <- data %>% 
  subset(select=reg.vars) %>% 
  subset(test == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble() 
lr.validate.dat <- data %>% 
  subset(select=reg.vars) %>% 
  subset(validate == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble()
tree.data <- subset(data, select=tree.vars)
names(tree.data)


#############
#Decision Tree
###############



data.tree <- rpart(DEFAULT ~ ., 
                   data = tree.data,method = "class",
                   control=rpart.control(minsplit=800,
                                         minbucket = 25,
                                         cp=0.001))

data.tree.2 <- rpart(DEFAULT ~ ., 
                     data = tree.train.data,method = "class",
                     control=rpart.control(minsplit=800,
                                           minbucket = 25,
                                           cp=0.001))

rpart.plot(data.tree)
rpart.plot(data.tree.2)
treevars <- corrr::correlate(tree.data)

corrplot(cor(tree.data))

data.cor = cor(tree.data, method = c("spearman"))
corrplot(data.cor, type = "lower", tl.col = "black", tl.srt = 45)


corrr::network_plot(treevars)
corrr::(treevars)


####
#oneR
####

onerdata <- data %>% 
  subset(select=reg.vars) %>% 
  subset(train == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble() 

model.1 <- OneR(DEFAULT ~ ., data=onerdata, verbose=TRUE)
stargazer(model.1)
plot(model.1)
model.1$cont_table
summary(model.1)
plot(model.1)
names(onerdata)
summary(model.1)

prediction.one.r <- predict(model.1, onerdata)
eval_model(prediction.one.r, onerdata)


rf.train.data.one.r <- rf.train.data.2 %>% 
  subset(select=-c(predicted))
model.2 <- OneR(DEFAULT ~., data = rf.train.data.one.r, verbose = TRUE)

##################
# 5. Predictive Modeling
###################
##################
# 5.1 Random Forest
###################

# 
#  cores <- 10
# registerDoParallel(cores = cores)
# #mtry: Number of random variables collected at each split. In normal equal square number columns.
# mtry <- sqrt(ncol(rf.train.data))
# #ntree: Number of trees to grow.
# ntree <- 10
# cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
# registerDoParallel(cluster) # register the parallel processing
# rf.train.data$DEFAULT <- as.factor(rf.train.data$DEFAULT)
# 
# RF.training <- randomForest(formula = as.factor(DEFAULT) ~ ., data = rf.train.data)
# varImpPlot(RF.training)
# 
# 
# 
# 
# 
# 
# RF.training2 <- randomForest(formula = as.factor(DEFAULT) ~ ., data = rf.train.data.2)
# varImpPlot(RF.training2)
# rf.train.data.3 <- rf.train.data.3 %>% 
#   subset(select=-c(predicted))
# RF.training3 <- randomForest(formula = as.factor(DEFAULT) ~ ., data = rf.train.data.3)
# varImpPlot(RF.training3)
# 
# rf.test.data$predicted  <- predict(RF.training, newdata = rf.test.data)
# rf.test.data.2$predicted <- predict(RF.training2, newdata = rf.test.data.2)
# rf.test.data.3$predicted <- predict(RF.training3, newdata = rf.test.data.3)
# 
# rf.train.data.3$predicted <- predict(RF.training3, newdata = rf.train.data.3)
# rf.test.data.3$predicted <- predict(RF.training3, newdata = rf.test.data.3)
# 
# # table_RF_test = table(rf.test.data$DEFAULT,rf.test.data$predicted) 
# table_RF2_train = table(rf.train.data.2$DEFAULT,rf.train.data.2$predicted) 
# table_RF2_test = table(rf.test.data.2$DEFAULT,rf.test.data.2$predicted) 
# table_RF3_train = table(rf.train.data.3$DEFAULT,rf.train.data.3$predicted) 
# table_RF3_test = table(rf.test.data.3$DEFAULT,rf.test.data.3$predicted) 
# 
# # roc.1.train <- roc(rf.train.data$DEFAULT, as.numeric(rf.train.data$predicted))
# roc.2.train <- roc(rf.train.data.2$DEFAULT, as.numeric(rf.train.data.2$predicted))
# roc.3.train <- roc(rf.train.data.3$DEFAULT, as.numeric(rf.train.data.3$predicted))
# # roc.1.test <- roc(rf.test.data$DEFAULT, as.numeric(rf.test.data$predicted))
# roc.2.test <- roc(rf.test.data$DEFAULT, as.numeric(rf.test.data.2$predicted))
# roc.3.test <- roc(rf.test.data$DEFAULT, as.numeric(rf.test.data.3$predicted))
# 
# print(roc.1)
# plot(roc.1)
# 
# 
# Random.Forest.Model.2 <-RF.training2
# varImpPlot(Random.Forest.Model.2)
# rf.test.data.3$predicted <- predict(RF.training3, newdata = rf.test.data.3)
# table_RF3_train = table(rf.test.data.3$DEFAULT,rf.test.data.3$predicted) 
# 
# 
# 
# rf.train.data.2$predicted <- predict(RF.training2, newdata = rf.train.data.2)
# roc.2.train <- roc(rf.train.data.2$DEFAULT, as.numeric(rf.train.data.2$predicted))
# roc.2 <- roc(rf.test.data.2$DEFAULT, as.numeric(rf.test.data.2$predicted))
# print(roc.2.train)
# print(roc.2)
# auc.2 <- auc(roc.2)
# plot(roc.2)
# title(main= "RF1 Test Curve", sub=paste("AUC Score:", as.character(auc.2)))
# plot(roc.2.train)
# title(main= "RF1 Train Curve", sub=paste("AUC Score:", as.character(auc.2)))
# 
# # Compute AUC
# auc.2 <- auc(roc.2);
# roc.specs.train <- coords(roc=roc.2.train,x=c('best'),
#                     input=c('threshold','specificity','sensitivity'),
#                     ret=c('threshold','specificity','sensitivity'),
#                     as.list=TRUE
# )
# roc.specs <- coords(roc=roc.2,x=c('best'),
#                     input=c('threshold','specificity','sensitivity'),
#                     ret=c('threshold','specificity','sensitivity'),
#                     as.list=TRUE
# )
# 
# rf.roc.specs <- rbind(as.data.frame(roc.specs.train),as.data.frame(roc.specs))
# rnames <- c("RF.Train","RF.Test")
# rf.roc.specs <- cbind(rnames,rf.roc.specs)
# gt(rf.roc.specs)
# 
# t <- table(rf.train.data.2$DEFAULT, rf.train.data.2$predicted);
# r <- apply(t,MARGIN=1,FUN=sum);
# t/r
# 
# t <- table(rf.test.data.2$DEFAULT, rf.test.data.2$predicted);
# r <- apply(t,MARGIN=1,FUN=sum);
# t/r
# 
# # 
# # clf.all.Vars <- train(DEFAULT ~ ., data = rf.train.data, method = "rf")
# # clf.allvars.pred <- predict(clf.all.Vars, newdata = rf.train.data, type = "prob" )
# # rf.train.data$clf.allvars.pred <- abs(as.numeric(clf.allvars.pred$'1'))
# 
# # 
# # names(data)
# # 
# 
# print(rf_random)
# plot(rf_random)
# set.seed(1)
# control <- trainControl(method='repeatedcv',
#                         number=10,
#                         repeats=3)
# rf_random <- train(as_factor(DEFAULT) ~ .,
#                    data=rf.train.data.3,
#                    method = 'rf',
#                    metric = 'Accuracy',
#                    tuneLength  = 15,
#                    trControl = control)
# print(rf_random)
# plot(rf_random)
# 
# rf.train.data.5 <- rf.train.data.2 %>% 
#   subset(select=-c(predicted))
# rf.test.data.5 <- rf.test.data.2 %>% 
#   subset(select=-c(predicted))
# 
# control <- trainControl(method='repeatedcv',
#                         number=10,
#                         repeats=3)
# rf_random <- train(as_factor(DEFAULT) ~ .,
#                    data=rf.train.data.5,
#                    method = 'rf',
#                    metric = 'Accuracy',
#                    tuneLength  = 15,
#                    trControl = control)
# 
# 
# rf.train.data.5$predicted <- predict(rf_random, newdata = rf.train.data.5)
# rf.test.data.5$predicted <- predict(rf_random, newdata = rf.test.data.5)
# 
# table_RF5_train = table(rf.train.data.5$DEFAULT,rf.train.data.5$predicted) 
# table_RF5_test = table(rf.test.data.5$DEFAULT,rf.test.data.5$predicted) 
# 
# roc.5.train <- roc(rf.train.data.5$DEFAULT, as.numeric(rf.train.data.5$predicted))
# roc.5.test <- roc(rf.test.data.5$DEFAULT, as.numeric(rf.test.data.5$predicted))
# 
# rf.train.data.4 <- rf.train.data.3 %>% 
#   subset(select=-c(predicted))
# rf.test.data.4 <- rf.test.data.3 %>% 
#   subset(select=-c(predicted))
# 
# rf.train.data.4$predicted <- predict(rf_random, newdata = rf.train.data.4)
# rf.test.data.4$predicted <- predict(rf_random, newdata = rf.test.data.4)
# 
# table_RF4_train = table(rf.train.data.4$DEFAULT,rf.train.data.4$predicted) 
# table_RF4_test = table(rf.test.data.4$DEFAULT,rf.test.data.4$predicted) 
# 
# roc.4.train <- roc(rf.train.data.4$DEFAULT, as.numeric(rf.train.data.4$predicted))
# roc.4.test <- roc(rf.test.data.4$DEFAULT, as.numeric(rf.test.data.4$predicted))
# 
# ##################
# # 5.1 Gradient Boost
# ###################
# 
# # Training Parameters
# CV_folds <- 5 # number of folds
# CV_repeats <- 3 # number of repeats
# minimum_resampling <- 5 # minimum number of resamples
# train_control <- caret::trainControl(method = "repeatedcv", number = CV_folds, repeats = CV_repeats, 
#                                      verboseIter = FALSE, returnData = FALSE) 
# 
# # trainControl object for repeated cross-validation with grid search
# adapt_control_grid <- caret::trainControl(method = "adaptive_cv", number = CV_folds, repeats = CV_repeats, 
#                                           adaptive = list(min = minimum_resampling, # minimum number of resamples tested before model is excluded
#                                                           alpha = 0.05, # confidence level used to exclude parameter settings
#                                                           method = "gls", # generalized least squares
#                                                           complete = TRUE), 
#                                           search = "grid", # execute grid search
#                                           verboseIter = FALSE, returnData = FALSE) 
# 
# # trainControl object for repeated cross-validation with random search
# adapt_control_random <- caret::trainControl(method = "adaptive_cv", number = CV_folds, repeats = CV_repeats, 
#                                             adaptive = list(min = minimum_resampling, # minimum number of resamples tested before model is excluded
#                                                             alpha = 0.05, # confidence level used to exclude parameter settings
#                                                             method = "gls", # generalized least squares
#                                                             complete = TRUE), 
#                                             search = "random", # execute random search
#                                             verboseIter = FALSE, returnData = FALSE) 
# # Create grid
# XGBoost_Linear_grid <- expand.grid(
#                                   nrounds = c(50, 100, 250, 500), # number of boosting iterations
#                                   eta = c(0.01, 0.1, 1),  # learning rate, low value means model is more robust to overfitting
#                                   lambda = c(0.1, 0.5, 1), # L2 Regularization (Ridge Regression)
#                                   alpha =  c(0.1, 0.5, 1) # L1 Regularization (Lasso Regression)
# ) 
# 
# GS_T0 <- Sys.time()
# cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
# registerDoParallel(cluster) # register the parallel processing
# 
# set.seed(1); 
# # Train model with grid search
# GS_XGBoost_Linear_model <- caret::train(DEFAULT ~., 
#                                         data = rf.train.data,
#                                         method = "xgbLinear",
#                                         trControl = adapt_control_grid,
#                                         verbose = FALSE, 
#                                         silent = 1,
#                                         # tuneLength = 20
#                                         tuneGrid = XGBoost_Linear_grid
# )
# 
# stopCluster(cluster) # shut down the cluster 
# registerDoSEQ(); #  force R to return to single threaded processing
# GS_T1 <- Sys.time()
# GS_T1-GS_T0
# 
# #https://rpubs.com/jeandsantos88/search_methods_for_hyperparameter_tuning_in_r
# 
# 
# 
# library(xgboost)
# 
# # Input to xgboost() must be a matrix;
# # Need to drop the response variable from the data frame before we 
# # convert it to a matrix.
# train.matrix <- as.matrix(rf.test.data[,-58]);
# #train.matrix[1:2,]
# 
# 
# rf.train.data.2
# 
# ####
# #random search
# ####
# # 
# # n_combinations <- nrow(XGBoost_Linear_grid)
# # RS_T0 <- Sys.time()
# # cluster <- makeCluster(detectCores() - 1) # number of cores, convention to leave 1 core for OS
# # registerDoParallel(cluster) # register the parallel processing
# # 
# # set.seed(1); 
# # RS_XGBoost_Linear_model <- caret::train(DEFAULT ~., 
# #                                         data = train.data,
# #                                         method = "xgbLinear",
# #                                         trControl = adapt_control_random,
# #                                         verbose = FALSE, 
# #                                         silent = 1,
# #                                         tuneLength = n_combinations
# # )
# # 
# # stopCluster(cluster) # shut down the cluster 
# # registerDoSEQ(); #force R to return to single threaded processing
# # RS_T1 <- Sys.time()
# # RS_T1-RS_T0
# # RS_XGBoost_Linear_model
# 
# 
# 
# 
# gb.train.matrix <- gb.train.data %>% 
#   subset(select=c(-DEFAULT)) %>% 
#   as.data.frame()
# gb.train.matrix <- as.matrix(gb.train.matrix)
# 
# gb.test.matrix <- gb.test.data %>% 
#   subset(select=c(-DEFAULT)) %>% 
#   as.data.frame()
# gb.test.matrix <- as.matrix(gb.test.matrix)
# 
# xg.10 <- xgboost(data=gb.train.matrix, label=as.numeric(gb.train.data$DEFAULT), max_depth=4,nrounds=10, objective='binary:logistic');
# 
# xg.100 <- xgboost(data=gb.train.matrix, label=gb.train.data$DEFAULT, max_depth=4,nrounds=100, objective='binary:logistic');
# 
# 
# # Plot variable importance;
# importance.10 <- xgb.importance(feature_names=colnames(gb.train.matrix),model=xg.10)
# xgb.plot.importance(importance.10, rel_to_first=TRUE, xlab='Relative Importance',
#                     main='XGBoost Model')
# 
# # Plot top 10 variables;
# xgb.plot.importance(importance.10[1:10], rel_to_first=TRUE, xlab='Relative Importance',
#                     main='XGBoost Model')
# # Plot variable importance;
# importance.100 <- xgb.importance(feature_names=colnames(gb.train.matrix),model=xg.100)
# xgb.plot.importance(importance.100, rel_to_first=TRUE, xlab='Relative Importance',
#                     main='XGBoost Model')
# 
# # Plot top 10 variables;
# xgb.plot.importance(importance.100[1:10], rel_to_first=TRUE, xlab='Relative Importance',
#                     main='XGBoost Model')
# 
# 
# xg.10.score <- predict(xg.10, newdata=gb.train.matrix);
# 
# roc.10 <- roc(response=gb.train.data$DEFAULT, predictor=xg.10.score)
# print(roc.10)
# roc.10$auc
# # Plot ROC Curve and add AUC to plot
# plot(roc.10)
# title(main= "XGB Train Curve")
# text(0.6,0.6,paste('AUC=',round(auc(roc.10),2),sep=''))
# xg.10.score.test <- predict(xg.10, newdata=gb.test.matrix);
# xg.10.score.test.2 <- predict(xg.10.2, newdata=gb.test.matrix);
# roc.10.test <- roc(response=gb.test.data$DEFAULT, predictor=xg.10.score.test)
# print(roc.10.test)
# roc.10.test$auc
# # Plot ROC Curve and add AUC to plot
# plot(roc.10.test)
# title(main= "XGB Test Curve")
# text(0.6,0.6,paste('AUC=',round(auc(roc.10.test),2),sep=''))
# 
# roc.specs.10 <- coords(roc=roc.10,x=c('best'),
#                        input=c('threshold','specificity','sensitivity'),
#                        ret=c('threshold','specificity','sensitivity'),
#                        as.list=TRUE
# )
# roc.specs.10.test <- coords(roc=roc.10.test,x=c('best'),
#                        input=c('threshold','specificity','sensitivity'),
#                        ret=c('threshold','specificity','sensitivity'),
#                        as.list=TRUE
# )
# xg.10.class <- ifelse(xg.10.score>roc.specs.10$threshold,1,0);
# xg.10.class.test <- ifelse(xg.10.score.test>roc.specs.10.test$threshold,1,0);
# 
# 
# t <- table(gb.train.data$DEFAULT,xg.10.class);
# # Let's create a proper confusion matrix
# t <- table(gb.test.data$DEFAULT,xg.10.class.test);
# # Compute row totals;
# r <- apply(t,MARGIN=1,FUN=sum);
# # Normalize confusion matrix to rates;
# t/r
# # 
# # Let's create a proper confusion matrix
# t <- table(gb.test.data$DEFAULT,xg.10.class.test);
# # Compute row totals;
# r <- apply(t,MARGIN=1,FUN=sum);
# # Normalize confusion matrix to rates;
# t/r
# 
# 
# # Let's create a proper confusion matrix
# t <- table(gb.train.data$DEFAULT,xg.10.score.2);
# # Compute row totals;
# r <- apply(t,MARGIN=1,FUN=sum);
# # Normalize confusion matrix to rates;
# t/r
# 
# 
# 
# xgb.roc.specs <- rbind(as.data.frame(roc.specs.10),as.data.frame(roc.specs.10.test))
# rnames <- c("XGB.Train","XGB.Test")
# xgb.roc.specs <- cbind(rnames,xgb.roc.specs)
# gt(xgb.roc.specs)
# 
# 
# 
# xg.100.score <- predict(xg.100, newdata=gb.train.matrix);
# xg.100.score.test <- predict(xg.100, newdata=gb.test.matrix);
# 
# roc.100 <- roc(response=gb.train.data$DEFAULT, predictor=xg.100.score)
# print(roc.100)
# t <- table(gb.test.data$DEFAULT,xg.100.score.test);
# 
# 
# 
# 
# gb.train.data.3 <- rf.train.data.3
# gb.test.data.3 <- rf.test.data.3
# 
# gb.train.matrix.3 <- gb.train.data.3 %>% 
#   subset(select=c(-DEFAULT)) %>% 
#   as.data.frame()
# gb.train.matrix.3 <- as.matrix(gb.train.matrix.3)
# 
# gb.test.matrix.3 <- gb.test.data.3 %>% 
#   subset(select=c(-DEFAULT)) %>% 
#   as.data.frame()
# gb.test.matrix.3 <- as.matrix(gb.test.matrix.3)
# 
# xg.10.3 <- xgboost(data=gb.train.matrix.3, label=gb.train.data.3$DEFAULT, max_depth=4,nrounds=10, objective='binary:hinge');
# xg.100.3 <- xgboost(data=gb.train.matrix.3, label=gb.train.data.3$DEFAULT, max_depth=4,nrounds=100, objective='binary:hinge');
# 
# importance.100.3 <- xgb.importance(feature_names=colnames(gb.train.matrix.3),model=xg.100.3)
# xgb.plot.importance(importance.100.3, rel_to_first=TRUE, xlab='Relative Importance',
#                     main='XGBoost Model 2')
# 
# # Plot top 10 variables;
# xgb.plot.importance(importance.100.3[1:10], rel_to_first=TRUE, xlab='Relative Importance',
#                     main='XGBoost Model 2')
# 
# importance.10.3 <- xgb.importance(feature_names=colnames(gb.train.matrix.3),model=xg.10.3)
# xgb.plot.importance(importance.10.3, rel_to_first=TRUE, xlab='Relative Importance',
#                     main='XGBoost Model 2')
# 
# # Plot top 10 variables;
# xgb.plot.importance(importance.10.3[1:10], rel_to_first=TRUE, xlab='Relative Importance',
#                     main='XGBoost Model 2')
# 
# 
# xg.10.3.train <- predict(xg.10.3, newdata=gb.train.matrix.3)
# xg.10.3.test <- predict(xg.10.3, newdata=gb.test.matrix.3)
# 
# roc.10.3.train <- roc(response=gb.train.data.3$DEFAULT, predictor=xg.10.3.train)
# roc.10.3.test <- roc(response=gb.test.data.3$DEFAULT, predictor=xg.10.3.test)
# 
# 
# roc.specs.10 <- coords(roc=roc.10,x=c('best'),
#                        input=c('threshold','specificity','sensitivity'),
#                        ret=c('threshold','specificity','sensitivity'),
#                        as.list=TRUE
# )
# roc.specs.10.test <- coords(roc=roc.10.test,x=c('best'),
#                             input=c('threshold','specificity','sensitivity'),
#                             ret=c('threshold','specificity','sensitivity'),
#                             as.list=TRUE
# )
# 
# table_XGB3_train = table(gb.train.data.3$DEFAULT,xg.10.3.train) 
# table_XGB3_test = table(gb.test.data.3$DEFAULT,xg.10.3.test) 



#############
# Logistic Regression with Variable Selection
#- Random Forest and Gradient Boosting will identify a pool of interesting predictor variables. 
#Use that information to help you choose an initial pool of predictor variables. 
#List your initial pool of predictor variables in a table.
#- Choose a variable selection algorithm. Use that variable selection 
#algorithm to arrive at an ‘optimal’ logistic regression model.
#- Since this is a linear model, you should provide a table of the model 
#coefficients and their p-values.#
##############

##################
for (col in names(data)) {
  print(col)
  
}


# reg.vars <- c("max_DLQ","avg_payment_ratio","average_utilization","util_growth_6mo",
#               "bal_growth_6mo","max_pmt_amt","max_bill_amt","sum_pay_AMT","avg_payment_amt",
#               "avg_balance_paid", "LIMIT_BAL","avg_bill_amt","sum_bill_AMT", "age_bin_26_and_35",
#               "age_bin_36_and_45","2-EDUCATION", "1-EDUCATION","1-MARRIAGE", "1-SEX", "2-SEX")
# 
# 
# reg.vars <- c("max_DLQ","avg_payment_ratio","average_utilization","util_growth_6mo",
#               "bal_growth_6mo","max_pmt_amt","max_bill_amt","sum_pay_AMT","avg_payment_amt",
#               "avg_balance_paid", "LIMIT_BAL","avg_bill_amt","sum_bill_AMT", "age_bin_26_and_35",
#               "age_bin_36_and_45","2-EDUCATION", "1-EDUCATION","1-MARRIAGE", "1-SEX", "2-SEX")
names(data)


# reg.vars <- c( "LIMIT_BAL_Less30000",
#                "LIMIT_BAL_between30000and160000",
#                "LIMIT_BAL_Greater160000",
#                "SEX_lessthanorEqual1",
#                "EDUCATION_lessthanorEqual1",
#                "Marriage_lesthanorqualto1",
#                "age_bin_Less_25",
#                "age_bin_26_and_48",
#                "age_bin_greater_48",
#                "avg_bill_amt_lessthan0",
#                "avg_bill_amt_2510to11861",
#                "avg_bill_amt_11861to71812",
#                "avg_bill_amt_11861to302472",
#                "avg_bill_amt_greaterThan302472",
#                "avg_pymt_amt_lessthan1333",
#                "avg_pymt_amt_2475to12935",
#                "avg_pymt_amt_3000to12092",
#                "avg_pymt_amt_greaterThan12092",
#                "avg_payment_ratio_lessthan6.87946722",
#                "avg_payment_ratio_6.87946722to29",
#                "avg_payment_ratio_greaterThan29",
#                "avg_util_lessthan0",
#                "avg_util_.09to0.92",
#                "avg_util_0.92_to_51",
#                "avg_util_greaterthan51",
#                "avg_balance_paid_lessthanNeg2",
#                "avg_balance_paid_neg2to11243.352",
#                "avg_balance_paid_11243to38253",
#                "avg_balance_paid_greaterThan38253",
#                "bal_growth_6mo_lessthaNeg11333",
#                "bal_growth_6mo_neg11333to734",
#                "bal_growth_6mo_greaterThan734",
#                "util_growth_6mo_lessthanNeg66.81695167",
#                "util_growth_6mo_Neg66.816951674toNeg0.075",
#                "util_growth_6mo_Neg0.075To0",
#                "util_growth_6mo_0to4",
#                "util_growth_6mo_greaterthan4",
#                "max_bill_amt_lessthan0",
#                "max_bill_amt_0to2265.14",
#                "max_bill_amt_2265.14to2500",
#                "max_bill_amt_2500to54974.66",
#                "max_bill_amt_greaterthan54974.66",
#                "max_pmt_amt_lessthan4500",
#                "max_pmt_amt_4500to36399.6",
#                "max_pmt_amt_greaterthan36399.6",
#                "max_DLQ_greaterthan1",
#                "sum_pay_AMT_lessthan8000",
#                "sum_pay_AMT_8000to18000",
#                "sum_pay_AMT_18000to72555",
#                "sum_pay_AMT_greaterthan72555",
#                "sum_bill_AMT_lessthan0",
#                "sum_bill_AMT_0to15065.06",
#                "sum_bill_AMT_15065.06to71169.56",
#                "sum_bill_AMT_71169.56to430875",
#                "sum_bill_AMT_430875to1814836.92", 
#                "sum_bill_AMT_greaterthan1814836.926",
#                "DEFAULT",
#                "train",
#                "test", "validate")
# reg.vars.clean <- c( "bal_growth_6mo",
#                                     "LIMIT_BAL_Less30000",
#                                     "LIMIT_BAL_between30000and160000",
#                                     "LIMIT_BAL_Greater160000",
#                                     "SEX_lessthanorEqual1",
#                                     "EDUCATION_lessthanorEqual1",
#                                     "Marriage_lesthanorqualto1",
#                                     "age_bin_Less_25",
#                                     "age_bin_26_and_48",
#                                     "age_bin_greater_48",
#                                     "avg_bill_amt_lessthan0",
#                                     "avg_bill_amt_2510to11861",
#                                     "avg_bill_amt_11861to71812",
#                                     "avg_bill_amt_11861to302472",
#                                     "avg_bill_amt_greaterThan302472",
#                                     "avg_pymt_amt_lessthan1333",
#                                     "avg_pymt_amt_2475to12935",
#                                     "avg_pymt_amt_3000to12092",
#                                     "avg_pymt_amt_greaterThan12092",
#                                     "avg_payment_ratio_lessthan6.87946722",
#                                     "avg_payment_ratio_6.87946722to29",
#                                     "avg_payment_ratio_greaterThan29",
#                                     "avg_util_lessthan0",
#                                     "avg_util_.09to0.92",
#                                     "avg_util_0.92_to_51",
#                                     "avg_util_greaterthan51",
#                                     "avg_balance_paid_lessthanNeg2",
#                                     "avg_balance_paid_neg2to11243.352",
#                                     "avg_balance_paid_11243to38253",
#                                     "avg_balance_paid_greaterThan38253",
#                                     "bal_growth_6mo_lessthaNeg11333",
#                                     "bal_growth_6mo_neg11333to734",
#                                     "bal_growth_6mo_greaterThan734",
#                                     "util_growth_6mo_lessthanNeg66.81695167",
#                                     "util_growth_6mo_Neg66.816951674toNeg0.075",
#                                     "util_growth_6mo_Neg0.075To0",
#                                     "util_growth_6mo_0to4",
#                                     "util_growth_6mo_greaterthan4",
#                                     "max_bill_amt_lessthan0",
#                                     "max_bill_amt_0to2265.14",
#                                     "max_bill_amt_2265.14to2500",
#                                     "max_bill_amt_2500to54974.66",
#                                     "max_bill_amt_greaterthan54974.66",
#                                     "max_pmt_amt_lessthan4500",
#                                     "max_pmt_amt_4500to36399.6",
#                                     "max_pmt_amt_greaterthan36399.6",
#                                     "max_DLQ_greaterthan1",
#                                     "sum_pay_AMT_lessthan8000",
#                                     "sum_pay_AMT_8000to18000",
#                                     "sum_pay_AMT_18000to72555",
#                                     "sum_pay_AMT_greaterthan72555",
#                                     "sum_bill_AMT_lessthan0",
#                                     "sum_bill_AMT_0to15065.06",
#                                     "sum_bill_AMT_15065.06to71169.56",
#                                     "sum_bill_AMT_71169.56to430875",
#                                     "sum_bill_AMT_430875to1814836.92", 
#                                     "sum_bill_AMT_greaterthan1814836.926",
#                                     "DEFAULT")


# stepwise forward regression

model <- glm(DEFAULT ~ ., data = rf.train.data)
rf.train.data$DEFAULT <- as.numeric(rf.train.data$DEFAULT)

model2 <- glm(DEFAULT ~ ., data = lr.train.data)
lr.train.data$DEFAULT <- as.numeric(lr.train.data$DEFAULT)


# model3 <- glm(DEFAULT ~ LIMIT_BAL_Less30000 + LIMIT_BAL_between30000and140000 + 
#                 LIMIT_BAL_Greater140000 + age_bin_Less_25 + age_bin_greater_48 + 
#                 avg_bill_amt_lessthan0 + avg_bill_amt_greaterThan302472 + 
#                 avg_pymt_amt_lessthan1333 + avg_pymt_amt_greaterThan12092 + 
#                 avg_payment_ratio_lessthan6.87946722 + avg_payment_ratio_6.87946722to29 + 
#                 avg_util_.09to0.92 + avg_balance_paid_lessthanNeg2 + avg_balance_paid_neg2to11243.352 + 
#                 bal_growth_6mo_lessthaNeg11333 + util_growth_6mo_lessthanNeg66.81695167 + 
#                 util_growth_6mo_Neg66.816951674toNeg0.075 + util_growth_6mo_0to4 + 
#                 max_bill_amt_0to2265.14 + max_bill_amt_2265.14to2500 + max_pmt_amt_lessthan4500 + 
#                 max_pmt_amt_4500to36399.6 + max_DLQ_lessthan1, data = lr.train.data)

model3 <- glm(DEFAULT ~ LIMIT_BAL_Less30000 + LIMIT_BAL_between30000and140000 + 
                LIMIT_BAL_Greater140000 + age_bin_greater_48 + avg_bill_amt_lessthan0 + 
                avg_bill_amt_greaterThan302472 + avg_pymt_amt_lessthan1333 + 
                avg_pymt_amt_greaterThan12092 + avg_payment_ratio_lessthan6.87946722 + 
                avg_payment_ratio_6.87946722to29 + avg_util_.09to0.92 + avg_balance_paid_lessthanNeg2 + 
                avg_balance_paid_neg2to11243.352 + bal_growth_6mo_lessthaNeg11333 + 
                util_growth_6mo_lessthanNeg66.81695167 + util_growth_6mo_Neg66.816951674toNeg0.075 + 
                util_growth_6mo_0to4 + max_bill_amt_2265.14to2500 + max_pmt_amt_lessthan4500 + 
                max_pmt_amt_4500to36399.6 + max_DLQ_lessthan1, data = lr.train.data)

backward_glm <- stepAIC(model3,direction="backward",trace=FALSE)
# both_glm <- stepAIC(model2,direction="both",trace=TRUE)
# forward_glm <- stepAIC(model2,direction="forward",trace=TRUE)

backward_glm$coefficients
backward_glm$anova
names <- backward_glm$coefficients

# gt(as.data.frame(backward_glm$coefficients))
# lr.train.data$DEFAULT <- as.factor(lr.train.data$DEFAULT)
# fit.GLM2 <- train(DEFAULT ~ LIMIT_BAL_Less30000 + LIMIT_BAL_between30000and140000 + 
#                     LIMIT_BAL_Greater140000 + age_bin_Less_25 + age_bin_greater_48 + 
#                     avg_bill_amt_lessthan0 + avg_bill_amt_greaterThan302472 + 
#                     avg_pymt_amt_lessthan1333 + avg_pymt_amt_greaterThan12092 + 
#                     avg_payment_ratio_lessthan6.87946722 + avg_payment_ratio_6.87946722to29 + 
#                     avg_util_.09to0.92 + avg_balance_paid_lessthanNeg2 + avg_balance_paid_neg2to11243.352 + 
#                     bal_growth_6mo_lessthaNeg11333 + util_growth_6mo_lessthanNeg66.81695167 + 
#                     util_growth_6mo_Neg66.816951674toNeg0.075 + util_growth_6mo_Neg0.075To0 + 
#                     util_growth_6mo_0to4 + max_bill_amt_0to2265.14 + max_bill_amt_2265.14to2500 + 
#                     max_pmt_amt_lessthan4500 + max_pmt_amt_4500to36399.6 + max_DLQ_lessthan1 + 
#                     age_bin_26_and_48 + avg_util_0.92_to_51,
#                   data = lr.train.data,family="binomial",method = "glm",trControl=control)


# lr.train.data$DEFAULT <- as.factor(lr.train.data$DEFAULT)
# fit.GLM3 <- train(DEFAULT ~ LIMIT_BAL_Less30000 + LIMIT_BAL_between30000and140000 + 
#                     LIMIT_BAL_Greater140000 + age_bin_Less_25 + age_bin_greater_48 + 
#                     avg_bill_amt_lessthan0 + avg_bill_amt_greaterThan302472 + 
#                     avg_pymt_amt_lessthan1333 + avg_pymt_amt_greaterThan12092 + 
#                     avg_payment_ratio_lessthan6.87946722 + avg_payment_ratio_6.87946722to29 + 
#                     avg_util_.09to0.92 + avg_balance_paid_lessthanNeg2 + avg_balance_paid_neg2to11243.352 + 
#                     bal_growth_6mo_lessthaNeg11333 + util_growth_6mo_lessthanNeg66.81695167 + 
#                     util_growth_6mo_Neg66.816951674toNeg0.075 + util_growth_6mo_0to4 + 
#                     max_bill_amt_0to2265.14 + max_bill_amt_2265.14to2500 + max_pmt_amt_lessthan4500 + 
#                     max_pmt_amt_4500to36399.6 + max_DLQ_lessthan1,
#                   data = lr.train.data,family="binomial",method = "glm",trControl=control)

fit.GLM3 <- glm(DEFAULT ~ LIMIT_BAL_Less30000 + LIMIT_BAL_between30000and140000 + LIMIT_BAL_Greater140000 + age_bin_greater_48 + avg_bill_amt_lessthan0 + avg_bill_amt_greaterThan302472 + avg_pymt_amt_lessthan1333 + avg_pymt_amt_greaterThan12092 + avg_payment_ratio_lessthan6.87946722 + avg_payment_ratio_6.87946722to29 + avg_util_.09to0.92 + avg_balance_paid_lessthanNeg2 + avg_balance_paid_neg2to11243.352 + bal_growth_6mo_lessthaNeg11333 + util_growth_6mo_lessthanNeg66.81695167 + util_growth_6mo_Neg66.816951674toNeg0.075 + util_growth_6mo_0to4 + max_bill_amt_2265.14to2500 + max_pmt_amt_lessthan4500 + max_pmt_amt_4500to36399.6 + max_DLQ_lessthan1,data = lr.train.data,family = "binomial")

lr.train.data$DEFAULT <- as.factor(lr.train.data$DEFAULT)

lr.test.data$DEFAULT <- as.factor(lr.test.data$DEFAULT)
# predGLM2_prob_train <- predict(fit.GLM, newdata = lr.train.data, type = "prob")
# predGLM2_prob <- predict(fit.GLM2, newdata = lr.test.data, type = "prob")
# lr.test.data$predGLM2_prob <- abs(as.numeric(predGLM2_prob$'1'))
# lr.train.data$predGLM2_prob <- abs(as.numeric(predGLM2_prob_train$'1'))

predGLM3_prob_train <- predict(fit.GLM3, newdata = lr.train.data, type = "response")
predGLM3_prob <- predict(fit.GLM3, newdata = lr.test.data, type = "response")
# lr.test.data$predGLM3_prob <- abs(as.numeric(predGLM3_prob$'1'))
# lr.train.data$predGLM3_prob <- abs(as.numeric(predGLM3_prob_train$'1'))
lr.train.data$predGLM3_prob <- predGLM3_prob_train
lr.test.data$predGLM3_prob <- predGLM3_prob
glm.roc.3 <- roc(response=lr.train.data$DEFAULT, predictor=lr.train.data$predGLM3_prob)
glm.roc.3.test <- roc(response=lr.test.data$DEFAULT, predictor=lr.test.data$predGLM3_prob)

print(glm.roc.3)
plot(glm.roc.3)
title(main= "LR Train Curve
      
      ")
text(0.6,0.6,paste('AUC=',round(auc(glm.roc.3),2),sep=''))
roc.specs.train.glm.3 <- coords(roc=glm.roc.3,x=c('best'),
                              input=c('threshold','specificity','sensitivity'),
                              ret=c('threshold','specificity','sensitivity'),
                              as.list=TRUE
)

plot(glm.roc.3.test)
title(main= "LR Test Curve
      
      ")
text(0.6,0.6,paste('AUC=',round(auc(glm.roc.3),2),sep=''))
roc.specs.train.glm.3 <- coords(roc=glm.roc.3,x=c('best'),
                                input=c('threshold','specificity','sensitivity'),
                                ret=c('threshold','specificity','sensitivity'),
                                as.list=TRUE
)

roc.specs.glm.3.class <- ifelse(lr.train.data$predGLM3_prob>roc.specs.train.glm.3$threshold,1,0);
roc.specs.glm.3.class.test<- ifelse(lr.test.data$predGLM3_prob>roc.specs.train.glm.3$threshold,1,0);
t <- table(lr.train.data$DEFAULT,roc.specs.glm.3.class);
r <- apply(t,MARGIN=1,FUN=sum);
t/r

t <- table(lr.test.data$DEFAULT,roc.specs.glm.3.class.test);
r <- apply(t,MARGIN=1,FUN=sum);
t/r

anova(fit.GLM3)
out.path <- "/Users/ryanlee/code/NorthwesternMSDS/MSDS498 - Capstone Modelling/Capstone/"
file.name <- "glmModel.html";

stargazer(fit.GLM3, type=c('html'),out=paste(out.path,file.name,sep=""),
          title=c('Logistic Regression Model'),
          align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE)

stargazer(fit.GLM3, title=c('Logistic Regression Model'),align=TRUE, digits=2, digits.extra=2, initial.zero=TRUE)
# 
# 
# glm.roc <- roc(response=lr.train.data$DEFAULT, predictor=roc.specs.glm.class)
# print(glm.roc)
# plot(glm.roc)
# title(main= "LR Train Curve")
# text(0.6,0.6,paste('AUC=',round(auc(glm.roc$auc),2),sep=''))
# 
# roc.specs.glm.class <- ifelse(lr.train.data$predGLM2_prob>roc.specs.glm$threshold,1,0);
# roc.specs.glm.class.test<- ifelse(lr.test.data$predGLM2_prob>roc.specs.glm.test$threshold,1,0);
# 
# glm.roc<- roc(response=lr.train.data$DEFAULT, predictor=roc.specs.glm.class)
# 
# glm.roc.test <- roc(response=lr.test.data$DEFAULT, predictor=roc.specs.glm.class.test)
# print(glm.roc)
# plot(glm.roc)
# glm.roc$auc
# title(main= "LR Test Curve")
# text(0.6,0.6,paste('AUC=',round(auc(glm.roc),2),sep=''))
# 
# text(1,1, label = paste("AUC Score:", as.character( round(auc(glm.roc),3))))
# 
# 
# roc.specs.train.glm <- coords(roc=glm.roc,x=c('best'),
#                           input=c('threshold','specificity','sensitivity'),
#                           ret=c('threshold','specificity','sensitivity'),
#                           as.list=TRUE
# )
# roc.specs.glm.test <- coords(roc=glm.roc.test,x=c('best'),
#                     input=c('threshold','specificity','sensitivity'),
#                     ret=c('threshold','specificity','sensitivity'),
#                     as.list=TRUE
# )
# 
# 
# 
# # Let's create a proper confusion matrix
# t <- table(lr.train.data$DEFAULT,roc.specs.glm.class);
# # Compute row totals;
# r <- apply(t,MARGIN=1,FUN=sum);
# # Normalize confusion matrix to rates;
# t/r
# 
# # Let's create a proper confusion matrix
# t <- table(lr.test.data$DEFAULT,roc.specs.glm.class.test);
# # Compute row totals;
# r <- apply(t,MARGIN=1,FUN=sum);
# # Normalize confusion matrix to rates;
# t/r
# 
# lr.roc.specs <- rbind(as.data.frame(roc.specs.train.glm),as.data.frame(roc.specs.glm.test))
# rnames <- c("GLM.Train","GLM.Test")
# lr.roc.specs <- cbind(rnames,lr.roc.specs)
# gt(lr.roc.specs)
# 







# 
# # Get AUC
# text(1,1, label = paste("AUC Score:", as.character( auc(glm.roc))))
# 
# 
# lr.test.data$classes <- ifelse(lr.test.data$predGLM2_prob >.25,1,0)
# 
# GLM2_train = table(lr.test.data$DEFAULT,lr.test.data$classes) 
# 
# 
# 
# # Predicting probability of survival using predict type 'prob'
# predGLM2_prob <- predict(backward_glm, newdata = lr.test.data, type = "prob")
# 
# #create column with likelihood factor
# xtrain_GLM2$predGLM2_prob <- abs(as.numeric(predGLM2_prob$'1'))
# summary(xtrain_GLM2$predGLM2_prob)




check <- cor(train.data)
corrplot(check)
full_glm <- glm(DEFAULT ~., data=train.data)

summary(full_glm)
data.cor = cor(train.data, method = c("spearman"))
corrplot(data.cor, type = "lower", tl.col = "black", tl.srt = 45)








########
#neural net
########
write_csv(data,"../python/all.csv")
X_train <- data %>% 
  subset(select=reg.vars) %>% 
  subset(train == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble() 
write_csv(X_train, "../python/train.csv")
y_train <- data %>% 
  subset(select=reg.vars) %>% 
  subset(train == 1) %>% 
  subset(select=c("DEFAULT")) %>% 
  as_tibble() 
write_csv(y_train, "../python/trainY.csv")

X_test <- data %>% 
  subset(select=reg.vars) %>% 
  subset(test == 1) %>% 
  subset(select=reg.vars.clean) %>% 
  as_tibble() 
write_csv(X_test, "../python/testX.csv")
y_test <- data %>% 
  subset(select=reg.vars) %>% 
  subset(test == 1) %>% 
  subset(select=c("DEFAULT")) %>% 
  as_tibble() 
write_csv(y_test, "../python/testY.csv")

# 
# 
# 
# # Network design
# model <- keras_model_sequential()
# model %>%
#   # Input layer
#   layer_dense(units = 256, activation = "relu", input_shape =  ncol(X_train)) %>% 
#   layer_dropout(rate = 0.4) %>% 
#   # Hidden layer
#   layer_dense(units = 75, activation = "relu") %>%
#   # Output layer
#   layer_dropout(rate = 0.3) %>%
#   layer_dense(units = 2, activation = "sigmoid")
# # Network config
# history <- model %>% compile(
#   loss = 'binary_crossentropy',
#   optimizer = 'adam',
#   metrics = c("accuracy"))
# # Running our data
# model %>% fit(
#   X_train, y_train, 
#   epochs = 100, 
#   batch_size = 5,
#   validation_split = 0.3
# )
# summary(model)


#https://stats.stackexchange.com/questions/386765/regression-or-classification-in-neural-networks
#Max-Min Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- as.data.frame(lapply(data, normalize))
attach(maxmindf)

nn <- neuralnet(y_train$DEFAULT ~ ., data=X_train, hidden=c(5,5), rep =10, linear.output=FALSE, threshold=0.01, err.fct = "sse", act.fct = "logistic")
nn$result.matrix
plot(nn)

FE <- read_csv("../python/FE.csv")
gt(FE)

py_nn_output <- read_csv("../python/nn_output.csv")
py_nn_output_train <- read_csv("../python/nn_output_train.csv")
nn_train <- py_nn_output_train$`0`
nn_test <- py_nn_output$`0`

# Let's create a proper confusion matrix
t <- table(lr.train.data$DEFAULT,nn_train);
t <- table(lr.test.data$DEFAULT,nn_test);

# Compute row totals;
r <- apply(t,MARGIN=1,FUN=sum);
# Normalize confusion matrix to rates;
t/r



nn.roc <- roc(response=lr.train.data$DEFAULT, predictor=nn_train)
print(nn.roc)
plot(nn.roc)
title(main= "NN Train Curve")
text(0.6,0.6,paste('AUC=',round(auc(nn.roc),2),sep=''))
nn.roc.test <- roc(response=lr.test.data$DEFAULT, predictor=nn_test)
print(nn.roc)
plot(nn.roc)
title(main= "NN Test Curve")
text(0.6,0.6,paste('AUC=',round(auc(nn.roc),2),sep=''))


nn.roc<- roc(response=lr.train.data$DEFAULT, predictor=roc.specs.nn.class)

nn.roc.test <- roc(response=lr.test.data$DEFAULT, predictor=roc.specs.nn.class.test)
print(nn.roc)
plot(nn.roc)
nn.roc$auc
title(main= "LR Test Curve")
text(0.6,0.6,paste('AUC=',round(auc(nn.roc),2),sep=''))

text(1,1, label = paste("AUC Score:", as.character( round(auc(nn.roc),3))))


roc.specs.train.nn <- coords(roc=nn.roc,x=c('best'),
                              input=c('threshold','specificity','sensitivity'),
                              ret=c('threshold','specificity','sensitivity'),
                              as.list=TRUE
)

roc.specs.nn.test <- coords(roc=nn.roc.test,x=c('best'),
                             input=c('threshold','specificity','sensitivity'),
                             ret=c('threshold','specificity','sensitivity'),
                             as.list=TRUE
)

nn.roc.specs <- rbind(as.data.frame(roc.specs.train.nn),as.data.frame(roc.specs.nn.test))
rnames <- c("NN.Train","NN.Test")
nn.roc.specs <- cbind(rnames,nn.roc.specs)
gt(nn.roc.specs)

gt(rbind(rf.roc.specs,xgb.roc.specs,lr.roc.specs,nn.roc.specs))

corrplot(cor(rf.train.data), type = "lower", tl.col = "black", tl.srt = 45)






##########
#linear regression ks
############

lr.train.data$predicted <- roc.specs.glm.3.class
lr.test.data$predicted <- roc.specs.glm.3.class.test


table(lr.train.data$DEFAULT)
table(lr.train.data$predicted)

ks.df <- cbind(lr.train.data$predGLM3_prob,lr.train.data$DEFAULT)
colnames(ks.df) <- c("model.score","response")
ks.df <- as.data.frame(ks.df)
decile.pts <- quantile(ks.df$model.score,
                       probs=c(0.05,0.1,0.15,0.2,0.25,
                               0.3,0.35,0.4,0.45,
                               0.5,0.55,0.6,0.65,
                               0.7,0.75,0.8,0.85,0.9,0.95));

ks.df$model.decile <- cut(ks.df$model.score,breaks=c(0,decile.pts,1),
                          labels=rev(c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20'))
);


aggregate(ks.df$model.score,by=list(Decile=ks.df$model.decile),FUN=min);
table(ks.df$model.decile)
table(ks.df$model.decile,ks.df$response)
ks.table <- as.data.frame(list(Y0=table(ks.df$model.decile,ks.df$response)[,1],
                               Y1=table(ks.df$model.decile,ks.df$response)[,2],
                               Decile=rev(c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20'))
));
ks.table[order(ks.table$Decile),]
# set.seed(123)
# model.score <- runif(n=10000,min=0,max=1);
# v <- runif(n=10000,min=0,max=1);
# response <- ifelse(v<0.25,1,0);
# table(response)
# 
# # Create a data frame for model.score and response
# my.df <- as.data.frame(cbind(model.score,response));
# head(my.df)

