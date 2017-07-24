# project
library(sqldf)
library(ggplot2)

loan <- read.table('fhlmc_sf2015a_loans.txt')
loan_1 <- read.table('fnma_sf2015a_loans.txt')
data <- rbind(loan,loan_1)
x<-c("Flag","no","MSA","Cen_Track","Income","Brw_inc_r","LTV","Purpose","Fed_Grt","Ethnicity","Co_Eth","Bor_Gender","Co_Bor_Gender","num_unit","unit")
colnames(data) <- x
str(data)
data <- data.frame(apply(data,2,as.factor))
cols.dont.want <- c("Flag","no","num_unit")
d_1 <- data[, ! names(data) %in% cols.dont.want, drop = F]

q_inc <- sqldf("select MSA, Income, Brw_inc_r, count(*) as count from d_1 group by MSA, Income, Brw_inc_r")
str(q_inc)
sum_MSA <- sqldf("select MSA, sum(count) as num from q_inc group by MSA ")
sum_Income <- sqldf("select Income, sum(count) as num from q_inc group by Income ")
sum_Brw_inc_r <- sqldf("select Brw_inc_r , sum(count) as num from q_inc group by Brw_inc_r  ")
MSA_Inc <- sqldf("select MSA, Income, sum(count) as num from q_inc group by MSA,Income ")
MSA_Brw_inc <- sqldf("select MSA, Brw_inc_r, sum(count) as num from q_inc group by Brw_inc_r ")

lbls <- c("non-metropolitan area","metropolitan area")
pct <- round(sum_MSA$num/sum(sum_MSA$num)*100)
lbls <- paste(lbls, sep=": ", pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(sum_MSA$num, labels = lbls, main = "Metropolitan Statistical Area")

lbls <- c("0~80% income","80%~120 income",">120% income","missing")
pct <- round(sum_Income$num/sum(sum_Income$num)*100)
lbls <- paste(lbls, sep=": ",pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(sum_Income$num, labels = lbls, main = "Tract Income Ratio")

lbls <- c("0~80% income","80%~120 income",">120% income","Not Applicable")
pct <- round(sum_Brw_inc_r$num/sum(sum_Brw_inc_r$num)*100)
lbls <- paste(lbls, sep=": ",pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(sum_Brw_inc_r$num, labels = lbls, main = "Borrower Income Ratio")


