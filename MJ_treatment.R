library(dplyr)
library(ggplot2)

mj_treatment <- read.csv('MJ treatment.csv',stringsAsFactors = F)%>%tbl_df()
circadian_genes <- read.csv('circadian clock list.csv',stringsAsFactors = F)%>%tbl_df()
mj_treatment <- filter(mj_treatment, GeneID%in%circadian_genes$AGI)
mj_treatment <- mj_treatment[,1:20]
mj_treatment <- merge(x = mj_treatment,y = circadian_genes,by.x = 'GeneID',by.y = 'AGI')%>%select(Name,T0:Regulator..Yes.1..no.0.,-GeneID)

mj_treatment <- t(mj_treatment)%>%as.data.frame()
colnames(mj_treatment) <- unlist(mj_treatment["Name",])
mj_treatment <- mj_treatment[-1,]
mj_treatment <- mutate(mj_treatment,time = row.names(mj_treatment))%>%select(time,LHY:CKA1)
mj_treatment <- mj_treatment[1:15,]
time_1 <- sapply(mj_treatment$time,function(x) substr(x,2,5))
mj_treatment <- mutate(mj_treatment,time = as.numeric(time_1))

write.csv(mj_treatment,'MJ_treatment.csv')
mj_treatment <- read.csv('MJ_treatment.csv',stringsAsFactors = F)

for (i in names(mj_treatment)[3:164]){
  dev.new()
  p<- ggplot(data = mj_treatment,aes_string(x='time',y = i))+geom_line()+labs(title=i,x='time',y='Mean fold change (log2)')
  ggsave(filename = as.character(paste(i,'.pdf')),plot = p)
  dev.off()
}
