
df<- Sevilleta_allbiomass_05Feb2023%>%
  filter(site == "NutNet",
         season == "fall")%>%
  select(year, quad, treatment,kartez,biomass.BM)%>%
  spread(kartez, biomass.BM)
df[is.na(df)]<- 0
quad201320<- data.frame(year = 2013, treatment = "N", quad = "20")
quad201320<- cbind(quad201320, t(colMeans(df[df$year %in% c(2012,2014) & df$quad=="20",4:65])))
df<- rbind(df,quad201320)%>%
  gather(kartez, AGB,colnames(df[,4:65]))
LifeHistory<- Sevilleta_allbiomass_05Feb2023%>%
  filter(site == "NutNet",
         season == "fall")%>%
  select(kartez,LifeHistory)%>%
  distinct()
df<- left_join(df, LifeHistory, by = "kartez")

# community dynamic-----
diversity<- df%>%
  group_by(quad,year, treatment,kartez)%>%
  summarise(AGB = mean(AGB,na.rm= T))%>%
  group_by(quad,year, treatment)%>%
  filter(AGB >0)%>%
  summarise(richness = n(),
            H = diversity(AGB,"shannon"))
diversity.x<- diversity%>% group_by(treatment, year)%>%summarise(R.mean = mean(richness,na.rm = T), R.se = sd(richness,na.rm = T)/sqrt(5))
diversity.2009<- data.frame(treatment = c("C","P","K","PK","N","NP","NK","NPK"), year = 2009, R.mean = NA, R.se = NA)
diversity.x<- rbind(diversity.x, diversity.2009)
diversity.x$treatment <- factor(diversity.x$treatment, levels=c('C','P','K','PK','N','NP','NK','NPK'))
diversity$N<-ifelse(diversity$treatment %in% c("C","P","K","PK"), 0, 1)
diversity$P<-ifelse(diversity$treatment %in% c("C","N","K","NK"), 0, 1)
diversity$K<-ifelse(diversity$treatment %in% c("C","N","P","NP"), 0, 1)

total.2022<- df%>%
  group_by(year, quad, treatment)%>%
  summarise(AGB = sum(AGB))
total.2009<- data.frame(treatment = c("C","P","K","PK","N","NP","NK","NPK"), year = 2009, AGB_mean = NA, AGB_SE = NA)
total.2022$AGB[is.na(total.2022$AGB)]<- 0
total.2022.mean2<- total.2022%>%
  group_by(treatment, year)%>%
  summarise(AGB_mean = mean(AGB),
            AGB_SE = sd(AGB)/sqrt(5))%>%
  rbind(total.2009)
total.2022.mean$treatment <- factor(total.2022.mean2$treatment, levels=c('C','P','K','PK','N','NP','NK','NPK'))
total.2022$N<-ifelse(total.2022$treatment %in% c("C","P","K","PK"), 0, 1)
total.2022$P<-ifelse(total.2022$treatment %in% c("C","N","K","NK"), 0, 1)
total.2022$K<-ifelse(total.2022$treatment %in% c("C","N","P","NP"), 0, 1)

BlackGrama<-df%>%
  filter(kartez == "BOER4")%>%
  group_by(year, quad, treatment)%>%
  summarise(AGB = sum(AGB))
BG.2009<- data.frame(treatment = c("C","P","K","PK","N","NP","NK","NPK"), year = 2009, AGB_mean = NA, AGB_SE = NA)
BG.mean<- BlackGrama%>%
  group_by(treatment, year)%>%
  summarise(AGB_mean = mean(AGB,na.rm = T),
            AGB_SE = sd(AGB,na.rm = T)/sqrt(5))%>%
  rbind(BG.2009)
BG.mean$treatment <- factor(BG.mean$treatment, levels=c('C','P','K','PK','N','NP','NK','NPK'))
BlackGrama$N<-ifelse(BlackGrama$treatment %in% c("C","P","K","PK"), 0, 1)
BlackGrama$P<-ifelse(BlackGrama$treatment %in% c("C","N","K","NK"), 0, 1)
BlackGrama$K<-ifelse(BlackGrama$treatment %in% c("C","N","P","NP"), 0, 1)

Annuals<- df%>%
  filter(LifeHistory == "annual")%>%
  group_by(year, quad, treatment)%>%
  summarise(AGB = sum(AGB))
A.2009<- data.frame(treatment = c("C","P","K","PK","N","NP","NK","NPK"), year = 2009, AGB_mean = NA, AGB_SE = NA)
A.mean<- Annuals%>%
  group_by(treatment, year)%>%
  summarise(AGB_mean = mean(AGB, na.rm = T),
            AGB_SE = sd(AGB,na.rm = T)/sqrt(5))%>%
  rbind(A.2009)
A.mean$treatment <- factor(A.mean$treatment, levels=c('C','P','K','PK','N','NP','NK','NPK'))
Annuals$N<-ifelse(Annuals$treatment %in% c("C","P","K","PK"), 0, 1)
Annuals$P<-ifelse(Annuals$treatment %in% c("C","N","K","NK"), 0, 1)
Annuals$K<-ifelse(Annuals$treatment %in% c("C","N","P","NP"), 0, 1)


a<- ggplot(diversity.x)+geom_point(aes(x = year, y =  R.mean, color =  treatment), size = 3)+
  geom_line(aes(x = year, y = R.mean, color = treatment), size = 0.8)+
  geom_errorbar(aes(x = year, ymin = R.mean-R.se, ymax = R.mean+R.se), width = 0.1, alpha= 0.4)+
  scale_color_manual(values = c("#00D3F5","#0034EF","#00D688","#00735C","#F55701","#F02201","#FFBB00","#D67500"))+
  ylim(0,12)+
  labs(x = "", y = "Richness")+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="top") 

b<- ggplot(total.2022.mean2)+geom_point(aes(x = year, y = AGB_mean, color = treatment), size = 3)+
  geom_line(aes(x = year, y = AGB_mean, color = treatment), size = 0.8)+
  geom_errorbar(aes(x = year, ymin = AGB_mean-AGB_SE, ymax = AGB_mean+AGB_SE), width = 0.1, alpha= 0.4)+
  scale_color_manual(values = c("#00D3F5","#0034EF","#00D688","#00735C","#F55701","#F02201","#FFBB00","#D67500"))+
  ylim(0,450)+
  labs(x = "", y = "Community biomass")+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="")


c<- ggplot(BG.mean)+geom_point(aes(x = year, y = AGB_mean, color = treatment), size = 3)+
  geom_line(aes(x = year, y = AGB_mean, color = treatment), size = 0.8)+
  geom_errorbar(aes(x = year, ymin = AGB_mean-AGB_SE, ymax = AGB_mean+AGB_SE), width = 0.1, alpha= 0.4)+
  scale_color_manual(values = c("#00D3F5","#0034EF","#00D688","#00735C","#F55701","#F02201","#FFBB00","#D67500"))+
  ylim(0,200)+
  labs(x = "", y = "Black grama")+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="")

d<- ggplot(A.mean)+geom_point(aes(x = year, y = AGB_mean, color = treatment), size = 3)+
  geom_line(aes(x = year, y = AGB_mean, color = treatment), size = 0.8)+
  geom_errorbar(aes(x = year, ymin = AGB_mean-AGB_SE, ymax = AGB_mean+AGB_SE), width = 0.1, alpha= 0.4)+
  scale_color_manual(values = c("#00D3F5","#0034EF","#00D688","#00735C","#F55701","#F02201","#FFBB00","#D67500"))+
  ylim(0,320)+
  labs(x = "", y = "Annuals")+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="")

a+b+c+d
ggsave("CommunityDynamic.png", width = 4596, height = 3059, units = c("px"), device = "png",dpi =300)
anova(lme(AGB~N*P*K*year, random = ~1|quad ,total.2022[total.2022$year != 2008,]))



Diversity.Q<- diversity%>%
  group_by(year, N)%>%
  summarise(Q.075 = quantile(richness, 0.75),
            Q.050 = quantile(richness, 0.50),
            Q.025 = quantile(richness, 0.25),)
T.Q<- total.2022%>%
  group_by(year, N)%>%
  summarise(Q.075 = quantile(AGB, 0.75),
            Q.050 = quantile(AGB, 0.50),
            Q.025 = quantile(AGB, 0.25),)
BG.Q<- BlackGrama%>%
  group_by(year, N)%>%
  summarise(Q.075 = quantile(AGB, 0.75),
            Q.050 = quantile(AGB, 0.50),
            Q.025 = quantile(AGB, 0.25),)
A.Q<- Annuals%>%
  group_by(year, N)%>%
  summarise(Q.075 = quantile(AGB, 0.75),
            Q.050 = quantile(AGB, 0.50),
            Q.025 = quantile(AGB, 0.25),)

a<- ggplot()+
  geom_line(data = Diversity.Q,aes(x = year, y = Q.050, color =  as.character(N)), alpha = 1, size = 1.5, linetype = 1)+
  #geom_smooth(data = BlackGrama, aes(x = year, y = AGB, color =  as.character(N)), method = "loess", size = 1.5, fill = NA, span =0.75)+
  geom_ribbon(data = Diversity.Q, aes(x = year, ymin = Q.025, ymax = Q.075, fill = as.character(N)),alpha = 0.1)+
  geom_vline (xintercept=2009, linetype = 2)+ 
  scale_color_manual(values = c("#31322c","#bb000e"))+
  scale_fill_manual(values = c("#31322c","#bb000e"))+
  ylim(0,12)+
  labs(x = "", y = "Richness")+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="top")
b<- ggplot()+
  geom_line(data = T.Q,aes(x = year, y =   Q.050, color =  as.character(N)), alpha = 1, size = 1.5, linetype = 1)+
  #geom_smooth(data = BlackGrama, aes(x = year, y = AGB, color =  as.character(N)), method = "loess", size = 1.5, fill = NA, span =0.75)+
  geom_ribbon(data = T.Q, aes(x = year, ymin = Q.025, ymax = Q.075, fill = as.character(N)),alpha = 0.1)+
  geom_vline (xintercept=2009, linetype = 2)+ 
  scale_color_manual(values = c("#31322c","#bb000e"))+
  scale_fill_manual(values = c("#31322c","#bb000e"))+
  ylim(0,450)+
  labs(x = "", y = "Community biomass")+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="")
c<- ggplot()+
  geom_line(data = BG.Q,aes(x = year, y =   Q.050, color =  as.character(N)), alpha = 1, size = 1.5, linetype = 1)+
  #geom_smooth(data = BlackGrama, aes(x = year, y = AGB, color =  as.character(N)), method = "loess", size = 1.5, fill = NA, span =0.75)+
  geom_ribbon(data = BG.Q, aes(x = year, ymin = Q.025, ymax = Q.075, fill = as.character(N)),alpha = 0.1)+
  geom_vline (xintercept=2009, linetype = 2)+ 
  scale_color_manual(values = c("#31322c","#bb000e"))+
  scale_fill_manual(values = c("#31322c","#bb000e"))+
  ylim(0,200)+
  labs(x = "", y = "Black grama")+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="")
d<- ggplot()+
  geom_line(data = A.Q,aes(x = year, y =   Q.050, color =  as.character(N)), alpha = 1, size = 1.5, linetype = 1)+
  #geom_smooth(data = BlackGrama, aes(x = year, y = AGB, color =  as.character(N)), method = "loess", size = 1.5, fill = NA, span =0.75)+
  geom_ribbon(data = A.Q, aes(x = year, ymin = Q.025, ymax = Q.075, fill = as.character(N)),alpha = 0.1)+
  geom_vline (xintercept=2009, linetype = 2)+ 
  scale_color_manual(values = c("#31322c","#bb000e"))+
  scale_fill_manual(values = c("#31322c","#bb000e"))+
  ylim(0,320)+
  labs(x = "", y = "Annuals")+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="")  
a+b+c+d
ggsave("CommunityDynamic.png", width = 4596, height = 3059, units = c("px"), device = "png",dpi =300)
# Variability-----
postfire.CV<- df%>%
  filter(year != 2008)%>%
  group_by(treatment,quad, year)%>%
  summarise(AGB = sum(AGB))%>%
  ungroup()%>%
  group_by(treatment,quad)%>%
  summarise(AGB.mean = mean(AGB),
            CV= sd(AGB)/mean(AGB))
postfire.CV$treatment <- factor(postfire.CV$treatment, levels=c('C','P','K','PK','N','NP','NK','NPK'))
postfire.CV$N<-ifelse(postfire.CV$treatment %in% c("C","P","K","PK"), 0, 1)
postfire.CV$P<-ifelse(postfire.CV$treatment %in% c("C","N","K","NK"), 0, 1)
postfire.CV$K<-ifelse(postfire.CV$treatment %in% c("C","N","P","NP"), 0, 1)

postfire.CV1016<- df%>%
  filter(year %in% c(2010:2016))%>%
  group_by(treatment,quad, year)%>%
  summarise(AGB = sum(AGB))%>%
  ungroup()%>%
  group_by(treatment,quad)%>%
  summarise(AGB.mean = mean(AGB),
            CV= sd(AGB)/mean(AGB))
postfire.CV1016$N<-ifelse(postfire.CV1016$treatment %in% c("C","P","K","PK"), 0, 1)
postfire.CV1016$P<-ifelse(postfire.CV1016$treatment %in% c("C","N","K","NK"), 0, 1)
postfire.CV1016$K<-ifelse(postfire.CV1016$treatment %in% c("C","N","P","NP"), 0, 1)
postfire.CV1016$treatment <- factor(postfire.CV1116$treatment, levels=c('C','P','K','PK','N','NP','NK','NPK'))
anova(lm(dis.mean~N*P*K,bist.df1022))

postfire.CV1622<- df%>%
  filter(year %in% c(2016:2022))%>%
  group_by(treatment,quad, year)%>%
  summarise(AGB = sum(AGB))%>%
  ungroup()%>%
  group_by(treatment,quad)%>%
  summarise(AGB.mean = mean(AGB),
            CV= sd(AGB)/mean(AGB))
postfire.CV1622$N<-ifelse(postfire.CV1622$treatment %in% c("C","P","K","PK"), 0, 1)
postfire.CV1622$P<-ifelse(postfire.CV1622$treatment %in% c("C","N","K","NK"), 0, 1)
postfire.CV1622$K<-ifelse(postfire.CV1622$treatment %in% c("C","N","P","NP"), 0, 1)
postfire.CV1622$treatment <- factor(postfire.CV1622$treatment, levels=c('C','P','K','PK','N','NP','NK','NPK'))
df.spread<- df%>% 
  select(year,quad,treatment,kartez,AGB)%>%
  spread(kartez,AGB)

bray.dis<- as.matrix(vegdist(df.spread[,-c(1:3)], method = 'bray'))

group<- df.spread[,c(1,3)]
group$N<-ifelse(group$treatment %in% c("C","P","K","PK"), 0, 1)
group$P<-ifelse(group$treatment %in% c("C","N","K","NK"), 0, 1)
group$K<-ifelse(group$treatment %in% c("C","N","P","NP"), 0, 1)

dis<- as.dist(bray.dis)
adonis(dis~Year*N*P*K, group[,-2], permutations = 999)

bray.dis<- data.frame(cbind(bray.dis, df.spread[,c(1:3)]))
plot<- unique(bray.dis$quad)
year<- unique(bray.dis$year)
bist.df<- data.frame()
for (i in 1:40) {
  
  bray.dis.i<- bray.dis[bray.dis$quad == plot[i],]
  for (j in 2:14) {
    
    bray.dis.i.j<- bray.dis.i[bray.dis.i$year == year[j],]
    bray.dis.i.j.t<- cbind(df.spread[,c(1:2)],t(bray.dis.i.j[,1:560]))
    dis = unlist(bray.dis.i.j.t[bray.dis.i.j.t$year == year[j-1]& bray.dis.i.j.t$quad == plot[i],3])
    bist.df<- rbind(bist.df, c(plot[i],year[j],dis))
    
  }
}
colnames(bist.df)<- c("quad","year","dis")  
bist.df$dis <- as.numeric(bist.df$dis)
bist.df$year <- as.numeric(bist.df$year)
treatment<- distinct(df[,2:3])
bist.df<- left_join(bist.df,treatment, by = "quad" )

bist.df$treatment <- factor(bist.df$treatment, levels=c('C','P','K','PK','N','NP','NK','NPK'))

bist.df1022<- bist.df%>%
  filter(year %in% c(2010:2022))%>%
  group_by(quad,treatment)%>%
  summarise(dis.mean = mean(dis))
bist.df1022$N<-ifelse(bist.df1022$treatment %in% c("C","P","K","PK"), 0, 1)
bist.df1022$P<-ifelse(bist.df1022$treatment %in% c("C","N","K","NK"), 0, 1)
bist.df1022$K<-ifelse(bist.df1022$treatment %in% c("C","N","P","NP"), 0, 1)

bist.df1016<- bist.df%>%
  filter(year %in% c(2010:2016))%>%
   group_by(quad,treatment)%>%
  summarise(dis.mean = mean(dis))
bist.df1016$N<-ifelse(bist.df1016$treatment %in% c("C","P","K","PK"), 0, 1)
bist.df1016$P<-ifelse(bist.df1016$treatment %in% c("C","N","K","NK"), 0, 1)
bist.df1016$K<-ifelse(bist.df1016$treatment %in% c("C","N","P","NP"), 0, 1)

bist.df1022<- bist.df%>%
  filter(year %in% c(2010:2022))%>%
  group_by(quad,treatment)%>%
  summarise(dis.mean = mean(dis))
bist.df1016$N<-ifelse(bist.df1016$treatment %in% c("C","P","K","PK"), 0, 1)
bist.df1016$P<-ifelse(bist.df1016$treatment %in% c("C","N","K","NK"), 0, 1)
bist.df1016$K<-ifelse(bist.df1016$treatment %in% c("C","N","P","NP"), 0, 1)

bist.df1622<- bist.df%>%
  filter(year %in% c(2016:2022))%>%
  group_by(quad,treatment)%>%
  summarise(dis.mean = mean(dis))
bist.df1622$N<-ifelse(bist.df1016$treatment %in% c("C","P","K","PK"), 0, 1)
bist.df1622$P<-ifelse(bist.df1016$treatment %in% c("C","N","K","NK"), 0, 1)
bist.df1622$K<-ifelse(bist.df1016$treatment %in% c("C","N","P","NP"), 0, 1)
anova(lm(dis.mean~N*P*K,bist.df1022))


a<- ggplot(postfire.CV)+ geom_boxplot(aes(x = treatment, y = CV, fill = as.character(N)))+
  ylim(0.2,1.3)+
  labs(x = "",y = "Biomass variability")+
  scale_fill_manual(values = c("#999999","#bb000e"))+
  theme_classic()+
  ggtitle("2010-2016")+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=20),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.5, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        plot.title = element_text(hjust = 0.5),
        legend.position="top")  

b<- ggplot(bist.df1022)+ geom_boxplot(aes(x = treatment, y = dis.mean, fill = as.character(N)))+
  ylim(0.1,0.9)+
  labs(x = "",y = "Compositional variability")+
  scale_fill_manual(values = c("#999999","#bb000e"))+
  ggtitle("2016-2022")+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        plot.title = element_text(hjust = 0.5),
        legend.position="")  
a+b
ggsave("Variablity.png", width = 3596, height = 1700, units = c("px"), device = "png",dpi =300)
anova()
a<- ggplot(postfire.CV1016)+ geom_boxplot(aes(x = treatment, y = CV, fill = as.character(N)))+
  ylim(0.1,1.3)+
  labs(x = "",y = "Biomass variability")+
  scale_fill_manual(values = c("#999999","#bb000e"))+
  theme_classic()+
  ggtitle("2010-2016")+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=20),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.5, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        plot.title = element_text(hjust = 0.5),
        legend.position="top")  

b<- ggplot(postfire.CV1622)+ geom_boxplot(aes(x = treatment, y = CV, fill = as.character(N)))+
  ylim(0.1,1.3)+
  labs(x = "",y = "")+
  scale_fill_manual(values = c("#999999","#bb000e"))+
  ggtitle("2016-2022")+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        plot.title = element_text(hjust = 0.5),
        legend.position="")  


c<- ggplot(bist.df1016)+ geom_boxplot(aes(x = treatment, y = dis.mean, fill = as.character(N)))+
  ylim(0.1,0.9)+
  labs(x = "",y = "Compositional variability")+
  scale_fill_manual(values = c("#999999","#bb000e"))+
  theme_classic()+
  theme(legend.text = element_text(size = 20),
        axis.title.x=element_text(vjust=0, size=22),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="")  

d<- ggplot(bist.df1622)+ geom_boxplot(aes(x = treatment, y = dis.mean, fill = as.character(N)))+
  ylim(0.1,0.9)+
  labs(x = "",y = "")+
  scale_fill_manual(values = c("#999999","#bb000e"))+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="")  
a+b+c+d
ggsave("Variablity.png", width = 3596, height = 3059, units = c("px"), device = "png",dpi =300)

BlackGramaCV.Incommunity<- BlackGrama%>%
  filter(year > 2009)%>%
  group_by(quad,treatment)%>%
  summarise(SD = sd(AGB))
BlackGramaCV.Incommunity<- left_join(BlackGramaCV.Incommunity, postfire.CV[,c(2,3)], by = "quad")
BlackGramaCV.Incommunity$BG.CV = BlackGramaCV.Incommunity$SD/BlackGramaCV.Incommunity$AGB.mean

AnnualsCV.Incommunity<- Annuals%>%
  filter(year > 2009)%>%
  group_by(quad,treatment)%>%
  summarise(SD = sd(AGB))
AnnualsCV.Incommunity<- left_join(AnnualsCV.Incommunity, postfire.CV[,c(2,3)], by = "quad")
AnnualsCV.Incommunity$A.CV = AnnualsCV.Incommunity$SD/AnnualsCV.Incommunity$AGB.mean
AnnualsCV.Incommunity<- left_join(AnnualsCV.Incommunity, BlackGramaCV.Incommunity[,c(1,5)],  by = "quad")
AnnualsCV.Incommunity$treatment <- factor(AnnualsCV.Incommunity$treatment, levels=c('C','P','K','PK','N','NP','NK','NPK'))
AnnualsCV.Incommunity$N<-ifelse(AnnualsCV.Incommunity$treatment %in% c("C","P","K","PK"), 0, 1)
ggplot(AnnualsCV.Incommunity)+geom_point(aes(x = BG.CV, y =A.CV, fill = as.character(N), size = 5, shape = treatment),color = "black", alpha = 0.6)+
  scale_shape_manual(values = c(21,23,24,25,21,23,24,25))+
  scale_fill_manual(values = c("#787878","#bb000e"))+
  geom_abline(aes(intercept = 0, slope=1)) +
  labs(x = "", y = "AGB_CV")+
  ylim(0,1.2)+  xlim(0,1.2)+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="top") 



# Precipitation response-----
Sevilleta_LTER_Hourly_Meteorological_Data_2005_2009$label<-ifelse(Sevilleta_LTER_Hourly_Meteorological_Data_2005_2009$Month %in% 10:12, "b", "a")

TP0509<- Sevilleta_LTER_Hourly_Meteorological_Data_2005_2009%>%
  filter(StationID == 40)%>%
  select(Year,Month,Temp_C,Precipitation,label)%>%
  group_by(Year,label)%>%
  summarise(Temp = sum(Temp_C, na.rm = T),
            Precipitation = sum(Precipitation, na.rm = T))

Sevilleta_LTER_Hourly_Meteorological_Data_2010_2014$label<-ifelse(Sevilleta_LTER_Hourly_Meteorological_Data_2010_2014$Month %in% 10:12, "b", "a")

TP1014<- Sevilleta_LTER_Hourly_Meteorological_Data_2010_2014%>%
  filter(StationID == 40)%>%
  select(Year,Month,Temp_C,Precipitation,label)%>%
  group_by(Year,label)%>%
  summarise(Temp = sum(Temp_C, na.rm = T),
            Precipitation = sum(Precipitation, na.rm = T))


Sevilleta_LTER_Hourly_Meteorological_Data_2020_2022$label<-ifelse(Sevilleta_LTER_Hourly_Meteorological_Data_2020_2022$Month %in% 10:12, "b", "a")

TP2122<- Sevilleta_LTER_Hourly_Meteorological_Data_2020_2022%>%
  filter(StationID == 40)%>%
  select(Year,Month,Temp_C,Precipitation,label)%>%
  group_by(Year,label)%>%
  summarise(Temp = sum(Temp_C, na.rm = T),
            Precipitation = sum(Precipitation, na.rm = T))

Sevilleta_LTER_Hourly_Meteorological_Data_2015_2019$label<-ifelse(Sevilleta_LTER_Hourly_Meteorological_Data_2015_2019$Month %in% 10:12, "b", "a")

TP1519<- Sevilleta_LTER_Hourly_Meteorological_Data_2015_2019%>%
  filter(StationID == 40)%>%
  select(Year,Month,Temp_C,Precipitation,label)%>%
  group_by(Year,label)%>%
  summarise(Temp = sum(Temp_C, na.rm = T),
            Precipitation = sum(Precipitation, na.rm = T))
TP<- rbind(TP1519,TP2122)
TP<- rbind(TP1014,TP)
TP<- rbind(TP0509,TP)
TP.annual<- data.frame()
for (i in 1:17){
  year = TP[2*i+1,]$Year
  P.i = sum(TP[c(2*i,2*i+1),]$Precipitation)
  T.i = sum(TP[c(2*i,2*i+1),]$Temp)/24/365
  TP.annual<- rbind(TP.annual, c(year,P.i, T.i))
}

colnames(TP.annual)<- c("year","Pre","Temp")

BlackGrama<- left_join(BlackGrama, TP.annual[,1:2], by = "year")
Annuals<- left_join(Annuals, TP.annual[,1:2], by = "year")


plot<- unique(BlackGrama$quad) 

slope<- data.frame()
for (i in 1:40) {
  
  BlackGrama.i1016 <- BlackGrama[BlackGrama$year%in%c(2010:2022) & BlackGrama$quad == plot[i],]
  BlackGrama.i1622 <- BlackGrama[BlackGrama$year%in%c(2016:2022) & BlackGrama$quad == plot[i],]
  Annuals.i1016 <- Annuals[Annuals$year%in%c(2010:2022) & Annuals$quad == plot[i],]
  Annuals.i1622 <- Annuals[Annuals$year%in%c(2016:2022) & Annuals$quad == plot[i],]
  
  slope.A1016 = lm(AGB~Pre,Annuals.i1016)$coefficients[2]
  slope.A1622 = lm(AGB~Pre,Annuals.i1622)$coefficients[2]
  slope.B1016 = lm(AGB~Pre,BlackGrama.i1016)$coefficients[2]
  slope.B1622 = lm(AGB~Pre,BlackGrama.i1622)$coefficients[2]

  slope<- rbind(slope, c(plot[i],slope.A1016,slope.A1622,slope.B1016,slope.B1622))
}
colnames(slope)<- c("quad","slope.A1022","slope.A1622","slope.B1022","slope.B1622")
slope[,2:5]<- apply(slope[,2:5], 2, as.numeric)

postfire.CV<- left_join(postfire.CV, slope[,c(1,2,4)], by = "quad")
postfire.CV1622<- left_join(postfire.CV1622, slope[,c(1,3,5)], by = "quad")
bist.df1022<- left_join(bist.df1022, slope[,c(1,2,4)], by = "quad")
bist.df1622<- left_join(bist.df1622, slope[,c(1,3,5)], by = "quad")

postfire.CV$N<-ifelse(postfire.CV$treatment %in% c("C","P","K","PK"), 0, 1)
postfire.CV$P<-ifelse(postfire.CV$treatment %in% c("C","N","K","NK"), 0, 1)
postfire.CV$K<-ifelse(postfire.CV$treatment %in% c("C","N","P","NP"), 0, 1)

bist.df1022$N<-ifelse(bist.df1022$treatment %in% c("C","P","K","PK"), 0, 1)
bist.df1022$P<-ifelse(bist.df1022$treatment %in% c("C","N","K","NK"), 0, 1)
bist.df1022$K<-ifelse(bist.df1022$treatment %in% c("C","N","P","NP"), 0, 1)
postfire.CV<- edit(postfire.CV)
bist.df1022<-edit(bist.df1022)

a1<- ggplot(postfire.CV)+geom_point(aes(x = slope.B1022, y = CV, fill = as.character(N), size = 5, shape = treatment),color = "white", alpha = 0.6)+
  scale_shape_manual(values = c(24,25,21,21,23,23,25,24))+
  scale_fill_manual(values = c("#787878","#bb000e"))+
  geom_smooth(aes(x = slope.B1022, y = CV, color = as.character(N), linetype = as.character(N)),size = 2,method = "lm", fill = NA)+
  scale_color_manual(values = c("black","#bb000e"))+
  #geom_smooth(aes(x = slope.A, y = dis.mean,  color = "black"),size = 2,method = "lm", fill = NA)+
  scale_linetype_manual(values = c(1,1))+
  labs(x = "", y = "AGB_CV")+
  ylim(0.3,1.2)+  xlim(-0.05,0.7)+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="top") 
a2<- ggscatterhist(postfire.CV, x= 'slope.B1022', y= 'CV', color = "NA", alpha = 0.5, 
              palette = c("#4F4F4F","#bb000e"),
              ggtheme =   theme_pubr()+theme(axis.title.x=element_text(vjust=0, size=26),
                                             axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                                             axis.text.x=element_text(vjust=0,size=22),
                                             axis.text.y=element_text(hjust=0,size=22),
                                             legend.key.size = unit(1.2, 'cm'),
                                             axis.line.x=element_line(linetype=1,color="black",size=1.0),
                                             axis.line.y=element_line(linetype=1,color="black",size=1.0)),
              margin.plot = "histogram",
              margin.params = list(fill = "N", color = "Black", size = 0.2, alpha = 0.5),
              margin.plot.size = 0.6,
              xlab = "Black grama response",
              ylab = "Biomass variability",
              legend = c("NA"),
              ggp = a1)
ggsave("relation1.png", width = 1800, height = 1800, units = c("px"), device = "png",dpi =300)

b1<- ggplot(postfire.CV)+geom_point(aes(x = slope.A1022, y = CV, fill = as.character(N), size = 5, shape = treatment),color = "white", alpha = 0.6)+
  scale_shape_manual(values = c(24,25,21,21,23,23,25,24))+
  scale_fill_manual(values = c("#787878","#bb000e"))+
  geom_smooth(aes(x = slope.A1022, y = CV, color = as.character(N), linetype = as.character(N)),size = 2,method = "lm", fill = NA)+
  scale_color_manual(values = c("black","#bb000e"))+
  #geom_smooth(aes(x = slope.A, y = dis.mean,  color = "black"),size = 2,method = "lm", fill = NA)+
  scale_linetype_manual(values = c(1,1))+
  labs(x = "", y = "AGB_CV")+
  ylim(0.3,1.2)+  xlim(-0.35,1.0)+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="top") 
b2<- ggscatterhist(postfire.CV, x= 'slope.A1022', y= 'CV', color = "NA", alpha = 0.5, 
                   palette = c("#4F4F4F","#bb000e"),
                   ggtheme =   theme_pubr()+theme(axis.title.x=element_text(vjust=0, size=26),
                                                  axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                                                  axis.text.x=element_text(vjust=0,size=22),
                                                  axis.text.y=element_text(hjust=0,size=22),
                                                  legend.key.size = unit(1.2, 'cm'),
                                                  axis.line.x=element_line(linetype=1,color="black",size=1.0),
                                                  axis.line.y=element_line(linetype=1,color="black",size=1.0)),
                   margin.plot = "histogram",
                   margin.params = list(fill = "N", color = "Black", size = 0.2, alpha = 0.5),
                   margin.plot.size = 0.6,
                   xlab = "Annuals response",
                   ylab = "Biomass variability",
                   legend = c("NA"),
                   ggp = b1)
ggsave("relation2.png", width = 1800, height = 1800, units = c("px"), device = "png",dpi =300)
c1<- ggplot(bist.df1022)+geom_point(aes(x = slope.B1022, y = dis.mean, fill = as.character(N), size = 5, shape = treatment),color = "white", alpha = 0.6)+
  scale_shape_manual(values = c(24,25,21,21,23,23,25,24))+
  scale_fill_manual(values = c("#787878","#bb000e"))+
  geom_smooth(aes(x = slope.B1022, y = dis.mean, color = as.character(N), linetype = as.character(N)),size = 2,method = "lm", fill = NA)+
  scale_color_manual(values = c("black","#bb000e"))+
  #geom_smooth(aes(x = slope.A, y = dis.mean,  color = "black"),size = 2,method = "lm", fill = NA)+
  scale_linetype_manual(values = c(1,1))+
  scale_y_continuous(labels=scaleFUN,limits=c(0.2, 0.75))+
  xlim(-0.05,0.7)+
  labs(x = "", y = "AGB_CV")+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="top") 
c2<- ggscatterhist(bist.df1022, x= 'slope.B1022', y= 'dis.mean', color = "NA", alpha = 0.5, 
                   palette = c("#4F4F4F","#bb000e"),
                   ggtheme =   theme_pubr()+theme(axis.title.x=element_text(vjust=0, size=26),
                                                  axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                                                  axis.text.x=element_text(vjust=0,size=22),
                                                  axis.text.y=element_text(hjust=0,size=22),
                                                  legend.key.size = unit(1.2, 'cm'),
                                                  axis.line.x=element_line(linetype=1,color="black",size=1.0),
                                                  axis.line.y=element_line(linetype=1,color="black",size=1.0)),
                   margin.plot = "histogram",
                   margin.params = list(fill = "N", color = "Black", size = 0.2, alpha = 0.5),
                   margin.plot.size = 0.6,
                   xlab = "Black grama response",
                   ylab = "Compositional variability",
                   legend = c("NA"),
                   ggp = c1)
ggsave("relation3.png", width = 1800, height = 1800, units = c("px"), device = "png",dpi =300)
scaleFUN <- function(x) sprintf("%.2f", x) 

d1<- ggplot(bist.df1022)+geom_point(aes(x = slope.A1022, y = dis.mean, fill = as.character(N), size = 5, shape = treatment),
                                    color = "white", alpha = 0.6)+
  scale_shape_manual(values = c(24,25,21,21,23,23,25,24))+
  scale_fill_manual(values = c("#787878","#bb000e"))+
  geom_smooth(aes(x = slope.A1022, y = dis.mean, color = as.character(N)),size = 2,method = "lm", fill = NA)+
  scale_color_manual(values = c("black","#bb000e"))+

  #geom_smooth(aes(x = slope.A, y = dis.mean,  color = "black"),size = 2,method = "lm", fill = NA)+
  labs(x = "", y = "AGB_CV")+
  scale_y_continuous(labels=scaleFUN,limits=c(0.2, 0.75))+
  xlim(-0.35,1.0)+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="top") 
d2<- ggscatterhist(bist.df1022, x= 'slope.A1022', y= 'dis.mean', color = "NA", alpha = 0.5, 
                   palette = c("#4F4F4F","#bb000e"),
                   ggtheme =   theme_pubr()+theme(axis.title.x=element_text(vjust=0, size=26),
                                                  axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
                                                  axis.text.x=element_text(vjust=0,size=22),
                                                  axis.text.y=element_text(hjust=0,size=22),
                                                  legend.key.size = unit(1.2, 'cm'),
                                                  axis.line.x=element_line(linetype=1,color="black",size=1.0),
                                                  axis.line.y=element_line(linetype=1,color="black",size=1.0)),
                   margin.plot = "histogram",
                   margin.params = list(fill = "N", color = "Black", size = 0.2, alpha = 0.5),
                   margin.plot.size = 0.6,
                   xlab = "Annuals response",
                   ylab = "Compositional variability",
                   legend = c("NA"),
                   ggp = d1)
ggsave("relation4.png", width = 1800, height = 1800, units = c("px"), device = "png",dpi =300)
anova(lm(dis.mean~slope.B1022*N,  bist.df1022))

slope<- left_join(slope, treatment, by = "quad")
slope$N<-ifelse(slope$treatment %in% c("C","P","K","PK"), 0, 1)
slope$P<-ifelse(slope$treatment %in% c("C","N","K","NK"), 0, 1)
slope$K<-ifelse(slope$treatment %in% c("C","N","P","NP"), 0, 1)
slope$treatment <- factor(slope$treatment, levels=c('C','P','K','PK','N','NP','NK','NPK'))
a<- ggplot(slope)+ geom_boxplot(aes(x = treatment, y = slope.B1022, fill = as.character(N)))+
  ylim(-0.4,1.3)+
  labs(x = "",y = "precipitation response of black grama")+
  scale_fill_manual(values = c("#999999","#bb000e"))+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=20),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=20),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1.5, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        plot.title = element_text(hjust = 0.5),
        legend.position="top")  

b<- ggplot(slope)+ geom_boxplot(aes(x = treatment, y = slope.A1022, fill = as.character(N)))+
  ylim(-0.4,1.3)+
  labs(x = "",y = "precipitation response of annuals")+
  scale_fill_manual(values = c("#999999","#bb000e"))+
  theme_classic()+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=20),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        plot.title = element_text(hjust = 0.5),
        legend.position="")  
a+b
ggsave("Precipitation ResponseS3.png", width = 3596, height = 1700, units = c("px"), device = "png",dpi =300)
anova(lm(slope.B1022~N*P*K,slope))
#CCM----


library(multispatialCCM)
N_B.df<- BlackGrama[BlackGrama$N == 1 & BlackGrama$year != 2008, c(1,2,4)]
C_B.df<- BlackGrama[BlackGrama$N == 0 & BlackGrama$year != 2008, c(1,2,4)]
N_A.df<- Annuals[Annuals$N == 1 & Annuals$year != 2008, c(1,2,4)]
C_A.df<- Annuals[Annuals$N == 0 & Annuals$year != 2008, c(1,2,4)]
N_quad<- unique(N_B.df$quad)
C_quad<- unique(C_B.df$quad)
C_B<- c()
C_A<- c()
N_B<- c()
N_A<- c()
for (i in 1:20) {
  N_Null<- data.frame(year = 2023, quad = N_quad[i], AGB = NA)
  C_Null<- data.frame(year = 2023, quad = C_quad[i], AGB = NA)
  N_B.i <- N_B.df[N_B.df$quad == N_quad[i],]%>% rbind(N_Null)
  C_B.i <- C_B.df[C_B.df$quad == C_quad[i],]%>% rbind(C_Null)
  N_A.i <- N_A.df[N_A.df$quad == N_quad[i],]%>% rbind(N_Null)
  C_A.i <- C_A.df[C_A.df$quad == C_quad[i],]%>% rbind(C_Null)
  N_B<- c(N_B, N_B.i$AGB)
  N_A<- c(N_A, N_A.i$AGB)
  C_B<- c(C_B, C_B.i$AGB)
  C_A<- c(C_A, C_A.i$AGB)
  }




maxE<-14
Emat<-matrix(nrow=maxE-1, ncol=2); colnames(Emat)<-c("A","B")
for(E in 2:maxE) {
  Emat[E-1,"A"]<-SSR_pred_boot(A=C_A, E=E, predstep=1, tau=0)$rho
  Emat[E-1,"B"]<-SSR_pred_boot(A=C_B, E=E, predstep=1, tau=0)$rho
}
matplot(2:maxE, Emat, type="l", col=1:2, lty=1:2,
        xlab="E", ylab="rho", lwd=2)
legend("bottomleft", c("A", "B"), lty=1:2, col=1:3, lwd=2, bty="n")
E_A<-12
E_B<-12

signal_A_out<-SSR_check_signal(A=N_A, E=E_A, tau=1,
                               predsteplist=1:100)
signal_A_out
signal_B_out<-SSR_check_signal(A=N_B, E=E_B, tau=1,
                               predsteplist=1:100)
signal_B_out
outA<-data.frame(signal_A_out$`predatout`)
plot(outA$predstep,outA$rho)
outB<-data.frame(signal_B_out$`predatout`)
plot(outB$predstep,outB$rho)

ccmresult<- data.frame()
for(i in 4:12){
  E_A<-i
  E_B<-i

CCM_boot_CAB0<-CCM_boot(C_A, C_B, E_A, tau=0, iterations=1000)
CCM_boot_CBA0<-CCM_boot(C_B, C_A,  E_B, tau=0, iterations=1000)
sign_CAB0<-ccmtest(CCM_boot_CAB0,CCM_boot_CBA0)

CCM_boot_CAB1<-CCM_boot(C_A, C_B, E_A, tau=1, iterations=1000)
CCM_boot_CBA1<-CCM_boot(C_B, C_A,  E_B, tau=1, iterations=1000)
sign_CAB1<-ccmtest(CCM_boot_CAB1,CCM_boot_CBA1)


CCM_boot_NAB0<-CCM_boot(N_A, N_B, E_A, tau=0, iterations=1000)
CCM_boot_NBA0<-CCM_boot(N_B, N_A,  E_B, tau=0, iterations=1000)
sign_NAB0<-ccmtest(CCM_boot_NAB0,CCM_boot_NBA0)
sign_NAB0
CCM_boot_NAB1<-CCM_boot(N_A, N_B, E_A, tau=1, iterations=1000)
CCM_boot_NBA1<-CCM_boot(N_B, N_A,  E_B, tau=1, iterations=1000)
sign_NAB1<-ccmtest(CCM_boot_NAB1,CCM_boot_NBA1)

ccmresult<- rbind(ccmresult, c(sign_CAB0,sign_NAB0,sign_CAB1,sign_NAB1))
}


CCM_boot_CAB0<-CCM_boot(C_A, C_B, 10, tau=0, iterations=1000)
CCM_boot_CBA0<-CCM_boot(C_B, C_A,  10, tau=0, iterations=1000)

CCM_boot_CAB1<-CCM_boot(C_A, C_B, 10, tau=1, iterations=1000)
CCM_boot_CBA1<-CCM_boot(C_B, C_A,  10, tau=1, iterations=1000)

CCM_boot_NAB0<-CCM_boot(N_A, N_B, 10, tau=0, iterations=1000)
CCM_boot_NBA0<-CCM_boot(N_B, N_A,  10, tau=0, iterations=1000)

CCM_boot_NAB1<-CCM_boot(N_A, N_B, 10, tau=1, iterations=1000)
CCM_boot_NBA1<-CCM_boot(N_B, N_A,  10, tau=1, iterations=1000)
#CCM figure 4----
CCM_boot_CBA1

CCM_ABC0_data<- data.frame(CCM_boot_CAB0$Lobs, CCM_boot_CAB0$rho, CCM_boot_CAB0$sdevrho)%>% mutate(label = "ABC")
colnames(CCM_ABC0_data)<- c("Lobs","rho","sdevrho","Label")
CCM_BAC0_data<- data.frame(CCM_boot_CBA0$Lobs, CCM_boot_CBA0$rho, CCM_boot_CBA0$sdevrho)%>% mutate(label = "BAC")
colnames(CCM_BAC0_data)<- c("Lobs","rho","sdevrho","Label")
CCM_ABC0<- rbind(CCM_ABC0_data,CCM_BAC0_data)

CCM_ABN0_data<- data.frame(CCM_boot_NAB0$Lobs, CCM_boot_NAB0$rho, CCM_boot_NAB0$sdevrho)%>% mutate(label = "ABC")
colnames(CCM_ABN0_data)<- c("Lobs","rho","sdevrho","Label")
CCM_BAN0_data<- data.frame(CCM_boot_NBA0$Lobs, CCM_boot_NBA0$rho, CCM_boot_NBA0$sdevrho)%>% mutate(label = "BAC")
colnames(CCM_BAN0_data)<- c("Lobs","rho","sdevrho","Label")
CCM_ABN0<- rbind(CCM_ABN0_data,CCM_BAN0_data)

CCM_ABC1_data<- data.frame(CCM_boot_CAB1$Lobs, CCM_boot_CAB1$rho, CCM_boot_CAB1$sdevrho)%>% mutate(label = "ABC")
colnames(CCM_ABC1_data)<- c("Lobs","rho","sdevrho","Label")
CCM_BAC1_data<- data.frame(CCM_boot_CBA1$Lobs, CCM_boot_CBA1$rho, CCM_boot_CBA1$sdevrho)%>% mutate(label = "BAC")
colnames(CCM_BAC1_data)<- c("Lobs","rho","sdevrho","Label")
CCM_ABC1<- rbind(CCM_ABC1_data,CCM_BAC1_data)

CCM_ABN1_data<- data.frame(CCM_boot_NAB1$Lobs, CCM_boot_NAB1$rho, CCM_boot_NAB1$sdevrho)%>% mutate(label = "ABC")
colnames(CCM_ABN1_data)<- c("Lobs","rho","sdevrho","Label")
CCM_BAN1_data<- data.frame(CCM_boot_NBA1$Lobs, CCM_boot_NBA1$rho, CCM_boot_NBA1$sdevrho)%>% mutate(label = "BAC")
colnames(CCM_BAN1_data)<- c("Lobs","rho","sdevrho","Label")
CCM_ABN1<- rbind(CCM_ABN1_data,CCM_BAN1_data)

a<- ggplot(CCM_ABC0, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "Cross map skill (ρ)", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")

b<- ggplot(CCM_ABN0, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "Cross map skill (ρ)", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")
c<- ggplot(CCM_ABC1, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "Cross map skill (ρ)", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")

d<- ggplot(CCM_ABN1, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "Cross map skill (ρ)", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")
a+b+c+d
ggsave("CCM1.png", width = 4096, height = 3059, units = c("px"), device = "png",dpi =300)

#CCM figure S5----
S8_CCM_boot_CAB0<-CCM_boot(C_A, C_B, 8, tau=0, iterations=1000)
S8_CCM_boot_CBA0<-CCM_boot(C_B, C_A,  8, tau=0, iterations=1000)
S8_CCM_boot_NAB0<-CCM_boot(N_A, N_B, 8, tau=0, iterations=1000)
S8_CCM_boot_NBA0<-CCM_boot(N_B, N_A,  8, tau=0, iterations=1000)

S12_CCM_boot_CAB0<-CCM_boot(C_A, C_B, 12, tau=0, iterations=1000)
S12_CCM_boot_CBA0<-CCM_boot(C_B, C_A,  12, tau=0, iterations=1000)
S12_CCM_boot_NAB0<-CCM_boot(N_A, N_B, 12, tau=0, iterations=1000)
S12_CCM_boot_NBA0<-CCM_boot(N_B, N_A,  12, tau=0, iterations=1000)

S6_CCM_boot_CAB0<-CCM_boot(C_A, C_B, 6, tau=0, iterations=1000)
S6_CCM_boot_CBA0<-CCM_boot(C_B, C_A,  6, tau=0, iterations=1000)
S6_CCM_boot_NAB0<-CCM_boot(N_A, N_B, 6, tau=0, iterations=1000)
S6_CCM_boot_NBA0<-CCM_boot(N_B, N_A,  6, tau=0, iterations=1000)

S6_CCM_ABC0_data<- data.frame(S6_CCM_boot_CAB0$Lobs, S6_CCM_boot_CAB0$rho, S6_CCM_boot_CAB0$sdevrho)%>% mutate(label = "ABC")
colnames(S6_CCM_ABC0_data)<- c("Lobs","rho","sdevrho","Label")
S6_CCM_BAC0_data<- data.frame(S6_CCM_boot_CBA0$Lobs, S6_CCM_boot_CBA0$rho, S6_CCM_boot_CBA0$sdevrho)%>% mutate(label = "BAC")
colnames(S6_CCM_BAC0_data)<- c("Lobs","rho","sdevrho","Label")
S6_CCM_ABC0<- rbind(S6_CCM_ABC0_data,S6_CCM_BAC0_data)
S6_CCM_ABN0_data<- data.frame(S6_CCM_boot_NAB0$Lobs, S6_CCM_boot_NAB0$rho, S6_CCM_boot_NAB0$sdevrho)%>% mutate(label = "ABC")
colnames(S6_CCM_ABN0_data)<- c("Lobs","rho","sdevrho","Label")
S6_CCM_BAN0_data<- data.frame(S6_CCM_boot_NBA0$Lobs, S6_CCM_boot_NBA0$rho, S6_CCM_boot_NBA0$sdevrho)%>% mutate(label = "BAC")
colnames(S6_CCM_BAN0_data)<- c("Lobs","rho","sdevrho","Label")
S6_CCM_ABN0<- rbind(S6_CCM_ABN0_data,S6_CCM_BAN0_data)

S8_CCM_ABC0_data<- data.frame(S8_CCM_boot_CAB0$Lobs, S8_CCM_boot_CAB0$rho, S8_CCM_boot_CAB0$sdevrho)%>% mutate(label = "ABC")
colnames(S8_CCM_ABC0_data)<- c("Lobs","rho","sdevrho","Label")
S8_CCM_BAC0_data<- data.frame(S8_CCM_boot_CBA0$Lobs, S8_CCM_boot_CBA0$rho, S8_CCM_boot_CBA0$sdevrho)%>% mutate(label = "BAC")
colnames(S8_CCM_BAC0_data)<- c("Lobs","rho","sdevrho","Label")
S8_CCM_ABC0<- rbind(S8_CCM_ABC0_data,S8_CCM_BAC0_data)
S8_CCM_ABN0_data<- data.frame(S8_CCM_boot_NAB0$Lobs, S8_CCM_boot_NAB0$rho, S8_CCM_boot_NAB0$sdevrho)%>% mutate(label = "ABC")
colnames(S8_CCM_ABN0_data)<- c("Lobs","rho","sdevrho","Label")
S8_CCM_BAN0_data<- data.frame(S8_CCM_boot_NBA0$Lobs, S8_CCM_boot_NBA0$rho, S8_CCM_boot_NBA0$sdevrho)%>% mutate(label = "BAC")
colnames(S8_CCM_BAN0_data)<- c("Lobs","rho","sdevrho","Label")
S8_CCM_ABN0<- rbind(S8_CCM_ABN0_data,S8_CCM_BAN0_data)

S12_CCM_ABC0_data<- data.frame(S12_CCM_boot_CAB0$Lobs, S12_CCM_boot_CAB0$rho, S12_CCM_boot_CAB0$sdevrho)%>% mutate(label = "ABC")
colnames(S12_CCM_ABC0_data)<- c("Lobs","rho","sdevrho","Label")
S12_CCM_BAC0_data<- data.frame(S12_CCM_boot_CBA0$Lobs, S12_CCM_boot_CBA0$rho, S12_CCM_boot_CBA0$sdevrho)%>% mutate(label = "BAC")
colnames(S12_CCM_BAC0_data)<- c("Lobs","rho","sdevrho","Label")
S12_CCM_ABC0<- rbind(S12_CCM_ABC0_data, S12_CCM_BAC0_data)
S12_CCM_ABN0_data<- data.frame(S12_CCM_boot_NAB0$Lobs, S12_CCM_boot_NAB0$rho, S12_CCM_boot_NAB0$sdevrho)%>% mutate(label = "ABC")
colnames(S12_CCM_ABN0_data)<- c("Lobs","rho","sdevrho","Label")
S12_CCM_BAN0_data<- data.frame(S12_CCM_boot_NBA0$Lobs, S12_CCM_boot_NBA0$rho, S12_CCM_boot_NBA0$sdevrho)%>% mutate(label = "BAC")
colnames(S12_CCM_BAN0_data)<- c("Lobs","rho","sdevrho","Label")
S12_CCM_ABN0<- rbind(S12_CCM_ABN0_data,S12_CCM_BAN0_data)

a<- ggplot(S6_CCM_ABC0, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")

b<- ggplot(S6_CCM_ABN0, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")
c<- ggplot(S8_CCM_ABC0, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "Cross map skill (ρ)", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")

d<- ggplot(S8_CCM_ABN0, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")
e<- ggplot(S12_CCM_ABC0, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")
f<- ggplot(S12_CCM_ABN0, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "", x = "Libarary length")+
  ylim(-0.25,1.1)+
  #xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")
(a+b)/(c+d)/(e+f)
ggsave("CCM2.png", width = 4096, height = 4559, units = c("px"), device = "png",dpi =300)


S8_CCM_boot_CAB1<-CCM_boot(C_A, C_B,  8, tau=1, iterations=1000)
S8_CCM_boot_CBA1<-CCM_boot(C_B, C_A,  8, tau=1, iterations=1000)
S8_CCM_boot_NAB1<-CCM_boot(N_A, N_B,  8, tau=1, iterations=1000)
S8_CCM_boot_NBA1<-CCM_boot(N_B, N_A,  8, tau=1, iterations=1000)

S12_CCM_boot_CAB1<-CCM_boot(C_A, C_B,  12, tau=1, iterations=1000)
S12_CCM_boot_CBA1<-CCM_boot(C_B, C_A,  12, tau=1, iterations=1000)
S12_CCM_boot_NAB1<-CCM_boot(N_A, N_B,  12, tau=1, iterations=1000)
S12_CCM_boot_NBA1<-CCM_boot(N_B, N_A,  12, tau=1, iterations=1000)

S6_CCM_boot_CAB1<-CCM_boot(C_A, C_B,  6, tau=1, iterations=1000)
S6_CCM_boot_CBA1<-CCM_boot(C_B, C_A,  6, tau=1, iterations=1000)
S6_CCM_boot_NAB1<-CCM_boot(N_A, N_B,  6, tau=1, iterations=1000)
S6_CCM_boot_NBA1<-CCM_boot(N_B, N_A,  6, tau=1, iterations=1000)

S6_CCM_ABC1_data<- data.frame(S6_CCM_boot_CAB1$Lobs, S6_CCM_boot_CAB1$rho, S6_CCM_boot_CAB1$sdevrho)%>% mutate(label = "ABC")
colnames(S6_CCM_ABC1_data)<- c("Lobs","rho","sdevrho","Label")
S6_CCM_BAC1_data<- data.frame(S6_CCM_boot_CBA1$Lobs, S6_CCM_boot_CBA1$rho, S6_CCM_boot_CBA1$sdevrho)%>% mutate(label = "BAC")
colnames(S6_CCM_BAC1_data)<- c("Lobs","rho","sdevrho","Label")
S6_CCM_ABC1<- rbind(S6_CCM_ABC1_data,S6_CCM_BAC1_data)
S6_CCM_ABN1_data<- data.frame(S6_CCM_boot_NAB1$Lobs, S6_CCM_boot_NAB1$rho, S6_CCM_boot_NAB1$sdevrho)%>% mutate(label = "ABC")
colnames(S6_CCM_ABN1_data)<- c("Lobs","rho","sdevrho","Label")
S6_CCM_BAN1_data<- data.frame(S6_CCM_boot_NBA1$Lobs, S6_CCM_boot_NBA1$rho, S6_CCM_boot_NBA1$sdevrho)%>% mutate(label = "BAC")
colnames(S6_CCM_BAN1_data)<- c("Lobs","rho","sdevrho","Label")
S6_CCM_ABN1<- rbind(S6_CCM_ABN1_data,S6_CCM_BAN1_data)

S8_CCM_ABC1_data<- data.frame(S8_CCM_boot_CAB1$Lobs, S8_CCM_boot_CAB1$rho, S8_CCM_boot_CAB1$sdevrho)%>% mutate(label = "ABC")
colnames(S8_CCM_ABC1_data)<- c("Lobs","rho","sdevrho","Label")
S8_CCM_BAC1_data<- data.frame(S8_CCM_boot_CBA1$Lobs, S8_CCM_boot_CBA1$rho, S8_CCM_boot_CBA1$sdevrho)%>% mutate(label = "BAC")
colnames(S8_CCM_BAC1_data)<- c("Lobs","rho","sdevrho","Label")
S8_CCM_ABC1<- rbind(S8_CCM_ABC1_data,S8_CCM_BAC1_data)
S8_CCM_ABN1_data<- data.frame(S8_CCM_boot_NAB1$Lobs, S8_CCM_boot_NAB1$rho, S8_CCM_boot_NAB1$sdevrho)%>% mutate(label = "ABC")
colnames(S8_CCM_ABN1_data)<- c("Lobs","rho","sdevrho","Label")
S8_CCM_BAN1_data<- data.frame(S8_CCM_boot_NBA1$Lobs, S8_CCM_boot_NBA1$rho, S8_CCM_boot_NBA1$sdevrho)%>% mutate(label = "BAC")
colnames(S8_CCM_BAN1_data)<- c("Lobs","rho","sdevrho","Label")
S8_CCM_ABN1<- rbind(S8_CCM_ABN1_data,S8_CCM_BAN1_data)

S12_CCM_ABC1_data<- data.frame(S12_CCM_boot_CAB1$Lobs, S12_CCM_boot_CAB1$rho, S12_CCM_boot_CAB1$sdevrho)%>% mutate(label = "ABC")
colnames(S12_CCM_ABC1_data)<- c("Lobs","rho","sdevrho","Label")
S12_CCM_BAC1_data<- data.frame(S12_CCM_boot_CBA1$Lobs, S12_CCM_boot_CBA1$rho, S12_CCM_boot_CBA1$sdevrho)%>% mutate(label = "BAC")
colnames(S12_CCM_BAC1_data)<- c("Lobs","rho","sdevrho","Label")
S12_CCM_ABC1<- rbind(S12_CCM_ABC1_data, S12_CCM_BAC1_data)
S12_CCM_ABN1_data<- data.frame(S12_CCM_boot_NAB1$Lobs, S12_CCM_boot_NAB1$rho, S12_CCM_boot_NAB1$sdevrho)%>% mutate(label = "ABC")
colnames(S12_CCM_ABN1_data)<- c("Lobs","rho","sdevrho","Label")
S12_CCM_BAN1_data<- data.frame(S12_CCM_boot_NBA1$Lobs, S12_CCM_boot_NBA1$rho, S12_CCM_boot_NBA1$sdevrho)%>% mutate(label = "BAC")
colnames(S12_CCM_BAN1_data)<- c("Lobs","rho","sdevrho","Label")
S12_CCM_ABN1<- rbind(S12_CCM_ABN1_data,S12_CCM_BAN1_data)

a<- ggplot(S6_CCM_ABC1, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")

b<- ggplot(S6_CCM_ABN1, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")
c<- ggplot(S8_CCM_ABC1, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "Cross map skill (ρ)", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")

d<- ggplot(S8_CCM_ABN1, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "", x = "Libarary length")+
  ylim(-0.25,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")
e<- ggplot(S12_CCM_ABC1, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "", x = "Libarary length")+
  ylim(-0.4,1.1)+
  xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")
f<- ggplot(S12_CCM_ABN1, aes(x = Lobs))+geom_ribbon(aes(ymin = rho-sdevrho, ymax = rho+sdevrho, fill = Label), alpha = 0.13)+
  geom_line(aes(y = rho, color = Label), linewidth = 2)+
  scale_color_manual(values = c("#bb000e","#1C1C1C"))+
  scale_fill_manual(values = c("#bb000e","#1C1C1C"))+
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1)+
  labs(y = "", x = "Libarary length")+
  ylim(-0.4,1.1)+
  #xlim(0,250)+
  theme_classic()+
  theme(axis.line.x=element_line(linetype=1,color="black",linewidth=1),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1),
        axis.ticks.x=element_line(color="black",linewidth=1),
        axis.ticks.y=element_line(color="black",linewidth=1),
        axis.ticks.length = unit(0.4, "cm"),
        axis.text.x=element_text(vjust=0,size=26),
        axis.text.y=element_text(hjust=0,size=26),
        axis.title.x=element_text(vjust=-1, size=30),
        axis.title.y=element_text(vjust=2, size=30),
        legend.position = "none")
(a+b)/(c+d)/(e+f)
ggsave("CCM3.png", width = 4096, height = 4559, units = c("px"), device = "png",dpi =300)


#Figure S1----
a<-ggplot(TP.annual)+geom_line(aes(x = year, y = Pre), linewidth = 1.5)+
  ylim(50,350)+
  theme_classic()+
  labs(x = "", y = "Precipitation (mm)")+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="top") 
b<-ggplot(TP.annual)+geom_line(aes(x = year, y = Temp), linewidth = 1.5)+
  #ylim(50,350)+
  theme_classic()+
  labs(x = "", y = "Temperature (℃)")+
  theme(legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="top") 
a/b
ggsave("PT.png", width = 3096, height = 2559, units = c("px"), device = "png",dpi =300)

#figure S2----
FunctionalGroup<- Sevilleta_allbiomass_05Feb2023%>%
  filter(site == "NutNet",
         season == "fall")%>%
  select(kartez,FunctionalGroup)%>%
  distinct()
df<- left_join(df, FunctionalGroup, by = "kartez")
FG.df1<- df%>%
  mutate(LH = substr(LifeHistory, 1,1))%>%
  group_by(year, quad, treatment, LH, FunctionalGroup)%>%
  summarise(AGB = sum(AGB, na.rm = T))%>%
  mutate(Life.Function = paste(LH,FunctionalGroup, sep = "-"))%>%
  na.omit()%>%
  mutate(N = ifelse(treatment %in% c("C","P","K","PK"), 0, 1),
         P = ifelse(treatment %in% c("C","N","K","NK"), 0, 1),
         K = ifelse(treatment %in% c("C","N","P","NP"), 0, 1))
FG.df<- df%>%
  mutate(LH = substr(LifeHistory, 1,1))%>%
  group_by(year, quad, treatment, LH, FunctionalGroup)%>%
  summarise(AGB = sum(AGB, na.rm = T))%>%
  mutate(Life.Function = paste(LH,FunctionalGroup, sep = "-"))%>%
  na.omit()%>%
  mutate(N = ifelse(treatment %in% c("C","P","K","PK"), 0, 1))%>%
  group_by(year, LH,FunctionalGroup, N)%>%
    summarise(Q.075 = quantile(AGB, 0.75),
              Q.050 = quantile(AGB, 0.50),
              Q.025 = quantile(AGB, 0.25))
FG.df[FG.df$LH == "a",]$LH <- "annual"
FG.df[FG.df$LH == "p",]$LH <- "perennial"
a<- ggplot(FG.df)+
  geom_line(aes(x = year, y = Q.050, color =  as.character(N)), alpha = 1, size = 1.5, linetype = 1)+
  #geom_smooth(data = BlackGrama, aes(x = year, y = AGB, color =  as.character(N)), method = "loess", size = 1.5, fill = NA, span =0.75)+
  geom_ribbon(aes(x = year, ymin = Q.025, ymax = Q.075, fill = as.character(N)),alpha = 0.1)+
  #geom_vline (xintercept=2009, linetype = 2)+ 
  scale_color_manual(values = c("#31322c","#bb000e"))+
  scale_fill_manual(values = c("#31322c","#bb000e"))+
  ylim(0,250)+
  labs(x = "", y = "Aboveground biomass")+
  theme_bw()+
  theme(panel.grid.minor=element_blank(),
        legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="top")+
  facet_grid(rows = vars(LH), cols = vars(FunctionalGroup))+theme(strip.text.x = element_text(size = 18), # 这里设置x轴方向的字体类型，
             strip.text.y = element_text(size = 18)) # 这里设置y轴方向的字体类型， )
ggsave("CommunityDynamic2.png", width = 4996, height = 2759, units = c("px"), device = "png",dpi =300)  
anova(lme(AGB~year*N*P*K, random = ~1|quad, total.2022[total.2022$year != 2008,]))

#figure S3----
df.spread2<-df%>% 
  group_by(year, treatment,kartez)%>% 
  summarise(AGB = mean(AGB, rm.na= T))%>%
  select(year,treatment,kartez,AGB)%>%
  spread(kartez,AGB)
dfNmds<-metaMDS(df.spread2[,-c(1:2)],distance="bray",k = 4)
adonis(i.obs ~ species, method = "euclidean", data= i.spp)

NMDS.result <- data.frame(dfNmds$points)
NMDS.result<- cbind(NMDS.result, df.spread2[,c(1:2)])
NMDS.result$N<-ifelse(NMDS.result$treatment %in% c("C","P","K","PK"), "N0", "N10")
NMDS.result2<- NMDS.result%>% group_by(year, N)%>% summarise(MDS1= mean(MDS1), MDS2 =mean(MDS2))
a<-ggplot(data=NMDS.result2,aes(x=MDS1,y=MDS2))+#指定数据、X轴、Y轴，颜色
  theme_bw()+#主题设置
  geom_point(aes(color = N), shape = 19, size=3)+
  geom_path(aes(color = N), linetype =1, size=1)+#绘制点图并设定大小
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed", size = 1, color = 'grey50')+
  geom_hline(yintercept = 0,lty="dashed", size = 1, color = 'grey50')+#图中虚线
  scale_color_manual(values = c("#999999","#bb000e"))+
  theme(panel.grid.minor=element_blank(),
        legend.text = element_text(size = 22),
        axis.title.x=element_text(vjust=0, size=26),
        axis.title.y=element_text(hjust=0.5, vjust=2,size=26),
        axis.text.x=element_text(vjust=0,size=22),
        axis.text.y=element_text(hjust=0,size=22),
        legend.key.size = unit(1, 'cm'),
        axis.line.x=element_line(linetype=1,color="black",linewidth=1.0),
        axis.line.y=element_line(linetype=1,color="black",linewidth=1.0),
        legend.position="top")+
  ggtitle(paste('Stress=',round(df_nmds_stress, 4)))
ggsave("NMDS.png", width = 3000, height = 3100, units = c("px"), device = "png",dpi =300)  
