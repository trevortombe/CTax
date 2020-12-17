source("core.R")
library(broom)
library(viridis)
col<-c("#CC2529","#396ab1","#3E9651","#DA7C30","#535154","#6B4C9A","#922428","#948B3D")
expcategories<-read.csv("ExpCategories.csv")

# Load the SPSM microdata
#data<-read.delim("SPSMv28/data_pull_2021.prn",sep=";")
data<-read.delim("SPSMv28/data_pull.prn",sep=";")

data %>%
  filter(hdprov==8,hhncf==1,ctpcar>0,impcai>0) %>% 
  mutate(higher=ctpcar>impcai) %>%
  summarise(total=sum(hdwgthh*higher))

# Share with rebates above costs
plotdata<-data %>%
  filter(hdprov==8,hhncf==1,ctpcar>0,impcai>0) %>%
  mutate(netcost=ctpcar-impcai,
         above=(netcost>0)*1,
         one=1) %>%
  select(netcost,hdwgthhs,above,one,immtot)
ggplot(plotdata,aes(x=netcost,y=1,fill=..x..,weight=hdwgthhs))+
  geom_density_ridges_gradient(show.legend = F)+
  scale_fill_viridis(name = "", option = "C")
ggplot(plotdata,aes(netcost,weight=hdwgthh))+geom_histogram()

plotdata %>% drop_na() %>% group_by(above) %>% summarise(hh=sum(hdwgthhs),hh2=sum(one))

# Plot carbon costs aginst spending on natural gas and gasoline
require(hexbin)
plotdata<-data %>%
  filter(hdprov==8,hhncf==1,ctpcar>0,immtot>0,hdnpers==4,hdnkids==2) %>%
  mutate(spend=ctnexp15+ctnexp8,
         netcost=ctpcar-impcai) %>%
  filter(spend>0,spend<15000)
ggplot(plotdata,aes(spend,netcost,weight=hdwgthh))+
  geom_hex(color='transparent')+
  scale_fill_viridis_c(name="Number of Households: ",label=comma,limit=c(0,NA))+
  geom_hline(yintercept = 0,size=1,color='gray')+
  mytheme+
  theme(panel.grid.major.x = element_line(),
        legend.key.width = unit(12,'mm'))+
  scale_y_continuous("Annual Net Carbon Costs",label=dollar)+
  scale_x_continuous("Annual Spending on Natural Gas and Gasoline",label=dollar)+
  labs(title="Household Net Carbon Tax Costs vs Fuel Use in Alberta (2020)",
       subtitle = "Displays average annual carbon tax costs less the rebate for families of 4 with 2 kids")
plotdata<-data %>%
  filter(hdprov==8,hhncf==1,ctpcar>0,immtot>0,hdnpers==2,hdnkids==0) %>%
  mutate(spend=ctnexp15+ctnexp8,
         netcost=ctpcar-impcai) %>%
  filter(spend>0,spend<15000)
ggplot(plotdata,aes(spend,netcost,weight=hdwgthh))+
  geom_hex(color='transparent')+
  scale_fill_viridis_c(name="Number of Households: ",label=comma,limit=c(0,NA))+
  geom_hline(yintercept = 0,size=1,color='gray')+
  mytheme+
  theme(panel.grid.major.x = element_line(),
        legend.key.width = unit(12,'mm'))+
  scale_y_continuous("Annual Net Carbon Costs",label=dollar)+
  scale_x_continuous("Annual Spending on Natural Gas and Gasoline",label=dollar)+
  labs(title="Household Net Carbon Tax Costs vs Fuel Use in Alberta (2020)",
       subtitle = "Displays average annual carbon tax costs less the rebate for families of 2 with no kids")
plotdata<-data %>%
  filter(hdprov==8,hhncf==1,ctpcar>0,immtot>0,hdnpers==2,hdnkids==0) %>%
  mutate(spend=ctnexp15+ctnexp8,
         type="Family of Two with No Kids",
         netcost=ctpcar-impcai) %>%
  filter(spend>0,spend<15000) %>%
  rbind(
    data %>%
      filter(hdprov==8,hhncf==1,ctpcar>0,immtot>0,hdnpers==1,hdnkids==0) %>%
      mutate(spend=ctnexp15+ctnexp8,
             type="Single Individual",
             netcost=ctpcar-impcai) %>%
      filter(spend>0,spend<15000)
  ) %>%
  rbind(
    data %>%
      filter(hdprov==8,hhncf==1,ctpcar>0,immtot>0,hdnpers==4,hdnkids==2) %>%
      mutate(spend=ctnexp15+ctnexp8,
             type="Family of Four with Two Kids",
             netcost=ctpcar-impcai) %>%
      filter(spend>0,spend<15000)
  )
ggplot(plotdata,aes(spend,netcost,weight=hdwgthh))+
  geom_hex(color='transparent')+
  scale_fill_binned(name="Number of\nHouseholds\nin a Cell: ",label=comma_format(accuracy = 1),type = "viridis",
                    breaks=c(0,1000,5000,30000))+
  #scale_fill_viridis_c(name="Number of\nHouseholds: ",label=comma_format(accuracy = 1),limit=c(0,NA))+
  #scale_fill_distiller(direction=-1)
  geom_hline(yintercept = 0,size=1,color='gray')+
  facet_wrap(~type)+
  mytheme+
  theme(panel.grid.major.x = element_line(),
        legend.key.width = unit(5,'mm'),
        legend.position = "right",
        strip.background = element_rect(fill="gray90",color="transparent"))+
  scale_y_continuous("Annual Net Carbon Costs",label=dollar)+
  scale_x_continuous("Annual Spending on Natural Gas and Gasoline",label=dollar)+
  labs(title="Household Net Carbon Tax Costs vs Fuel Use in Alberta (2020)",
       caption="Source: own calculations from Statistics Canada SPSM/D (version 28) microdata. Graph by @trevortombe",
       subtitle = "Displays average annual carbon tax costs, net of the rebate, against fuel spending by selected family types.
Includes only households with a single census family.")
ggsave('plot.png',width=9,height=3.75,dpi=400)

plotdata %>%
  group_by(type) %>%
  summarise(spend=weighted.mean(spend,hdwgthh),
            HHs=sum(hdwgthh))

ggplot(plotdata,aes(log(ctnexp15),log(ctnexp8),weight=hdwgthh))+
  geom_hex(color='transparent')+
  scale_fill_viridis_c(name="Number of\nHouseholds: ",label=comma_format(accuracy = 1),limit=c(0,NA))

# Waterfall Graph of Carbon Costs in Alberta (2020, SPSM v28)
plotdata<-data %>%
  filter(hhncf==1,hdprov==8) %>%
  summarise_at(vars(ctpcar,ctpcar0,ctpcar1,ctpcar8,ctpcar9,ctpcar15,impcai),funs(weighted.mean(.,hdwgthh))) %>%
  mutate(other=ctpcar-ctpcar0-ctpcar1-ctpcar8-ctpcar9-ctpcar15,
         food_bev=ctpcar0+ctpcar1,
         fuel=ctpcar8+ctpcar9) %>%
  gather(type,value) %>%
  mutate(type=factor(type,levels = c("fuel","ctpcar15","food_bev","other","impcai"))) %>%
  arrange(type) %>%
  filter(!is.na(type)) %>%
  mutate(value=ifelse(type=="impcai",-value,value),
         type=as.character(type),
         total=cumsum(value)) %>%
  mutate(type=ifelse(type=="fuel","Natural Gas\nand Other\nFuel Costs",type),
         type=ifelse(type=="ctpcar15","Motor Vehicle\nFuel Costs",type),
         type=ifelse(type=="food_bev","Food and\nBeverage\nCosts",type),
         type=ifelse(type=="other","All Other\nIndirect\nCosts",type),
         type=ifelse(type=="impcai","Average\nRebate\nValue",type))
plotdata<-plotdata %>%
  bind_rows(data.frame(type="Average\nNet Cost",value=min(plotdata$total))) %>%
  select(-total) %>%
  mutate(type=factor(type,levels=type),
         id=seq_along(value),
         sign=ifelse(value>0,"in","out"),
         sign=ifelse(id==n(),"net",sign),
         end=cumsum(value),
         end=ifelse(id==n(),0,end),
         start=ifelse(id>1,lag(end,1),0))
ggplot(plotdata)+
  geom_rect(aes(x=type,xmin = id - 0.4, xmax = id + 0.4,ymin=end,ymax=start,fill=sign),show.legend = F)+
  geom_hline(yintercept=0,size=1)+
  geom_segment(data=plotdata %>% filter(sign!="net"),aes(x=id+0.45,xend=id+0.45,y=start,yend=end),
               size=0.75,arrow=arrow(type="closed",length=unit(0.1,"cm")))+
  mythemebar+
  theme(axis.text.x = element_text(size=9,hjust=0.5))+
  scale_fill_manual(name="",values=c("#E41A1C","#4DAF4A","#377EB8"))+
  scale_y_continuous(breaks=pretty_breaks(n=6),label=dollar,limit=c(-600,600))+
  labs(x="
       Sources of Carbon Tax Costs in Alberta",
       y="Dollar per Year",
       caption="Graph by @trevortombe",
       subtitle="Source: own calculation from the Statistics Canada SPSM/D version 28 microdata. Includes only households with one census family.",
       title="Decomposing Net Carbon Tax Costs on Alberta Households (2021)")
ggsave("plot.png",width=8,height=4,dpi=300)


# Regression of carbon costs in Ontario
regdata<-data %>%
  filter(hhncf==1,hdprov==8,ctnexp8>0,ctnexp15>0) %>%
  mutate(urban=ifelse(hdurb>1,0,1))
summary(lm(log(ctpcar)~log(ctnexp8),data=regdata))
summary(lm(ctpcar~ctnexp8+log(immtot),data=regdata))
summary(lm(log(ctpcar)~log(ctnexp15),data=regdata))
regdata<-data %>%
  filter(hdprov==8)
tidy(lm(regdata[,c("ctpcar",grep("ctnexp",colnames(data),value=TRUE))])) %>% # all expenditure categories
  left_join(expcategories,by="term") %>%
  filter(!is.na(name)) %>%
  ggplot(aes(reorder(name,estimate),estimate))+geom_col()+coord_flip()+
  geom_text(aes(y=0.21,hjust=0,label=paste(round(100*estimate,2),"c")))+
  scale_y_continuous(limit=c(0,0.23))
reg<-lm(regdata[,c("ctpcar",grep("ctnexp",colnames(data),value=TRUE))])
temp<-tidy(anova(reg)) %>%
  filter(df==1) %>%
  mutate(share=sumsq/sum(sumsq)) %>%
  left_join(expcategories,by="term")
ggplot(temp,aes(reorder(name,share),share))+
  geom_col()+
  coord_flip()

# Do everything as a share of total expenditures
regdata<-data %>%
  filter(hdprov==8)
temp<-regdata[,c("hdseqhh","ctpcar",grep("ctnexp",colnames(data),value=TRUE))] %>%
  gather(var,value,-hdseqhh,-ctpcar) %>%
  group_by(hdseqhh) %>%
  mutate(exp=value/sum(value),
         total_exp=sum(value),
         tax=ctpcar/sum(value)) %>%
  select(hdseqhh,var,exp,tax,ctpcar,total_exp) %>%
  spread(var,exp) %>%
  ungroup() %>%
  select(-hdseqhh)
reg<-lm(temp %>% select(-ctpcar,-total_exp))
temp<-tidy(anova(reg)) %>%
  filter(df==1) %>%
  mutate(share=sumsq/sum(sumsq)) %>%
  left_join(expcategories,by="term") %>%
  mutate(name=ifelse(term=="total_exp","Total Expenditures",as.character(name)))
ggplot(temp,aes(reorder(name,share),share))+
  geom_col()+
  coord_flip()

# Regression results
regdata<-data %>%
  filter(hhncf==1,ctnexp15>0,ctnexp8>0,immtot>0,hdprov %in% c(3,5,6,7,8)) %>%
  mutate(urban=ifelse(hdurb>1,0,1))
summary(lm(log(ctpcar)~log(immtot),data=regdata))
summary(lm(log(ctpcar)~log(immtot)+urban,data=regdata))
summary(lm(log(ctpcar)~log(immtot)+hdnpers,data=regdata))
summary(lm(log(ctpcar)~log(immtot)+hdnpers+ctnexp15,data=regdata))
summary(lm(log(ctpcar)~log(immtot)+hdnpers+urban+log(ctnexp15)+log(ctnexp8)+factor(hdprov),data=regdata))
summary(lm(log(ctpcar)~log(ctnexp15)+log(ctnexp8),data=regdata))
summary(lm(urban~log(immtot)+hdnpers,data=regdata))

ggplot(regdata,
       aes(x=log(ctnexp8),y=as.character(hdurb),fill=..x..,weight=hdwgthh))+
  geom_density_ridges_gradient(show.legend = F)

# Carbon Tax Costs by Person's Main Job Industry
plotdata<-data %>%
  filter(hhncf==1) %>%
  filter(hdprov==8,immtot>0) %>%
  mutate(netcost=ctpcar-impcai)
ggplot(plotdata %>% filter(netcost<750,netcost>(-1500)),
       aes(x=netcost,y=as.character(idind),fill=..x..,weight=hdwgthh))+
  geom_density_ridges_gradient(show.legend = F)

# Plot of carbon costs across region
plotdata<-data %>%
  filter(hhncf==1) %>%
  filter(hdprov==8,immtot>0) %>%
  mutate(netcost=ctpcar-impcai,
         income=immtot/1000,
         income=cut(income,c(0,30,60,90,120,150,99999999))) %>%
  group_by(income) %>%
  mutate(order=min(immtot))
ggplot(plotdata %>% filter(netcost<750,netcost>(-1500)),
       aes(x=netcost,y=as.character(hdurb),fill=..x..,weight=hdwgthh))+
  geom_density_ridges_gradient(show.legend = F)+
  geom_vline(xintercept=0,size=1)+
  scale_fill_viridis(name = "", option = "C")+
  coord_cartesian(xlim=c(-1200,500))+
  mytheme+
  scale_y_discrete("Size of Area",label=c("500k+","30-100k","<30k","Rural"))+
  scale_x_continuous(label=dollar)+
  labs(x="Net Annual Carbon Cost",
       subtitle="Source: own calculations from Statistics Canada SPSM/D (version 28) microdata. Includes only households with one census family.",
       caption='Graph by @trevortombe',
       title="Distribution of Household Net Carbon Tax/Rebate Costs in Alberta (2020)")
ggsave('plot.png',width=8,height=5,dpi=300)

# Urban/Rural for Ontario
plotdata<-data %>%
  filter(hhncf==1) %>%
  filter(hdprov==5,immtot>0) %>%
  mutate(netcost=ctpcar-impcai,
         income=immtot/1000,
         income=cut(income,c(0,30,60,90,120,150,99999999))) %>%
  group_by(income) %>%
  mutate(order=min(immtot))
ggplot(plotdata %>% filter(netcost<750,netcost>(-1500)),
       aes(x=netcost,y=as.character(hdurb),fill=..x..,weight=hdwgthh))+
  geom_density_ridges_gradient(show.legend = F)+
  scale_fill_viridis(name = "", option = "C")+
  coord_cartesian(xlim=c(-1000,500))+
  scale_y_discrete("Size of Area",label=c("500k+","100-500k","30-100k","<30k","Rural"))+
  labs(x="Net Annual Carbon Cost",
       title="Distribution of Household Net Carbon Tax/Rebate Costs in Ontario (2020)")

# Urban/Rural for Alberta
plotdata<-data %>%
  filter(hhncf==1) %>%
  filter(hdprov==8,immtot>0) %>%
  mutate(netcost=ctpcar-impcai,
         income=immtot/1000,
         income=cut(income,c(0,30,60,90,120,150,99999999))) %>%
  group_by(income) %>%
  mutate(order=min(immtot))
ggplot(plotdata %>% filter(netcost<750,netcost>(-1500)),
       aes(x=ctpcar,y=reorder(income,order),fill=..x..,weight=hdwgthh))+
  geom_density_ridges_gradient(show.legend = F)+
  scale_fill_viridis(name = "", option = "C")
  coord_cartesian(xlim=c(-1000,500))
  scale_y_discrete("Size of Area",label=c("500k+","100-500k","30-100k","<30k","Rural"))+
  labs(x="Net Annual Carbon Cost",
       title="Distribution of Household Net Carbon Tax/Rebate Costs in Ontario (2020)")

# Versus Income
ggplot(plotdata %>% filter(netcost>(-1500),
                           netcost<=1000),aes(x=netcost,y=reorder(income,order),fill=..x..,weight=hdwgthh))+
  geom_density_ridges_gradient(show.legend = F)+
  scale_fill_viridis(name = "", option = "C")
  coord_cartesian(xlim=c(-1500,500))

# Net carbon tax costs as a share of income
ggplot(plotdata %>% group_by(income) %>%
         summarise(netcost=weighted.mean(netcost,hdwgthh),
                   order=min(immtot),
                   immtot=weighted.mean(immtot,hdwgthh)),aes(reorder(income,order),netcost/immtot))+
  geom_col()
ggplot(plotdata %>% filter(immtot>10000),aes(log(immtot),netcost/immtot))+geom_smooth()

# By rural/urban
ggplot(plotdata %>% group_by(income,hdurb) %>%
         summarise(netcost=weighted.mean(netcost,hdwgthh),
                   ctpcar=weighted.mean(ctpcar,hdwgthh),
                   gastax=weighted.mean(ctpgas,hdwgthh),
                   order=min(immtot),
                   immtot=weighted.mean(immtot,hdwgthh)),aes(reorder(income,order),ctpcar,
                                                             group=as.character(hdurb),fill=as.character(hdurb)))+
  geom_col(position='dodge')+
  geom_hline(yintercept = 0,size=1)+
  mytheme+
  scale_y_continuous(label=dollar)
  scale_fill_manual("Area Size: ",labels=c("500k+","30k-100k","<30k","Rural"),values=col[1:4])+
  labs(title="Provincial Gas Tax Costs in 2020 for Alberta",x="Household Income",y="Dollars per Year")

plotdata %>% group_by(hdurb) %>%
  summarise(netcost=weighted.mean(netcost,hdwgthh),
            HHs=sum(hdwgthh))

test<-data %>%
  group_by(hdprov) %>%
  mutate(netcost=ctpcar-impcai) %>%
  summarise(number=sum(hdwgthh),
            ctpcar=weighted.mean(ctpcar,hdwgthh),
            impcai=weighted.mean(impcai,hdwgthh)) %>%
  mutate(share=ctpcar/impcai)

ggplot(plotdata %>% group_by(hdnkids) %>%
         summarise(netcost=weighted.mean(netcost,hdwgthh),
                   impcai=weighted.mean(impcai,hdwgthh)),
       aes(hdnkids,impcai))+
  geom_col()

# Net Balance by Geographic Area
plotdata<-data %>%
  filter(hdprov==8) %>%
  group_by(hdurb) %>%
  summarise(imprvbal=weighted.mean(imprvbal,hdwgthh),
            imfedbal=weighted.mean(imfedbal,hdwgthh)) %>%
  gather(level,balance,-hdurb)
ggplot(plotdata,aes(hdurb,balance,group=level,fill=level))+
  geom_col(position='stack')

# Distribution of taxes paid less transfers received
plotdata<-data %>%
  filter(hhncf==1) %>%
  filter(hdprov==8,immtot>0) %>%
  mutate(netcost=imprvbal+imfedbal,
         income=immtot/1000,
         income=cut(income,c(0,30,60,90,120,150,99999999))) %>%
  group_by(income) %>%
  mutate(order=min(immtot))
ggplot(plotdata %>% filter(netcost<200000,netcost>(-10000)),
       aes(x=netcost,y=as.character(hdurb),fill=..x..,weight=hdwgthh))+
  geom_density_ridges_gradient(show.legend = F)+
  scale_fill_viridis(name = "", option = "C")+
  scale_y_discrete("Size of Area",label=c("500k+","30-100k","<30k","Rural"))+
  labs(x="Dollars per Year",
       title="Distribution of Taxes Paid Less Transfers Received by Households (2020, Alberta)")

# Federal Taxes vs Income by Province
plotdata<-data %>% filter(immtot>10000,immtot<175000,hhncf==1,hdnpers==1,hdnkids==0)
ggplot(plotdata,aes(immtot,imbft/immtot,group=hdprov))+
  geom_smooth(size=1)+
  geom_smooth(data=plotdata %>% filter(hdprov==8),color='red',size=2)+
  geom_point(data=plotdata %>%
               mutate(prov=ifelse(hdprov==8,"Alberta","Rest of Canada")) %>%
               group_by(prov) %>%
               summarise(immtot=weighted.mean(immtot,hdwgthh),
                         imbft=weighted.mean(imbft,hdwgthh)),aes(x=immtot,group=prov),
             size=4)+
  geom_text_repel(data=plotdata %>%
               mutate(prov=ifelse(hdprov==8,"Alberta","Rest of Canada")) %>%
               group_by(prov) %>%
               summarise(immtot=weighted.mean(immtot,hdwgthh),
                         imbft=weighted.mean(imbft,hdwgthh)),aes(x=immtot,group=prov,label=prov),
             size=5,point.padding = unit(2.5,'mm'),segment.alpha = 0)

ggplot(data %>% group_by(hdprov) %>% mutate(atr=imbft/immtot) %>%
         summarise(atr=weighted.mean(atr,hdwgthh)),aes(hdprov,atr))+
  geom_col()
