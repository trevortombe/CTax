source("core.R")
library(viridis)

# Distribution of net carbon tax costs across all households
plotdata<-read_excel("CTaxCosts_2030.xlsx",sheet="NetCostwithFed") %>%
  gather(income,number,-NetCost) %>%
  group_by(income) %>%
  mutate(share=number/sum(number)) %>%
  filter(NetCost!=1250,NetCost>-900,income=="All") %>%
  mutate(NetCost=(170/50)*(1/1.21)*0.76*NetCost)
plotdata <- data.frame(x = rep(plotdata$NetCost, 10*plotdata$number)) %>%
  mutate(id=1:n()) %>%
  group_by(id) %>%
  mutate(x2=x-25/2+rnorm(1,0,25/3)) %>% # (25/2) since x is the upper bound of a $25 bin. SD of 25/3 ensure draws within the bins.
  ungroup()
ggplot(plotdata,aes(x=x2,y=1,fill=..x..))+
  geom_density_ridges_gradient(show.legend = F)+
  geom_point(aes(x=25,y=1.0012),color="transparent",show.legend = F)+
  annotate("text",x=25,y=Inf,label="CTax > Rebate",size=3,hjust=0,vjust=1.25)+
  geom_segment(aes(x=25,xend=350,y=1.001,yend=1.001),arrow=arrow(length=unit(1,'mm')))+
  annotate("text",x=-25,y=Inf,label="Rebate > CTax",size=3,hjust=1,vjust=1.25)+
  geom_segment(aes(x=-25,xend=-350,y=1.001,yend=1.001),arrow=arrow(length=unit(1,'mm')))+
  scale_fill_viridis(name = "", option = "C")+
  geom_vline(xintercept = 0,size=1)+
  geom_hline(yintercept=0,size=1)+
  mytheme+
  scale_x_continuous(label=dollar)+
  scale_y_discrete(expand=c(0,0))+
  labs(x="Net Carbon Tax Costs (2020$/Year)",
       y="Distribution",
       title="Net Cost to Households of a $170/t Federal Carbon Tax",
       subtitle="Source: Own calculations from Statistics Canada's SPSM/D version 28.",
       caption="Assumes 2% inflation and uniform behavioural response equivalent to 25% lower emissions from 2022. Graph by @trevortombe")
ggsave("AllHouseholds_2030.png",width=6.5,height=4,dpi=300)

## Comparing the Alberta rebates to your estimate of the federal rebates
plotdata<-read_excel("CTaxCosts_2030.xlsx",sheet="NetCostwithFed") %>%
  gather(income,number,-NetCost) %>%
  group_by(income) %>%
  mutate(share=number/sum(number)) %>%
  filter(NetCost!=1250,NetCost>-900,income!="All") %>% 
  ungroup() %>%
  mutate(order=1:n()) %>%
  mutate(NetCost=(170/50)*(1/1.21)*0.76*NetCost)
# Must simulate individual level data based on small perterbations of the SPSD/M output
test=NULL
for (v in unique(plotdata$income)){
  temp<-plotdata %>% 
    filter(income==v) %>% 
    select(NetCost,income,number)
  temp2 <- data.frame(x = rep(temp$NetCost, 10*temp$number),
                      income=v) %>%
    mutate(id=1:n()) %>%
    group_by(id) %>%
    mutate(x2=x-25/2+rnorm(1,0,25/3)) %>% # (25/2) since x is the upper bound of a $25 bin. SD of 25/3 ensure draws within the bins.
    ungroup()
  test<-test %>%
    bind_rows(temp2)
}
test<-test %>%
  mutate(order=1:n())
ggplot(test,aes(x=x,y=reorder(income,order),fill=..x..))+
  geom_density_ridges_gradient(show.legend = F)+
  coord_cartesian(ylim=c(1,8))+
  annotate("text",x=50,y=7.75,label="CTax > Rebate",size=3,hjust=0)+
  geom_segment(aes(x=50,xend=550,y=7.4,yend=7.4),arrow=arrow(length=unit(1,'mm')))+
  annotate("text",x=-50,y=7.75,label="Rebate > CTax",size=3,hjust=1)+
  geom_segment(aes(x=-50,xend=-550,y=7.4,yend=7.4),arrow=arrow(length=unit(1,'mm')))+
  geom_hline(yintercept=0,size=1)+
  scale_fill_viridis(name = "", option = "C")+
  geom_vline(xintercept = 0,size=1)+
  mytheme+
  scale_x_continuous(label=dollar)+
  scale_y_discrete(expand=c(0,0))+
  labs(x="Net Carbon Tax Costs per Year ($2020)",
       y="Total Household Annual Income                ",
       title="Net Cost to Households of a $170/t Federal Carbon Tax in Alberta",
       subtitle="Source: Own calculations from Statistics Canada's SPSM/D version 28.",
       caption="Assumes 2% inflation and uniform behavioural response equivalent to 25% lower emissions from 2022. Graph by @trevortombe")
ggsave("IncomePlot_2030.png",width=7,height=4.5,dpi=300)

# Share with CTax costs > rebate
col<-c("#CC2529","#396ab1","#3E9651","#DA7C30","#535154","#6B4C9A","#922428","#948B3D")
plotdata<-read_excel("CTaxCosts_2030.xlsx",sheet="NetCostwithFed") %>%
  gather(income,number,-NetCost) %>%
  group_by(income) %>%
  filter(NetCost!=1250,income!="All") %>%
  mutate(share=number/sum(number))  %>%
  mutate(negative=(NetCost<=0)) %>%
  group_by(income,negative) %>%
  summarise(share=sum(share)) %>%
  ungroup() %>%
  mutate(income=factor(income,levels=c("<$30k","$30-60k","$60-90k","$90-120k","$120-150k",">$150k")))
ggplot(plotdata,aes(income,share,group=negative,fill=negative))+
  geom_col(position='stack')+
  scale_fill_manual(name="",values=col[1:2],label=c("CTax Costs > Rebate","Rebate > CTax Costs"))+
  mytheme+
  scale_y_continuous(label=percent_format(accuracy=1))+
  labs(x="Total Household Annual Income",y="Share of Households",
       caption="Graph by @trevortombe",
       title="How Are Disposable Incomes Affected by the Federal Carbon Tax + Rebate?",
       subtitle="Source: own calculations from Statistics Canada's SPSM/D version 28.")
ggsave("disp_income.png",width=8,height=4,dpi=300)

# Plot Urban and Rural Areas
plotdata<-read_excel("UrbanRural.xlsx",range="A1:E7") %>%
  select(income=HouseholdIncome,`Cities >500,000`,`Cities <30,000 and Rural Areas`) %>%
  gather(type,cost,-income) %>%
  mutate(income=factor(income,levels=c("<$30k","$30-60k","$60-90k","$90-120k","$120-150k",">$150k")))
ggplot(plotdata,aes(income,cost,group=type,fill=type))+
  geom_col(position='dodge')+
  geom_hline(yintercept=0,size=1)+
  scale_fill_manual(name="",values=col[1:2])+
  mytheme+
  scale_y_continuous(label=dollar,limit=c(NA,1000))+
  labs(x="Total Household Annual Income",y="Annual Carbon Costs, Before Rebate",
       title="Carbon Tax Costs for Alberta Households ($50/t for 2022)",
       caption="Graph by @trevortombe",
       subtitle="Source: own calculations from Statistics Canada's SPSM/D version 28. Excludes rebates.")
ggsave("rural_urban.png",width=8,height=4,dpi=300)
  



