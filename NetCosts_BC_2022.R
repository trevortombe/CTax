source("core.R")
library(viridis)

# Distribution of net carbon tax costs across all households
plotdata<-read_excel("CTaxCosts.xlsx",sheet="NetCostsBC2022") %>%
  gather(income,number,-NetCost) %>%
  group_by(income) %>%
  mutate(share=number/sum(number)) %>%
  filter(NetCost!=2350,NetCost>-550,income=="All")
plotdata <- data.frame(x = rep(plotdata$NetCost, 10*plotdata$number)) %>%
  mutate(id=1:n()) %>%
  group_by(id) %>%
  mutate(x2=x-25/2+rnorm(1,0,25/3)) %>% # (25/2) since x is the upper bound of a $25 bin. SD of 25/3 ensure draws within the bins.
  ungroup()
ggplot(plotdata,aes(x=x2,y=1,fill=..x..))+
  geom_density_ridges_gradient(show.legend = F)+
  geom_point(aes(x=25,y=1.0025),color="transparent",show.legend = F)+
  annotate("text",x=25,y=1.0023,label="CTax > Rebate",size=3,hjust=0)+
  geom_segment(aes(x=25,xend=350,y=1.0021,yend=1.0021),arrow=arrow(length=unit(1,'mm')))+
  annotate("text",x=-25,y=1.0023,label="Rebate > CTax",size=3,hjust=1)+
  geom_segment(aes(x=-25,xend=-350,y=1.0021,yend=1.0021),arrow=arrow(length=unit(1,'mm')))+
  scale_fill_viridis(name = "", option = "C")+
  geom_vline(xintercept = 0,size=1)+
  geom_hline(yintercept=0,size=1)+
  mytheme+
  coord_cartesian(xlim=c(-400,2000))+
  scale_x_continuous(label=dollar)+
  scale_y_discrete(expand=c(0,0))+
  labs(x="Net Carbon Tax Costs ($ per Year)",
       y="All Income Levels        ",
       title="Net Cost to Households of BC's Carbon Tax in 2022",
       subtitle="Displays the distribution of costs to households with one census family.",
       caption="Source: Own calculations from Statistics Canada's SPSD/M version 29.")
ggsave("AllHouseholds_BC.png",width=6.5,height=4,dpi=300)

# Costs by Household Total Income
plotdata<-read_excel("CTaxCosts.xlsx",sheet="NetCostsBC2022") %>%
  gather(income,number,-NetCost) %>%
  group_by(income) %>%
  mutate(share=number/sum(number)) %>%
  filter(NetCost!=2350,NetCost>-550,income!="All") %>% 
  ungroup() %>%
  mutate(order=1:n())
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
  coord_cartesian(ylim=c(1,8),xlim=c(-400,2000))+
  annotate("text",x=25,y=7.75,label="Net Tax (CTax > Rebate)",size=3,hjust=0)+
  geom_segment(aes(x=25,xend=350,y=7.4,yend=7.4),arrow=arrow(length=unit(1,'mm')))+
  annotate("text",x=-25,y=7.75,label="Net Rebate (Rebate > CTax)",size=3,hjust=1)+
  geom_segment(aes(x=-25,xend=-350,y=7.4,yend=7.4),arrow=arrow(length=unit(1,'mm')))+
  geom_hline(yintercept=0,size=1)+
  scale_fill_viridis(name = "", option = "C")+
  geom_vline(xintercept = 0,size=1)+
  mytheme+
  scale_x_continuous(label=dollar)+
  scale_y_discrete(expand=c(0,0))+
  labs(x="Net Carbon Tax Costs ($ per Year)",
       y="Total Annual Income                ",
       title="Distribution of Net Carbon Tax Costs for British Columbia Families in 2022",
       subtitle="Displays the distribution of costs to households with one census family, by annual income.",
       caption="Source: Own calculations from Statistics Canada's SPSD/M version 29.")
ggsave("IncomePlot_BC.png",width=8,height=4,dpi=300)
  