source("core.R")
library(viridis)

# Distribution of net carbon tax costs across all households
plotdata<-read_excel("CTaxCosts.xlsx") %>%
  gather(income,number,-NetCost) %>%
  group_by(income) %>%
  mutate(share=number/sum(number)) %>%
  filter(NetCost!=1150,NetCost>-550,income=="All")
plotdata <- data.frame(x = rep(plotdata$NetCost, 10*plotdata$number)) %>%
  mutate(id=1:n()) %>%
  group_by(id) %>%
  mutate(x2=x-25/2+rnorm(1,0,25/3)) %>% # (25/2) since x is the upper bound of a $25 bin. SD of 25/3 ensure draws within the bins.
  ungroup()
ggplot(plotdata,aes(x=x2,y=1,fill=..x..))+
  geom_density_ridges_gradient(show.legend = F)+
  geom_point(aes(x=25,y=1.0035),color="transparent",show.legend = F)+
  annotate("text",x=25,y=1.0033,label="CTax > Rebate",size=3,hjust=0)+
  geom_segment(aes(x=25,xend=350,y=1.0031,yend=1.0031),arrow=arrow(length=unit(1,'mm')))+
  annotate("text",x=-25,y=1.0033,label="Rebate > CTax",size=3,hjust=1)+
  geom_segment(aes(x=-25,xend=-350,y=1.0031,yend=1.0031),arrow=arrow(length=unit(1,'mm')))+
  scale_fill_viridis(name = "", option = "C")+
  geom_vline(xintercept = 0,size=1)+
  geom_hline(yintercept=0,size=1)+
  mytheme+
  scale_x_continuous(label=dollar)+
  scale_y_discrete(expand=c(0,0))+
  labs(x="Net Carbon Tax Costs ($ per Year)",
       y="All Income Levels        ",
       title="Net Cost to Households of Alberta's Carbon Tax in 2018",
       subtitle="Source: Own calculations from Statistics Canada's SPSM/D version 27.",
       caption="Graph by @trevortombe")
ggsave("AllHouseholds.png",width=6.5,height=4,dpi=300)

# Costs by Household Total Income
plotdata<-read_excel("CTaxCosts.xlsx") %>%
  gather(income,number,-NetCost) %>%
  group_by(income) %>%
  mutate(share=number/sum(number)) %>%
  filter(NetCost!=1150,NetCost>-550,income!="All") %>% 
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
  coord_cartesian(ylim=c(1,8))+
  annotate("text",x=25,y=7.75,label="CTax > Rebate",size=3,hjust=0)+
  geom_segment(aes(x=25,xend=350,y=7.4,yend=7.4),arrow=arrow(length=unit(1,'mm')))+
  annotate("text",x=-25,y=7.75,label="Rebate > CTax",size=3,hjust=1)+
  geom_segment(aes(x=-25,xend=-350,y=7.4,yend=7.4),arrow=arrow(length=unit(1,'mm')))+
  geom_hline(yintercept=0,size=1)+
  scale_fill_viridis(name = "", option = "C")+
  geom_vline(xintercept = 0,size=1)+
  mytheme+
  scale_x_continuous(label=dollar)+
  scale_y_discrete(expand=c(0,0))+
  labs(x="Net Carbon Tax Costs ($ per Year)",
       y="Total Household Annual Income                ",
       title="Net Cost to Households of Alberta's Carbon Tax in 2018",
       subtitle="Source: Own calculations from Statistics Canada's SPSD/M version 27.",
       caption="Graph by @trevortombe")
ggsave("IncomePlot.png",width=6.5,height=5,dpi=300)
