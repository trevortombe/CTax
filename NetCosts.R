source("core.R")
library(viridis)

# Distribution of net carbon tax costs across all households
plotdata<-read_excel("CTaxCosts.xlsx",sheet="NetCosts") %>%
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
#plotdata<-read_excel("CTaxCosts.xlsx",sheet="NetCosts") %>%
plotdata<-read_excel("CTaxCosts_update.xlsx",sheet="NetCostwithFed") %>% # using new Fed rebate numbers
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
       #caption="Graph by @trevortombe",
       title="Distribution of Net Carbon Tax Costs for Alberta Families in 2018",
       subtitle="Displays the distribution of costs to households with one nuclear family, by annual income.",
       caption="Source: Own calculations from Statistics Canada's SPSD/M version 27. Based on $30 per tonne.")
ggsave("IncomePlot.png",width=9,height=4,dpi=300)

library(grid)
title.grob <- textGrob(
  label = "Distribution of Net Carbon Tax Costs for Alberta Families in 2018",
  x = unit(1, "cm"), 
  y = unit(0, "cm"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 14,fontface="bold"))
subtitle.grob <- textGrob(
  label = "Displays the distribution of costs to households with one nuclear family, by annual income.",
  x = unit(1, "cm"), 
  y = unit(0, "cm"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 8,col="gray40"))
p1<-arrangeGrob(p,top=subtitle.grob)
p2<-arrangeGrob(p1,top=title.grob)
grid.arrange(p2)
ggsave("IncomePlot.png",p2,width=6.5,height=4,dpi=300)


## Comparing the Alberta rebates to your estimate of the federal rebates
plotdata<-read_excel("CTaxCosts.xlsx",sheet="NetCostwithFed") %>%
  gather(income,number,-NetCost) %>%
  group_by(income) %>%
  mutate(share=number/sum(number)) %>%
  filter(NetCost!=1150,NetCost>-700,income!="All") %>% 
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
       subtitle="Source: Own calculations from Statistics Canada's SPSD/M version 27.
Reflects Federal rebates of $444 (first adult), $222 (spouse), and $111 (child).",
       caption="Graph by @trevortombe")
ggsave("Federal2020.png",width=6.5,height=4,dpi=300)

# All incomes
plotdata<-read_excel("CTaxCosts_update.xlsx",sheet="NetCostwithFed") %>%
  gather(income,number,-NetCost) %>%
  group_by(income) %>%
  mutate(share=number/sum(number)) %>%
  filter(NetCost!=1150,NetCost>-700,income=="All")
plotdata <- data.frame(x = rep(plotdata$NetCost, 10*plotdata$number)) %>%
  mutate(id=1:n()) %>%
  group_by(id) %>%
  mutate(x2=x-25/2+rnorm(1,0,25/3)) %>% # (25/2) since x is the upper bound of a $25 bin. SD of 25/3 ensure draws within the bins.
  ungroup()
ggplot(plotdata,aes(x=x2,y=1,fill=..x..))+
  geom_density_ridges_gradient(show.legend = F)+
  geom_point(aes(x=25,y=1.00535),color="transparent",show.legend = F)+
  annotate("text",x=25,y=Inf,label="CTax > Rebate",size=3,hjust=0,vjust=1.5)+
  geom_segment(aes(x=25,xend=350,y=1.005,yend=1.005),arrow=arrow(length=unit(1,'mm')))+
  annotate("text",x=-25,y=Inf,label="Rebate > CTax",size=3,hjust=1,vjust=1.5)+
  geom_segment(aes(x=-25,xend=-350,y=1.005,yend=1.005),arrow=arrow(length=unit(1,'mm')))+
  scale_fill_viridis(name = "", option = "C")+
  geom_vline(xintercept = 0,size=1)+
  geom_hline(yintercept=0,size=1)+
  mytheme+
  scale_x_continuous(label=dollar)+
  scale_y_discrete(expand=c(0,0))+
  labs(x="Net Carbon Tax Costs ($ per Year)",
       y="Density of Households",
       title="Net Cost to Households of the Federal $30/t Carbon Tax Minus 2020 Rebate",
       subtitle="Source: Own calculations from Statistics Canada's SPSD/M version 27.1.
Reflects Federal rebates of $444 (first adult), $222 (spouse), and $111 (child).",
       caption="Graph by @trevortombe")
ggsave("Federal2020.png",width=7,height=4,dpi=300)

## Comparing Both Provincial and Federal On Same Graph
data1<-read_excel("CTaxCosts.xlsx",sheet="NetCostwithFedx2") %>%
  select(NetCost,number=All) %>%
  mutate(type="Federal Rebates")
data2<-read_excel("CTaxCosts.xlsx",sheet="NetCostsx2") %>%
  select(NetCost,number=All) %>%
  mutate(type="Alberta Rebates")
plotdata<-data1 %>%
  bind_rows(data2) %>%
  group_by(type) %>%
  mutate(share=number/sum(number)) %>%
  filter(NetCost!=1150,NetCost>min(NetCost))
# Must simulate individual level data based on small perterbations of the SPSD/M output
test=NULL
for (v in unique(plotdata$type)){
  temp<-plotdata %>% 
    filter(type==v) %>% 
    select(NetCost,type,number)
  temp2 <- data.frame(x = rep(temp$NetCost, 10*temp$number),
                      type=v) %>%
    mutate(id=1:n()) %>%
    group_by(id) %>%
    mutate(x2=x-25/2+rnorm(1,0,25/3)) %>% # (25/2) since x is the upper bound of a $25 bin. SD of 25/3 ensure draws within the bins.
    ungroup()
  test<-test %>%
    bind_rows(temp2)
}
test<-test %>%
  mutate(order=1:n())
ggplot(test,aes(x=x,y=reorder(type,order),fill=..x..))+
  geom_density_ridges_gradient(show.legend = F,scale=1.1)+
  coord_cartesian(ylim=c(1,3))+
  annotate("text",x=25,y=2.9,label="Net Tax (CTax > Rebate)",size=3,hjust=0)+
  geom_segment(aes(x=25,xend=350,y=2.75,yend=2.75),arrow=arrow(length=unit(1,'mm')))+
  annotate("text",x=-25,y=2.9,label="Net Rebate (Rebate > CTax)",size=3,hjust=1)+
  geom_segment(aes(x=-25,xend=-350,y=2.75,yend=2.75),arrow=arrow(length=unit(1,'mm')))+
  geom_hline(yintercept=0,size=1)+
  scale_fill_viridis(name = "", option = "C")+
  geom_vline(xintercept = 0,size=1)+
  mytheme+
  scale_x_continuous(label=dollar)+
  scale_y_discrete(expand=c(0,0))+
  labs(x="Net Carbon Tax Costs ($ per Year)",
       y="",
       title="Estimate of Net Costs Under the Federal Backstop",
       #caption="Graph by @trevortombe",
       subtitle="Assumed Federal rebates: $400 (first adult), $200 (spouse), and $100 (child).",
       caption="Source: Own calculations from Statistics Canada's SPSD/M version 27. Based on $30 per tonne.")
ggsave("CompareToFeds.png",width=9,height=4,dpi=300)

library(grid)
title.grob <- textGrob(
  label = "Carbon Tax Costs for All Households, AB vs Fed Rebates",
  x = unit(1, "cm"), 
  y = unit(0, "cm"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 14,fontface="bold"))
subtitle.grob <- textGrob(
  label = "Assumed Federal rebates: $400 (first adult), $200 (spouse or first child), and $100 (child).",
  x = unit(1, "cm"), 
  y = unit(0, "cm"),
  hjust = 0, vjust = 0,
  gp = gpar(fontsize = 8,col="gray40"))
p1<-arrangeGrob(p,top=subtitle.grob)
p2<-arrangeGrob(p1,top=title.grob)
grid.arrange(p2)
ggsave("CompareToFeds.png",p2,width=6.5,height=3.25,dpi=300)
