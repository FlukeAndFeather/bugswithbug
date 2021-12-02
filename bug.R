library(tidyverse)

d3<-read.csv("2014//GSC plant 2014-07-28.csv")
d3$plant<-factor(d3$plant)
#calculate mean stem height
d3$mean.ht<-unlist(lapply(strsplit(as.character(d3$height),","),function(x) mean(as.numeric(x))))
#total number of leaves
d3$sum.leaves<-unlist(lapply(strsplit(as.character(d3$X.leaves),","),function(x) sum(as.numeric(x))))
#calculate total stem height
d3$total.stem<-d3$mean.ht*d3$X.stems
#calculate mean stem diameter
d3$mean.diam<-unlist(lapply(strsplit(as.character(d3$stemdia),","),function(x) mean(as.numeric(x))))
#calculate total stem crosssectional area
d3$stem.area<-pi*(d3$mean.diam/2)^2*d3$X.stems
#calculate plant ages
d3$start.date<-as.Date(ifelse(d3$co.nun==4,"4/8/2014",ifelse(d3$co.num==5,"5/7/2014","6/8/2014")),format="%m/%d/%Y")
d3$obs.date<-as.Date(d3$obs.date,format="%d-%b-%y")
d3$age.days<-difftime(d3$obs.date,d3$start.date,units="days")
d3$age.weeks<-difftime(d3$obs.date,d3$start.date,units="weeks")
d3$age.weeks.factor<-factor(round(d3$age.weeks))

d4<-read.csv("2014//GSC latex 2014-07-28.csv")
d4<-d4[-which(d4$plant=="Control"),] #remove filter papers without latex
d4$plant<-factor(d4$plant)
d4<-aggregate(latex~plant,d4,sum)

dp.2014<-merge(d3,d4,by="plant")

p1 <- ggplot(dp.2014, aes(x = age.weeks.factor,
                          y = total.stem,
                          fill = species,
                          group = species,
                          shape = age.weeks.factor)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = .1) +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.y = "mean", geom = "point", size = 4, stroke = 0.7) +
  theme(legend.position = "none",
        plot.margin = margin(30,0,0,0)) +
  scale_shape_manual(values = c(21, 22, 23)) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4"),
                    name = "plant species",
                    labels = c(expression(italic("A. fascicularis")),
                               expression(italic("A. speciosa")))) +
  ylab("total stem length (cm)") +
  xlab("plant age (weeks)")
