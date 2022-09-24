install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("patchwork")
install.packages("ggplotify")
install.packages("cowplot")

#load packages
library(dplyr)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggplotify)
library(cowplot)

urlfile1="https://raw.githubusercontent.com/myco-analytical/data/main/dna-8-31.csv?token=GHSAT0AAAAAABZFL633TZGC35JOA2DN4JFKYZPSIZQ"
dna1<-read.csv(url(urlfile1)) #data from CtPharma Gorilla Glue (gene expression & metabolite levels)

names(dna1)[names(dna1) == 'sample.no.'] <- 'sample_id' #changes name of variable to sample_id
names(dna1)[names(dna1) == 'rep.no.'] <- 'rep' 
names(dna1)[names(dna1) == 'X260.280'] <- 'r1'
names(dna1)[names(dna1) == 'X260.230'] <- 'r2' 
dna1 <- filter(dna1, conc != "na") #remove rows that contain missing values
dna1 <- filter(dna1, sample_id != 5) #remove sample 5 from dataframe 
dna1 <- filter(dna1, sample_id != 8)
dna1$conc <- as.numeric(dna1$conc) #coerce variable into a numeric one 
dna1$r1 <- as.numeric(dna1$r1)
dna1$r2 <- as.numeric(dna1$r2)
dna1$sample_id <- as.numeric(dna1$sample_id)

#summarise the data: 
sample_mean8.31<-summarise(
  group_by(dna1, bead, sample_id, .groups = TRUE), #columns to include 
  obs = n(), #counts the number of observations by group
  m_conc = mean(conc), #mean of the nucleic acid yield 
  m_r1 = mean(r1), #mean of the 260/280 values 
  m_r2 = mean(r2), #mean of the 260/230 values 
  sd_conc = sd(conc), #standard deviation of the nucleic acid yield values 
  sd_r1 = sd(r1),#standard deviation of the 260/280 values 
  sd_r2 = sd(r2))#standard deviation of the 260/280 values 
sample_mean8.31 <- select(sample_mean8.31, 1,2,5:10) #select specific rows of the dataframe while excluding others
sample_mean8.31
bead1 <- c("SS","SS","SS","SSG","SSG","SSG","SSG") #create object that contains a list 
sample_mean8.31$bead <- bead1 #coerce the bead variable to take on the array encompasses by the bead1 object
sample_mean8.31$bead <- as.factor(sample_mean8.31$bead) #coerce varible into a factor 
sample_mean8.31$sample_id <- as.character(sample_mean8.31$sample_id) #coerce variable into a character 

#create objects to encompass labels 
SSlab <- "Stainless Steel"
SSGlab <- "Stainless Steel + Garnette"

sample_mean8.31$bead <- factor(sample_mean8.31$bead, levels = c("SSG", "SS")) #the only point of doing this is so that I could change the order of the labels on the legend 

# only the first plot's code will be annotated
# the code for the following plots are nearly identical 

conc_plot_8.31<- ggplot(data=sample_mean8.31, #use this dataframe 
                   aes(color = bead)) + #distinguish points by bead variable 
  geom_point(aes(x=sample_id, y=m_conc), #geom is points as opposed to bars or boxplots 
           stat="identity")   +   #prevents R from doing new calculations on the data and just present them as they are 
scale_x_discrete(limits = c("1","2","3","4","5","7","8","9","10")) + #set x axis ticks to characters to represent the samples 
  scale_y_continuous(limits = c(-15,60), #y axis range 
                     breaks = seq(-15,60, 
                                  by = 15)) + #y axis increments 
  geom_errorbar(aes(x=sample_id, #argument is for standard deviation for the bar chart 
                    ymin=m_conc-sd_conc, #minimum for the standard deviation 
                    ymax=m_conc+sd_conc,  #maximum for the standard deviation 
                    width=.2), #width of the error bar
                position = position_dodge(0.9), 
                size=1.0, #size of the error bar 
                linetype = 1)+ #line style for error bar
ylab("Nucleic acid concentration (ng/µL)") + #y axis label 
  ggtitle("Nucleic Acid Yield") + #plot title  
  xlab("") + 
  scale_color_discrete(name = "Bead type",  #legend title 
                                  labels = c(SSGlab, SSlab)) +#legend labels 
  theme(                                           #self explanatory theme modifications 
    axis.line.y.left = element_line(color = "black"), 
    axis.title.y.left = element_text(color="black", face="bold", size=18),
    axis.text.y.left = element_text(color="black", face="bold", size =18),
    axis.text.x = element_text(color="black", face="bold", size =14, vjust=0.5),  
    axis.ticks = element_line(size = 1), #size of tick on x and y axes
    axis.ticks.length = unit(-0.25, "cm"), #makes ticks go inside the plot 
    axis.title.x=element_text(color="black",face = "bold", size=18, vjust =0.6), 
    plot.title = element_text(face="bold", size=19, hjust = 0.5), 
    plot.caption = element_text(size=12.5, colour ="red"),
    plot.tag = element_text(color="black",face = "bold", size=18),
    axis.line = element_line(colour = "black"),
    legend.position = "none", #don't show the legend 
    legend.title = element_blank(),
    legend.text = element_text(size = 16, face ="bold"),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) 
conc_plot_8.31

#note: legend position must not say 'none' for get_legend() to properly grab the legend 
leg <- get_legend(conc_plot_8.31)  
leg <-as.ggplot(leg, vjust = 0.4) #coerce into a ggplot object 



r1plot<-ggplot(data=sample_mean8.31, aes(color = bead)) + #use dataframe 'GG.CW' in ggplot function 
  geom_point(aes(x=sample_id, y=m_r1), #geom_bar inherits the dataframe GG.CW 
             stat="identity")   +   #prevents R from doing new calculations on the data and just present them as they are 
  scale_x_discrete(limits = c("1","2","3","4","5","7","8","9","10")) +
  scale_y_continuous(limits = c(0,3),
                     breaks = seq(0,3, by = 0.5)) +
  geom_errorbar(aes(x=sample_id, #argument is for standard error for the bar chart 
                    ymin=m_r1-sd_r1, #minimum for the standard error
                    ymax=m_r1+sd_r1,  
                    width=.2), #width of the error bar
                position = position_dodge(0.9), 
                size=1.0, #size of the error bar 
                linetype = 1)+ #line style for error bar
  ylab("260/280 ratio") + 
  xlab("") +   
  ggtitle("260/280 ratios") + 
  scale_fill_continuous(labels = c(SSlab, SSGlab)) +
  scale_color_discrete(name = "Bead type",  #legend title 
                       labels = c(SSGlab, SSlab)) +#legend labels  
  theme(
    axis.line.y.left = element_line(color = "black"), 
    axis.title.y.left = element_text(color="black", face="bold", size=18),
    axis.text.y.left = element_text(color="black", face="bold", size =18),
    axis.text.x = element_text(color="black", face="bold", size =14, vjust=0.5),  #
    axis.ticks = element_line(size = 1), #size of tick on x and y axes
    axis.ticks.length = unit(-0.25, "cm"), #makes ticks go inside the plot 
    axis.title.x=element_text(color="black",face = "bold", size=18, vjust =0.6), 
    plot.title = element_text(face="bold", size=19, hjust = 0.5), 
    plot.caption = element_text(size=12.5, colour ="red"),
    plot.tag = element_text(color="black",face = "bold", size=18),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 16, face ="bold"),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  geom_hline(yintercept=1.8, linetype='dashed', color=c('#4E784E'))

r1plot

r2plot<-ggplot(data=sample_mean8.31, aes(color = bead)) + #use dataframe 'GG.CW' in ggplot function 
  geom_point(aes(x=sample_id, y=m_r2), #geom_bar inherits the dataframe GG.CW 
             stat="identity")   +   #prevents R from doing new calculations on the data and just present them as they are 
  scale_x_discrete(limits = c("1","2","3","4","5","7","8","9","10")) +
  scale_y_continuous(limits = c(0,2),
                     breaks = seq(0,2, by = 0.25)) +
  geom_errorbar(aes(x=sample_id, #argument is for standard error for the bar chart 
                    ymin=m_r2-sd_r2, #minimum for the standard error
                    ymax=m_r2+sd_r2,  
                    width=.2), #width of the error bar
                position = position_dodge(0.9), 
                size=1.0, #size of the error bar 
                linetype = 1)+ #line style for error bar
  ylab("260/230 ratio") + 
  xlab("Sample ID") +   
  ggtitle("260/230 ratios") + 
  scale_fill_continuous(labels = c(SSGlab, SSlab)) +
  #
  scale_color_discrete(name = "Bead type",  #legend title 
                       labels = c(SSGlab, SSlab)) +#legend labels 
  theme(
    axis.line.y.left = element_line(color = "black"), 
    axis.title.y.left = element_text(color="black", face="bold", size=18),
    axis.text.y.left = element_text(color="black", face="bold", size =18),
    axis.text.x = element_text(color="black", face="bold", size =14, vjust=0.5),  #
    axis.ticks = element_line(size = 1), #size of tick on x and y axes
    axis.ticks.length = unit(-0.25, "cm"), #makes ticks go inside the plot 
    axis.title.x=element_text(color="black",face = "bold", size=18, vjust =0.6), 
    plot.title = element_text(face="bold", size=19, hjust = 0.5), 
    plot.caption = element_text(size=12.5, colour ="red"),
    plot.tag = element_text(color="black",face = "bold", size=18),
    axis.line = element_line(colour = "black"),
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 16, face ="bold"),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) +
  geom_hline(yintercept=1.8, linetype='dashed', color=c('#4E784E'))

r2plot

#create multi-paneled figure 
combined <- (conc_plot_8.31/r1plot/r2plot/leg)
combined


ggsave("data.png", width = 7, height = 16) #save figure with these specifications 

