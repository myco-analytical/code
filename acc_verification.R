### 9-19-2022 

urlfile1="https://raw.githubusercontent.com/myco-analytical/data/main/acc_ver.csv?token=GHSAT0AAAAAABZFL633C2L7WXURR4HZZCBEYZPTGHA"
acc1<-read.csv(url(urlfile1)) 

acc1 <- filter(acc1, date == "9-19")
acc1

ggplot(data = acc1) + 
  geom_point(aes(x=channel, y=vol), #geom is points as opposed to bars or boxplots 
             stat="identity", #prevents R from doing new calculations on the data and just present them as they are
             size =3)   +  
  scale_y_continuous(limits = c(95,105), #y axis range 
                     breaks = seq(95,105, 
                                  by = 1)) + #y axis increments 
  geom_hline(yintercept=100, linetype='dashed', color=c('#2a9d8f'), size =2) +
  ylab("Volume of Dispensed Water (µL)") + xlab("Channel") +

theme(                                          
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

ggsave("verification_data.png", width = 5, height = 5) #save figure with these specifications 
######

###### 
#9-20-2022
######

acc1 #dataframe

vol_mean_9.20<-summarise(
  group_by(acc1, channel), #columns to include 
  obs = n(), #counts the number of observations by group
  m_vol = mean(vol), #mean of the nucleic acid yield
  sd_vol = sd(vol))#standard deviation of the 260/280 values 
vol_mean_9.20

acc1_spread<-ggplot(data = vol_mean_9.20) + 
  geom_point(aes(x=channel, y=m_vol), #geom is points as opposed to bars or boxplots 
             stat="identity", #prevents R from doing new calculations on the data and just present them as they are
             size =3, 
             color = "red")   +  
  geom_errorbar(aes(x=channel, #argument is for standard deviation for the bar chart 
                    ymin=m_vol-sd_vol, #minimum for the standard deviation 
                    ymax=m_vol+sd_vol,  #maximum for the standard deviation 
                    width=.2), #width of the error bar
                position = position_dodge(0.9), 
                size=1.0, #size of the error bar 
                linetype = 1, color="red")+ #line style for error bar
  scale_y_continuous(limits = c(95,106), #y axis range 
                     breaks = seq(95,106, 
                                  by = 1)) + #y axis increments 
  geom_hline(yintercept=100, linetype='dashed', color=c('#2a9d8f'), size =2) +
  ylab("Volume of Dispensed Water (µL)") + xlab("Channel")+
  ggtitle("Plate Washer acc1uracy Verification") +
  labs(caption = "n = 3") +
  theme(                                          
    axis.line.y.left = element_line(color = "black"), 
    axis.title.y.left = element_text(color="black", face="bold", size=18),
    axis.text.y.left = element_text(color="black", face="bold", size =18),
    axis.text.x = element_text(color="black", face="bold", size =14, vjust=0.5),  
    axis.ticks = element_line(size = 1), #size of tick on x and y axes
    axis.ticks.length = unit(-0.25, "cm"), #makes ticks go inside the plot 
    axis.title.x=element_text(color="black",face = "bold", size=18, vjust =0.6), 
    plot.title = element_text(face="bold", size=19, hjust = 0.5), 
    plot.caption = element_text(size=20, colour ="black", face = "bold"),
    plot.tag = element_text(color="black",face = "bold", size=18),
    axis.line = element_line(colour = "black"),
    legend.position = "none", #don't show the legend 
    legend.title = element_blank(),
    legend.text = element_text(size = 16, face ="bold"),
    panel.border = element_rect(colour = "black", fill=NA, size=1),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()) 
acc1_spread


ggsave("verification-spread_data.png", width = 6, height = 5) #save figure with these specifications 

