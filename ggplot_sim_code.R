ggplot(bc, aes(x=times,y=bmc)) + geom_line(aes(y=bmc),color='red') +
  scale_x_continuous(name = "Time since start (mins)", breaks= 0:40*1200, 
                     labels=0:40*20) +
  scale_y_continuous(name = "Temperature (C)") +
  theme_minimal() + # start with a minimal theme and add what we need
  theme(text = element_text(color = "gray20"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -3), # move title away from axis
        axis.title.y = element_text(vjust = 2) # move away for axis
  )


newhist <- data.frame(cbind(seq(0,99.9,0.1),bmc$bmchist))
colnames(newhist) <- c("temp","bmchist")
#ggplot(newhist, aes(bmchist)) + geom_bar(stat="identity",fill="red") + coord_flip()+
#  theme_minimal() + # start with a minimal theme and add what we need
#  scale_y_continuous(name = "Observation count",limits = tlims) +
ggplot(newhist, aes(y=newhist$bmc,x=newhist$temp)) + 
  geom_bar(stat="identity",fill="red")+
  coord_flip()+
  scale_x_continuous(name = "",limits = tlims)  +
  scale_y_continuous(name = "Observation count")  +
  theme(text = element_text(color = "gray10"),
      axis.text = element_text(face = "italic",size=10),
      axis.title.x = element_text(vjust = -3, size=14), # move title away from axis
      axis.title.y = element_blank(),# move away for axis
      axis.text.y  = element_blank(),# remove y ticks
      panel.grid.major.y=element_line(colour="black", linetype = "dashed"),
      panel.grid.major.x=element_blank()
  )
tlims=c(max(c(1,min(which(newhist$bmchist>0))-1))/10.0,min(c(999,max(which(newhist$bmchist>0))+1))/10.0)