
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(gtsummary)
library(gt)
library(palmerpenguins)
library(flextable)
library(lubridate)
library(scales)

LT50 <- read_excel("data/LT50_data.xlsx")

#omit NA in before/after column 
LT50 <- LT50[complete.cases(LT50[,8]),]

#create dataframe for SD
LT50_sd <- LT50%>%
  group_by(state, species) %>%
  dplyr::summarise(meanLT50=mean(LT50),
                   sdLT50=sd(LT50))






table3_plot <- ggplot(data= LT50_sd, aes(x=state, y= meanLT50, group = species, color = species))+
  geom_violin(trim = FALSE, position = position_dodge(width = 1))+
  geom_errorbar(aes(x= state, ymin= meanLT50-LT50_sd$sdLT50, ymax=meanLT50+LT50_sd$sdLT50, group=species, width= 0.5), position = position_dodge(width = 1))+
  scale_color_manual(values = c("Acer saccharum" = "red", "Liriodendron tulipifera" = "blue", "Fagus grandifolia" = "black"))+
  labs(y=expression("LT"["50"]/"Temperature (°C)"))+
  xlab("Species")+
  theme_bw()+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+
  ggtitle("LT50 by state and species")

table3_plot
