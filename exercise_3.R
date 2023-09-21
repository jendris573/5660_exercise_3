library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(tidyverse)
library(gtsummary)
library(gt)
library(knitr)
library(flextable)

#Load NOAA Climate Data Online data
climate <- read_excel("data/climate.xlsx")

#omit NA in temperature recordings 
climate <- climate[complete.cases(climate[,6]),]

#create column for month
climate <- mutate(climate, month=month(climate$date))

#create column for year
climate <- mutate(climate, Year=year(climate$date))

#create column for julian date
climate$julian_date <- yday(climate$date)

#create column with the meanTMAX & SD
climate <- climate %>% 
  group_by(Year) %>%
  mutate(meanTMAX = mean(TMAX)) %>%
  mutate(TMAXsd=sd(TMAX))

#Load seedling data
seedlings <- read_excel("data/seedlings.xlsx")

#create column with the mean height
seedlings <- seedlings %>% 
  group_by(species) %>%
  mutate(mean_height = mean(height))

#create dataframe for SD  
sd_seedlings <- seedlings %>%
  group_by(species) %>%
  summarize(mean_height = mean(height), 
            seedling_sd=sd(height))

LT50 <- read_excel("data/LT50_data.xlsx")

#omit NA in before/after column 
LT50 <- LT50[complete.cases(LT50[,8]),]

#create dataframe for SD
LT50_sd <- LT50%>%
  group_by(state, species) %>%
  dplyr::summarise(meanLT50=mean(LT50),
                   sdLT50=sd(LT50))

table1 <- climate %>%
  select(Year, TMAX) %>%
  tbl_summary(by = Year,
              missing= "no",
              digits = all_continuous() ~1,
              label = list(TMAX ~ "Temperature (°C)"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n}")) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% #number of digits displayed for p-values
  modify_caption("Table 1. Mean high temperature for 2022 and 2023.") %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(
    update = list(
      label ~ '',
      stat_1 ~ '**2022**', #is markdown **bold** formatting
      stat_2 ~ '**2023**',
      p.value ~ '**P-value**')) %>%
  as_gt() %>%
  gt::tab_options(heading.align = "left")
table1

table1_plot <-ggplot(data = climate, (aes(x=Year, y=meanTMAX, group = Year, colour = factor(Year)))) +
  geom_point()+
  geom_errorbar(aes(x= Year, ymin= meanTMAX-TMAXsd, ymax=meanTMAX+TMAXsd, group=Year, width= 0.5))+
  ylab("Temperature (°C)")+
  xlab("Year")+
  labs(color = "Year")+
  scale_x_continuous(breaks = c(2022, 2023))+
  scale_fill_discrete(name="Year")+
  theme_bw()+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+
  ggtitle("Maximum Temperatures by Year")

table1_plot

table2 <- seedlings %>%
  select(species, height, mean_dia) %>%
  tbl_summary(by = species,
              missing= "no",
              digits = all_continuous() ~1,
              label = list(mean_dia ~ "Mean diameter (cm)",
                           height ~ "height (cm)"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n}")) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% #number of digits displayed for p-values
  modify_caption("Table 2. Mean stem diameter and overall height measurements (cm) of three tree seedling species.") %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(
    update = list(
      label ~ '',
      stat_1 ~ '***Acer saccharum***', #is markdown **bold** formatting
      stat_2 ~ '***Quercus alba***',
      stat_3 ~ '***Quercus coccinea***',
      p.value ~ '**P-value**')) %>%
  as_gt() %>%
  gt::tab_options(heading.align = "left")

table2


table2_plot <- ggplot(data= seedlings, aes(x=species, y= height, group = species))+
  geom_boxplot()+
  geom_errorbar(aes(x= species, ymin= mean_height-sd_seedlings$seedling_sd, ymax=mean_height+sd_seedlings$seedling_sd, group=species, width= 0.5))+
  ylab("Height (cm)")+
  xlab("Species")+
  theme_bw()+
  theme(panel.border = element_blank(),  
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank())+
  ggtitle("Seedling height by species")

table2_plot

### table 3
table3 <- LT50 %>%
  select(species, LT50) %>%
  tbl_summary(by = species,
              missing= "no",
              digits = all_continuous() ~1,
              label = list(species ~ "Species",
                           LT50 ~ "Temperature"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n}")) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% #number of digits displayed for p-values
  modify_caption("Table 3. Mean LT50 for three hardwood tree species")%>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(
    update = list(
      label ~ '',
      stat_1 ~ '***Acer saccharum***', 
      stat_2 ~ '***Fagus grandifolia***',
      stat_3 ~ '***Liriodendron tulipifera***',
      p.value ~ '**P-value**')) %>%
  as_gt() %>%
  gt::tab_options(heading.align = "left")

table3

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
