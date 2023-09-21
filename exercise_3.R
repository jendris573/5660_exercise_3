
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

table3 <- seedlings %>%
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
table3



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
