
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(gtsummary)
library(gt)
library(palmerpenguins)
library(flextable)

spruce <- read_excel("data/white_spruce.xlsx")
fir <- read_excel("data/balsam_fir.xlsx")
red_maple <- read_excel("data/red_maple.xlsx")
sugar_maple <- read_excel("data/sugar_maple.xlsx")
aspen <- read_excel("data/quaking_aspen.xlsx")
whitecedar <- read_excel("data/eastern_whitecedar.xlsx")

DBH <- rbind(spruce, fir, red_maple, sugar_maple, aspen, whitecedar)



#Load NOAA Climate Data Online data
climate <- read_excel("data/climate.xlsx")

#omit NA in temperature recordings 
climate <- climate[complete.cases(climate[,6]),]

#create column for month
climate <- mutate(climate, month=month(climate$date))

#create column for year
climate <- mutate(climate, year=year(climate$date))

## create column for julian date
climate$julian_date <- yday(climate$date)

table1 <- climate %>%
  select(year, TMAX) %>%
  tbl_summary(by = year,
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

  
  

seedlings <- read_excel("data/seedlings.xlsx")

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










spruce <- read_excel("data/white_spruce.xlsx")
fir <- read_excel("data/balsam_fir.xlsx")
red_maple <- read_excel("data/red_maple.xlsx")
sugar_maple <- read_excel("data/sugar_maple.xlsx")
aspen <- read_excel("data/quaking_aspen.xlsx")
whitecedar <- read_excel("data/eastern_whitecedar.xlsx")

diameter <- rbind(spruce, fir, red_maple, sugar_maple, aspen, whitecedar)

table1 <- diameter %>%
  filter(family =="Cupressaceae" | family =="Pinaceae") %>%
  select(DBH, family) %>%
  tbl_summary(by = family,
              missing= "no",
              digits = all_continuous() ~1,
              label = list(DBH ~ "DBH (cm)"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n}")) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% #number of digits displayed for p-values
  modify_caption("Table 1. Mean DBH (cm) of Cupressaceae and Pinaceae individuals") %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(
    update = list(
      label ~ '',
      stat_1 ~ '**Cupressaceae**', #is markdown **bold** formatting
      stat_2 ~ '**Pinaceae**',
      p.value ~ '**P-value**')) %>%
  as_gt() %>%
  gt::tab_options(heading.align = "left")

table1
                           

table2 <- diameter %>%
  select(DBH, species) %>%
  tbl_summary(by = species,
              missing= "no",
              digits = all_continuous() ~1,
              label = list(DBH ~ "DBH (cm)"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n}")) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% #number of digits displayed for p-values
  modify_caption("Table 2. Mean DBH (cm) of all species present.") %>%
  modify_header(
    update = list(
      label ~ '',
      stat_1 ~ '***Abies balsamea***', #is markdown ***bold & italics*** formatting
      stat_2 ~ '***Acer rubrum***',
      stat_3 ~ '***Acer saccharum***',
      stat_4 ~ "***Picea glauca***",
      stat_5 ~ "***Populus temuloides***",
      stat_6 ~ '***Thuja occidentalis***',
      p.value ~ '**P-value**')) %>%
  as_gt() %>%
  gt::tab_options(heading.align = "left")

table2












test_table <- diameter %>%
  filter(family =="Cupressaceae" | family =="Pinaceae") %>%
  select(DBH, family) %>%
  tbl_summary(by = family,
              missing= "no",
              digits = all_continuous() ~1,
              label = list(DBH ~ "DBH (cm)"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n}")) %>%
  add_p(pvalue_fun = ~ style_pvalue(.x, digits = 3)) %>% #number of digits displayed for p-values
  add_p = t.test(DBH ~ as.factor(by), data = DBH, conf.level = 0.95) %>%
  modify_caption("Table 1. Mean DBH of Cupressaceae and Pinaceae individuals") %>%
  modify_footnote(everything() ~ NA) %>%
  modify_header(
    update = list(
      label ~ '',
      stat_1 ~ '**Cupressaceae**', #is markdown **bold** formatting
      stat_2 ~ '**Pinaceae**',
      p.value ~ '**P-value**'))

test_table
