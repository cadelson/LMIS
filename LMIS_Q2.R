---
title: "ARV_Optimization_LMIS"
output: html_document
---

install.packages("lubridate")
install.packages("writexl")
install.packages("readxl")
install.packages("tidyverse")
install.packages("devtools")
install.packages("skimr")
install.packages("scales")
install.packages("extrafont")
install.packages("patchwork")
install.packages("extrafont")
install.packages("collapse")

##packages
library("lubridate")
library("tidyverse")
library("devtools")
library("skimr")
library("scales")
library("extrafont")
library("patchwork")
library("glamr")
library("writexl")
library("readxl")
library("collapse")


##Above but for April Update, added products

sc_fact_april <- read_excel("2020-04_SC-FACT_Data_updated.xlsx")
glimpse(sc_fact_april)
ARV_products <- read_excel("lmis_ARV_products_adult.xlsx")
view(ARV_products)

april_data <- merge(sc_fact_april, ARV_products, by="Product")
glimpse(april_data)
view(april_data)

##Create MOT

april_data %>%
  mutate(mot_ami = AMI*mot_adult) %>%
  mutate(mot_soh = SOH*mot_adult) %>%
  glimpse()
  
april_data %>%
  mutate(mot_ami = AMI*mot_adult) %>%
  mutate(mot_soh = SOH*mot_adult) %>%
  mutate(mot_soh=if_else(SOH/pill count)*mot, Country %in% c("Zimbabwe", "Haiti"), mot_soh) %>%
  mutate(mot_ami=if_else(AMI/pill count)*mot, Country %in% c("Zimbabwe", "Haiti"), mot_ami) %>%
  glimpse(april_data)
  write_xlsx(april_data,"april_data_merged.xlsx")

april_data_tmp <- april_data %>% 
  mutate(mot_ami = AMI * mot_adult, mot_soh = SOH * mot_adult) %>% 
  mutate(mot_ami = case_when(Country %in% c("Zimbabwe", "Haiti") ~ (AMI / `pill count`), TRUE ~ mot_ami),mot_soh = case_when(Country %in% c("Zimbabwe", "Haiti") ~ (SOH / `pill count`),TRUE ~ mot_soh))
  glimpse(april_data_tmp)
  write_xlsx(april_data_tmp,"april_data_merged1.xlsx")
  
april_data_tmp <- april_data %>% 
  mutate(mot_ami = AMI * mot_adult, mot_soh = SOH * mot_adult) %>% 
  mutate(mot_ami = case_when(Country %in% c("Zimbabwe", "Haiti") ~ ((AMI / `pill count`)*mot_adult), TRUE ~ mot_ami),mot_soh = case_when(Country %in% c("Zimbabwe", "Haiti") ~ ((SOH / `pill count`)*mot_adult),TRUE ~ mot_soh))
  glimpse(april_data_tmp)
  write_xlsx(april_data_tmp,"april_data_merged_updated.xlsx")
  
##Save new data frame as Excel document
write_xlsx(april_data,"april_data_merged.xlsx")



##Collapse Josh's test dataset
data_merged <- read_excel("merge_test.xlsx")
view(data_merged)
collapsed_data <- collap(data_merged, TX_CURR ~ regimen_type)
view(collapsed_data)
  TX_CURR ~ regimen_type, data=hsb2)
?collapse


##Combine the MER and LMIS datasets
MER_March <- read_excel("mer_q2_wide_sc_xwalk.xlsx")
LMIS_March <- read_excel("sc_fact_2020.03.wide.xlsx")

triangulated_data <- merge(MER_March, LMIS_March, by="facility")

glimpse(triangulated_data)
view(triangulated_data)
triangulated_data %>%
  rename(
    country_MER = country.x,
    country_LMIS = country.y,
    ) %>%
  view(triangulated_data)

view(triangulated_data)



#ggplot

ggplot(q2_data, aes(x=Period, y=AMI))+
  geom_smooth(data = filter(q2_data, name == "tld_150_30""))+
  facet_wrap(~Country, nrow=5)
  
ggplot(q2_data, aes(x=Period, y=AMI))+
  geom_smooth(data = filter(q2_data, country == "Botswana""), SE=FALSE)+
  facet_wrap(~Country, nrow=5)