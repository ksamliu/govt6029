library(readxl)
library(readr)
library(tidyverse)
library(stargazer)
library(texreg)
options(scipen=999)

setwd("/Users/samuel27/Google Drive/Cornell PhD/GOVT6857/6857_ResearchIdea")

# ties

ties <- read_excel("TIESv4.xls")

ties %>% 
  filter(sanctiontype==9) %>% 
  View()

# Magnitsky

mag <- read_excel("200113-USG-GMA-Designations.xlsx")

mag2 <- mag %>% 
  filter(Type == "Individual") %>% 
  group_by(Country) %>% 
  mutate(count=n()) %>% 
  select("Designation Date","Country","count") %>% 
  distinct(Country, .keep_all = T)
mag2[12,]$Country <- "Democratic Republic of the Congo"
mag2[14,]$Country <- "Yugoslavia"


mag2$Country



# Vdem

vdem <- read_csv("V-Dem-CY-Full+Others-v10.csv")
head(vdem)

vdem_new <- vdem %>%   filter(year==2014) %>% 
  select(country_name, country_id, year, COWcode, v2x_corr, e_p_polity, e_polity2, e_wbgi_cce, e_fh_cl, e_fh_pr, e_fh_status, e_ti_cpi, e_peaveduc, e_regiongeo, e_cow_exports, e_cow_imports, e_migdpgro, e_migdppc, e_migdppcln, e_total_resources_income_pc, e_wb_pop)
vdem_new[8,]$country_name <- "Myanmar"
vdem_new[170,]$country_name <- "Yugoslavia"
vdem_new[111,]$country_name <- "Gambia"
vdem_new[68,]$country_name <- "East Timor"
vdem_new[158,]$country_name <- "Macedonia"
vdem_new[106,]$country_name <- "Congo"
vdem_new[126,]$country_name <- "Swaziland"
vdem_new$country_name


cowdyadic <- read_csv("Dyadic_COW_4.0.csv")
cowdyadic %>% select(importer2) %>% distinct(importer2) %>% View()

cow_usa <- cowdyadic %>% 
  filter(ccode1==2)  %>% 
  select(ccode1, ccode2, year, importer1, importer2, flow1, flow2, smoothtotrade) %>% 
  filter(year>2013)

cow_usa %>% filter(importer2 == "Gambia") %>% View()

#



merge1 <- right_join(mag2, cow_usa, by=c("Country"="importer2"))

merge2 <- right_join(vdem_new, merge1, by=c("COWcode"="ccode2"))

merge2$count[is.na(merge2$count)] <- 0

# 

summary(lm(data = merge2, 
   count~flow2))

model1 <- glm(count ~ flow2 + e_p_polity, family="poisson", data=merge2)
merge2$e_p_polity
screenreg(model1)

merge2$imposed<- 0
merge2$imposed[merge2$count>0]  <- 1

model2 <- glm(imposed ~ smoothtotrade + e_p_polity, family="binomial", data=merge2 )
screenreg(model2)
summary(model2)

merge2 %>% 
  filter(flow2<20000) %>% 
  ggplot(aes(y=flow2, x=factor(imposed))) + geom_boxplot()

merge2$imposed

tempdata <- merge2 %>% select(-c(country_name,country_id,year.x,year.y,ccode1))

write.csv(tempdata, file = "samuel_data.csv")
