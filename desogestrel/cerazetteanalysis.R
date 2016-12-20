
library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)

## Measure: How GP practices prescribe of branded Cerazette (BNF code 0703021Q0BB) 
## compared with prescribing of all desogestrel (BNF code 0703021Q0*).

## Definition: Total quantity of Cerazette, as a proportion of total quantity of desogestrel.

## Why it matters: This is the NHS Business Service Authority's top cost-saver from generic switching. 
##                 Cerazette and desogestrel are both exactly the same drug, the same molecule, but 
##                 Cerazette is an expensive branded package, and desogestrel is a cheap generic package.

## This shows how the desogestrel file has been subsetted from the full and combined prescribing dataset
## from HSCNI Business Services Organisation https://www.opendatani.gov.uk/dataset/gp-prescribing-data
## It is commented out but it results in the desogestrel.csv file that is read in after
## desogestrel = subset(prescribing, bnf_chapter == 7 & bnf_section == 3 & bnf_paragraph == 2 & bnf_subparagraph == 1) %>% 
##                mutate(date = ymd(paste(year, month, 1))) %>%
##                subset(date > '2013-05-01')


## read in the dataset of desogestrel
desogestrel <- read.csv("~/desogestrel.csv")

## read in our dataset of practices
practices <- read.csv("~/../unique_practices_latest.csv")

cerazette = function(field){grepl('^0703021Q0BB.*', field)}
all_desogestrel = function(field){grepl('^0703021Q0.*', field)}

analysis = desogestrel %>% 
  group_by(practice, date) %>%
  summarise(cerazette = sum(total_items[which(cerazette(bnf_code))]),    
            all_desogestrel = sum(total_items[which(all_desogestrel(bnf_code))]),
            cerazette_spend = sum(actual_cost[which(cerazette(bnf_code))]),    
            all_desogestrel_spend = sum(actual_cost[which(all_desogestrel(bnf_code))])
            ) %>%
  mutate(measure = round(cerazette/all_desogestrel * 100,3)) %>%
  arrange(date)

analysis = merge(analysis, practices[c(1:3,7)], by.x = 'practice', by.y = 'PracNo')

spending = analysis %>% 
  subset(date >= ymd("2013-07-01")) %>%
  group_by(date) %>%
  summarise(cerazette_spending = sum(cerazette_spend), 
            all_desogestrel_spending = sum(all_desogestrel_spend)) %>%
  gather(stat, "Spend", cerazette_spending:all_desogestrel_spending) %>%
  ggplot()+
  geom_line(aes(date, Spend, group=stat, colour=stat))

items = analysis %>% group_by(date) %>%
  subset(date >= ymd("2013-07-01")) %>%
  summarise(cerazette_items = sum(cerazette),
            all_desogestrel_items = sum(all_desogestrel)) %>%
  gather(stat, "Items", cerazette_items:all_desogestrel_items) %>%
  ggplot()+
  geom_col(aes(date, Items, group=stat, fill=stat), position="dodge")

percentiles = analysis %>% group_by(date) %>% 
  summarise(percentile10 = quantile(measure, 0.1, na.rm=T), percentile20 = quantile(measure, 0.2, na.rm=T), 
            percentile30 = quantile(measure, 0.3, na.rm=T), percentile40 = quantile(measure, 0.4, na.rm=T), 
            median = quantile(measure, 0.5, na.rm=T), percentile60 = quantile(measure, 0.6, na.rm=T), 
            percentile70 = quantile(measure, 0.7, na.rm=T),
            percentile80 = quantile(measure, 0.8, na.rm=T), percentile90 = quantile(measure, 0.9, na.rm=T)) %>% 
  gather(stat, "Measure", percentile10:percentile90)

percentiles_plot = percentiles %>%
  ggplot()+ 
  geom_line(aes(date, Measure, group=stat, linetype=stat), size=1, colour="red")+
  scale_linetype_manual(values=c('dotted', 'dotted', 'dotted', 'dotted', 'dashed', 'dotted', 'dotted', 'dotted', 'dotted'))


## measure of all items prescribed by the practices in each LCG
cerazette_lcg = analysis %>% 
  subset(date >= ymd("2013-07-01")) %>%
  group_by(date, LCG) %>%
  summarise(cerazette = sum(cerazette),    
            all_desogestrel = sum(all_desogestrel),
            cerazette_spend = sum(cerazette_spend),    
            all_desogestrel_spend = sum(all_desogestrel_spend)
            ) %>%
  mutate(measure = round(cerazette/all_desogestrel * 100, 3)) %>%
  mutate(measure_spend = round(cerazette_spend/all_desogestrel_spend * 100, 3)) %>%
  arrange(date) %>%
  ggplot()+
  geom_line(aes(date, measure, group=LCG, colour=LCG))

 belfast_cerazette = subset(analysis, LCG == 'Belfast') %>% select(date, practice, measure) %>% 
   ggplot(aes(date, measure, group=practice, colour="Measure"))+ 
   geom_line()+ 
   geom_line(data = percentiles, aes(date, Measure, group=stat, linetype=stat, colour="Quantiles"))+
   scale_linetype_manual(values=c('dotted', 'dotted', 'dotted', 'dotted', 'dashed', 'dotted', 'dotted', 'dotted', 'dotted'))+
   facet_wrap(~ practice, ncol = 5, scales='free_y')
 
 south_cerazette = subset(analysis, LCG == 'Southern') %>% select(date, practice, measure) %>% 
   ggplot(aes(date, measure, group=practice, colour="Measure"))+ 
   geom_line()+ 
   geom_line(data = percentiles, aes(date, Measure, group=stat, linetype=stat, colour="Quantiles"))+
   scale_linetype_manual(values=c('dotted', 'dotted', 'dotted', 'dotted', 'dashed', 'dotted', 'dotted', 'dotted', 'dotted'))+
   facet_wrap(~ practice, ncol = 5, scales='free_y')
 
 southEast_cerazette = subset(analysis, LCG == 'South Eastern') %>% select(date, practice, measure) %>% 
   ggplot(aes(date, measure, group=practice, colour="Measure"))+ 
   geom_line()+ 
   geom_line(data = percentiles, aes(date, Measure, group=stat, linetype=stat, colour="Quantiles"))+
   scale_linetype_manual(values=c('dotted', 'dotted', 'dotted', 'dotted', 'dashed', 'dotted', 'dotted', 'dotted', 'dotted'))+
   facet_wrap(~ practice, ncol = 5, scales='free_y')
 
 north_cerazette = subset(analysis, LCG == 'Northern') %>% select(date, practice, measure) %>% 
   ggplot(aes(date, measure, group=practice, colour="Measure"))+ 
   geom_line()+ 
   geom_line(data = percentiles, aes(date, Measure, group=stat, linetype=stat, colour="Quantiles"))+
   scale_linetype_manual(values=c('dotted', 'dotted', 'dotted', 'dotted', 'dashed', 'dotted', 'dotted', 'dotted', 'dotted'))+
   facet_wrap(~ practice, ncol = 5, scales='free_y')
 
 west_cerazette = subset(analysis, LCG == 'Western') %>% select(date, practice, measure) %>% 
   ggplot(aes(date, measure, group=practice, colour="Measure"))+ 
   geom_line()+ 
   geom_line(data = percentiles, aes(date, Measure, group=stat, linetype=stat, colour="Quantiles"))+
   scale_linetype_manual(values=c('dotted', 'dotted', 'dotted', 'dotted', 'dashed', 'dotted', 'dotted', 'dotted', 'dotted'))+
   facet_wrap(~ practice, ncol = 5, scales='free_y')
