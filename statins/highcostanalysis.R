library(tidyr)
library(lubridate)
library(ggplot2)
library(dplyr)

## Measure: How practices prescribe expensive instances of statins as compared to low-cost alternatives

## Definition: Total quantity of a sub-set of statins with the highest costs, as a proportion of 
##             total quantity of all statins.

## Why it matters: When treating high cholesterol, doctors are recommended to prescribe statins which are both low-cost, 
##                 and reduce cholesterol by at least 40%. The low-cost statins (simvastatin and atorvastatin) 
##                 are suitable for the majority of patients.

## Example from OpenPrescribing, on which this analysis is based https://openprescribing.net/ccg/00Q/statins/
## http://www.evidence.nhs.uk/formulary/bnf/current/2-cardiovascular-system/212-lipid-regulating-drugs/statins
## https://www.nice.org.uk/guidance/cg181/evidence/lipid-modification-update-full-guideline-243786637#page=194

## This shows how the statins file has been subsetted from the full prescribing dataset
## from HSCNI Business Services Organisation https://www.opendatani.gov.uk/dataset/gp-prescribing-data
## It is commented out but it results in the statins.csv file that is read in 
## statins = subset(prescribing, bnf_chapter == 2 & bnf_section == 12) %>%   ## subset Lipid-Regulating Drugs (BNF Chapter 2.12)
##   subset(grepl('statin', vmp_nm)) %>%                                     ## identify the statins (those containing the generic name 'statin')
##   mutate(date = ymd(paste(year, month, 1))) %>%
##   subset(date > '2013-05-01')
## statins$amp_nm[statins$amp_nm=="NULL"] <- NA                              ## simple data cleaning - some 'NULL' values, change these to NA

## read in the dataset of statins
statins <- read.csv("~/statins.csv")

## read in our dataset of practices
practices <- read.csv("~/../unique_practices_latest.csv")

## We now have to work out which are the most costly statins, based on their *per quantity* cost (i.e. number of actual tablets).
## The 'high-cost' variants need to be hand-picked out of this analysis. This is just to show how they were calculated.
statins_cost_analysis = statins %>%
  subset(date >= '2016-01-01') %>%
  group_by(bnf_code, bnf_name) %>%
  summarise(total_cost = sum(actual_cost),
            total_quantity = sum(total_quantity)
    ) %>%
  mutate(cost_per_dosage = round(total_cost/total_quantity, 2))

## We have determined the high-cost statins. They are:
##      *Rosuvastatin/Crestor
##      *Inegy
##      *Lipitor
##      *Lescol
##      *Simvastatin + Ezetimibe
##      *Atorvastatin chewable tablets & 30mg/60mg tablets
##      *Fluvastatin 80mg modified-release tablets
##      *Luvinsta
##      *Lipostat
##      *Simvastatin oral suspension
##      *Zocor

## Their BNF code regular expression patterns begin with 0212000 and the following:
high_cost_codes <- c('AA','ACBB','B0BB','M0BB','ACAAA','B0AAAALAL','B0AAAMAM','B0AAAANAN','B0AAADAD','M0AAADAD','M0BC','X0BB','Y0AAAKAK','Y0AAALAL','Y0BB')

## Low-cost: all other presentations of Atorvastatin, Fluvastatin, Simvastatin, Pravastatin.

high_cost = function(field){grepl(paste('^0212000',high_cost_codes,'.*', sep="", collapse='|'), field)}
all_statins = function(field){grepl('^0212.*', field)}

statin_analysis = statins %>% 
  group_by(practice, date) %>%
  summarise(high_cost = sum(total_items[which(high_cost(bnf_code))]),    
            all_statins = sum(total_items),
            high_cost_spend = sum(actual_cost[which(high_cost(bnf_code))]),    
            all_statins_spend = sum(actual_cost)
  ) %>%
  mutate(measure = round(high_cost/all_statins * 100,3)) %>%
  arrange(date)

statin_analysis = merge(statin_analysis, practices[c(1:3,7)], by.x = 'practice', by.y = 'PracNo')

spending = statin_analysis %>% 
  subset(date >= ymd("2013-07-01")) %>%
  group_by(date) %>%
  summarise(high_cost_spending = sum(high_cost_spend), 
            all_statins_spending = sum(all_statins_spend)) %>%
  gather(stat, "Spend", high_cost_spending:all_statins_spending) %>%
  ggplot()+
  geom_line(aes(date, Spend, group=stat, colour=stat))

items = statin_analysis %>% group_by(date) %>%
  subset(date >= ymd("2013-07-01")) %>%
  summarise(high_cost_items = sum(high_cost),
            all_statins_items = sum(all_statins)) %>%
  gather(stat, "Items", high_cost_items:all_statins_items) %>%
  ggplot()+
  geom_col(aes(date, Items, group=stat, fill=stat), position="dodge")

percentiles = statin_analysis %>% group_by(date) %>% 
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
statins_lcg = statin_analysis %>% 
  subset(date >= ymd("2013-07-01")) %>%
  group_by(date, LCG) %>%
  summarise(high_cost = sum(high_cost),    
            all_statins = sum(all_statins),
            high_cost_spend = sum(high_cost_spend),    
            all_statins_spend = sum(all_statins_spend)
  ) %>%
  mutate(measure = round(high_cost/all_statins * 100, 3)) %>%
  mutate(measure_spend = round(high_cost_spend/all_statins_spend * 100, 3)) %>%
  arrange(date) %>%
  ggplot()+
  geom_line(aes(date, measure, group=LCG, colour=LCG))

belfast_statins = subset(analysis, LCG == 'Belfast') %>% select(date, practice, measure) %>%
  ggplot(aes(date, measure, group=practice, colour="Measure"))+
  geom_line()+
  geom_line(data = percentiles, aes(date, Measure, group=stat, linetype=stat, colour="Quantiles"))+
  scale_linetype_manual(values=c('dotted', 'dotted', 'dotted', 'dotted', 'dashed', 'dotted', 'dotted', 'dotted', 'dotted'))+
  facet_wrap(~ practice, ncol = 5, scales='free_y')

south_statins = subset(analysis, LCG == 'Southern') %>% select(date, practice, measure) %>%
  ggplot(aes(date, measure, group=practice, colour="Measure"))+
  geom_line()+
  geom_line(data = percentiles, aes(date, Measure, group=stat, linetype=stat, colour="Quantiles"))+
  scale_linetype_manual(values=c('dotted', 'dotted', 'dotted', 'dotted', 'dashed', 'dotted', 'dotted', 'dotted', 'dotted'))+
  facet_wrap(~ practice, ncol = 5, scales='free_y')

southEast_statins = subset(analysis, LCG == 'South Eastern') %>% select(date, practice, measure) %>%
  ggplot(aes(date, measure, group=practice, colour="Measure"))+
  geom_line()+
  geom_line(data = percentiles, aes(date, Measure, group=stat, linetype=stat, colour="Quantiles"))+
  scale_linetype_manual(values=c('dotted', 'dotted', 'dotted', 'dotted', 'dashed', 'dotted', 'dotted', 'dotted', 'dotted'))+
  facet_wrap(~ practice, ncol = 5, scales='free_y')

north_statins = subset(analysis, LCG == 'Northern') %>% select(date, practice, measure) %>%
  ggplot(aes(date, measure, group=practice, colour="Measure"))+
  geom_line()+
  geom_line(data = percentiles, aes(date, Measure, group=stat, linetype=stat, colour="Quantiles"))+
  scale_linetype_manual(values=c('dotted', 'dotted', 'dotted', 'dotted', 'dashed', 'dotted', 'dotted', 'dotted', 'dotted'))+
  facet_wrap(~ practice, ncol = 5, scales='free_y')

west_statins = subset(analysis, LCG == 'Western') %>% select(date, practice, measure) %>%
  ggplot(aes(date, measure, group=practice, colour="Measure"))+
  geom_line()+
  geom_line(data = percentiles, aes(date, Measure, group=stat, linetype=stat, colour="Quantiles"))+
  scale_linetype_manual(values=c('dotted', 'dotted', 'dotted', 'dotted', 'dashed', 'dotted', 'dotted', 'dotted', 'dotted'))+
  facet_wrap(~ practice, ncol = 5, scales='free_y')