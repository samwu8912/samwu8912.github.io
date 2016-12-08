require(ggplot2)
require(dplyr)
require(tidyr)
source('personalTheme.R')

data <- read.csv('anes_timeseries_cdf/anes_timeseries_cdf_rawdata.csv')

# rename ethnicity codes into actual ethnicities, used for labelling facets
ethnicity <- c('650'='Pakistani','660'='Afghan','670'='Indian','680'='Southeast Asia',
               '690'='Chinese','700'='Japanese','710'='Korean','720'='Asian',
               '730'='Egyptian','740'='Iranian,Persian','750'='Iraqi','760'='Israeli',
               '770'='Jordanian','780'='Lebanese','790'='Arab','800'='Syrian',
               '810'='Turkish','820'='Armenian','840'='South Pacific Islander',
               '864'='Asian American,-Japanese American','862'='Black','877'='American', 
               '10'='Native American','610'='Portuguese','830'='African',
               '850'='Australian,New Zealander,Tasmanian','874'='Other, Multiracial')

aa_7_data <- filter(data, VCF0105a == 3 |VCF0109 %in% c(650,660,670,680,690,700,710,720,
                                                        730,740,750,760,770,780,790,800,
                                                        810,820,840,864))
asia_7_data <- filter(aa_7_data, VCF0109 %in% c(650,660,670,680,690,700,710,720))
near_east_7_data <- filter(aa_7_data, VCF0109 %in% c(730,740,750,760,770,780,790,
                                                          800,810,820))
oceania_7_data <- filter(aa_7_data, VCF0109 %in% c(840))
other_7_data <- filter(aa_7_data, !VCF0109 %in% c(650,660,670,680,690,700,710,720,
                                                       730,740,750,760,770,780,790,800,
                                                       810,820,840))

# to get the total sample sizes for each year & ethnicity
aa_7_year_breakdown <- count(aa_7_data, VCF0004, VCF0109)
aa_7_year_breakdown$VCF0109 <- factor(aa_7_year_breakdown$VCF0109)

sum_aa_7_vnr <- count(aa_7_data, VCF0004, VCF0109, VCF0703)
sum_aa_7_vnr$VCF0109 <- factor(sum_aa_7_vnr$VCF0109)
sum_aa_7_vnr$VCF0703 <- factor(sum_aa_7_vnr$VCF0703)
# code NAs as 0s so they're properly counted
sum_aa_7_vnr$VCF0703[is.na(sum_aa_7_vnr$VCF0703)] <- 0

# testing having the data broken by regions (as defined by ANES)
sum_asia_7_vnr <- count(asia_7_data, VCF0004, VCF0109, VCF0703)
sum_asia_7_vnr$VCF0109 <- factor(sum_asia_7_vnr$VCF0109)
sum_asia_7_vnr$VCF0703 <- factor(sum_asia_7_vnr$VCF0703)
sum_near_east_7_vnr <- count(near_east_7_data, VCF0004, VCF0109, VCF0703)
sum_near_east_7_vnr$VCF0109 <- factor(sum_near_east_7_vnr$VCF0109)
sum_near_east_7_vnr$VCF0703 <- factor(sum_near_east_7_vnr$VCF0703)
sum_oceania_7_vnr <- count(oceania_7_data, VCF0004, VCF0109, VCF0703)
sum_oceania_7_vnr$VCF0109 <- factor(sum_oceania_7_vnr$VCF0109)
sum_oceania_7_vnr$VCF0703 <- factor(sum_oceania_7_vnr$VCF0703)
sum_other_7_vnr <- count(other_7_data, VCF0004, VCF0109, VCF0703)
sum_other_7_vnr$VCF0109 <- factor(sum_other_7_vnr$VCF0109)
sum_other_7_vnr$VCF0703 <- factor(sum_other_7_vnr$VCF0703)

# testing having the total sample size for the ethnicity
aa_7_vnr <- spread(sum_aa_7_vnr, VCF0703, n)
aa_7_vnr <- left_join(aa_7_vnr, aa_7_year_breakdown, by = c('VCF0109', 'VCF0004'))
aa_7_vnr <- gather(aa_7_vnr, VCF0703, n, 3:7)

# moving forward with this as this dives into whether the community was:
# not registered (1), registered but did not vote (2), registered & voted (3) 
# in the various elections. it is, clearly, dependent on the sample collected
#
# TODO cleaning up would involve grouping the various ethnicities since the ids
# mostly cover that but could still use some further organizing
#
# DONE consider removing Israeli, not currently treated as members of the AA community
#
# DONE add in total sample size for each ethnicity, does not add much of anything
# to test, create following chart with `aa_7_vnr`
aa_7_vnr_chart <- ggplot(filter(sum_aa_7_vnr, !is.na(VCF0109)#, !VCF0109 %in% 
                                  #c(760#,610,850,830,862,10)
                                ), 
                         aes(x = VCF0004, y = n, colour = VCF0703, group = VCF0703)) +
  geom_point() +
  geom_path() +
  facet_wrap(~ VCF0109, labeller=as_labeller(ethnicity)) +
  personalTheme
print(aa_7_vnr_chart)

# testing breaking the chart out by region
asia_7_vnr_chart <- ggplot(sum_asia_7_vnr, aes(x = VCF0004, y = n, colour = VCF0703)) +
  geom_path() +
  facet_wrap(~ VCF0109, labeller=as_labeller(ethnicity)) +
  personalTheme
# print(asia_7_vnr_chart)

near_east_7_vnr_chart <- ggplot(sum_near_east_7_vnr, aes(x = VCF0004, y = n, 
                                                         colour = VCF0703)) +
  geom_path() +
  facet_wrap(~ VCF0109, labeller=as_labeller(ethnicity)) +
  personalTheme
# print(near_east_7_vnr_chart)

oceania_7_vnr_chart <- ggplot(sum_oceania_7_vnr, aes(x = VCF0004, y = n, colour = VCF0703)) +
  geom_path() +
  facet_wrap(~ VCF0109, labeller=as_labeller(ethnicity)) +
  personalTheme
# print(oceania_7_vnr_chart)

other_7_vnr_chart <- ggplot(sum_other_7_vnr, aes(x = VCF0004, y = n, colour = VCF0703)) +
  geom_path() +
  facet_wrap(~ VCF0109, labeller=as_labeller(ethnicity)
             ) +
  personalTheme
# print(other_7_vnr_chart)

# ---------------------------------------------------------

# 0 = na, 1 = democrat, 2 = republican, 3 = major 3rd party candidate, 4 = other
# 7 = did not vote (or just not for pres)
sum_aa_7_vote <- count(aa_7_data, VCF0004, VCF0109, VCF0706)
sum_aa_7_vote$VCF0109 <- factor(sum_aa_7_vote$VCF0109)
sum_aa_7_vote$VCF0706 <- factor(sum_aa_7_vote$VCF0706)
# code NAs as 0s so they're properly counted
sum_aa_7_vote$VCF0706[is.na(sum_aa_7_vote$VCF0706)] <- 0

# TODO consider removing NA for which party got the vote
aa_7_vote_chart <- ggplot(filter(sum_aa_7_vote, !is.na(VCF0109), VCF0706!=0
                                 #,VCF0109!=760
                                 ), 
                          aes(x = VCF0004, y = n, colour = VCF0706)) +
  geom_point() +
  geom_path() +
  scale_colour_manual(values = c('0'='gold','1'='blue','2'='red','3'='purple','4'='magenta',
                      '7'='aquamarine4')) +
  facet_wrap(~ VCF0109, labeller=as_labeller(ethnicity)) +
  personalTheme
print(aa_7_vote_chart)

# ---------------------------------------------------------

# practically all values were NA
sum_aa_7_party <- count(aa_7_data, VCF0004, VCF0109, VCF0738a)
sum_aa_7_party$VCF0109 <- factor(sum_aa_7_party$VCF0109)
sum_aa_7_party$VCF0738a <- factor(sum_aa_7_party$VCF0738a)

# ---------------------------------------------------------

sum_aa_7_engagement <- count(aa_7_data, VCF0004, VCF0109, VCF0723)
sum_aa_7_engagement$VCF0109 <- factor(sum_aa_7_engagement$VCF0109)
sum_aa_7_engagement$VCF0723 <- factor(sum_aa_7_engagement$VCF0723)
sum_aa_7_vote$VCF0723[is.na(sum_aa_7_vote$VCF0723)] <- 0

aa_7_engagement_chart <- ggplot(filter(sum_aa_7_engagement, !is.na(VCF0109), VCF0109!=760), 
                                aes(x = VCF0004, y = n, fill = VCF0723)) +
  geom_bar(stat = 'identity') +
  scale_fill_brewer(type = 'seq', palette = 'Blues') +
  facet_wrap(~ VCF0109, labeller=as_labeller(ethnicity)) +
  personalTheme
print(aa_7_engagement_chart)