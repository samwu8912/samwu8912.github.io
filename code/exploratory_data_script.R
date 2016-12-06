require(ggplot2)
require(dplyr)
require(tidyr)

anes_data <- read.csv('anes_timeseries_cdf/anes_timeseries_cdf_rawdata.csv')
# print(head(anes_data))

# 650 = Pakistani; 660 = Afghan; 670 = Indian (not American Indian); 
# 680 = Southeast Asia (from Indochina, Thailand, Malaya, Burma, Philippines, Indonesia);
# 690 = Chinese; 700 = Japanese/Japanese American; 710 = Korean; 720 = Asian;
# 730 = Egyptian; 740 = Iranian, Persian; 750 = Iraqi; 760 = Israeli; 770 = Jordanian;
# 780 = Lebanese; 790 = Arab, Arabian, Saudi Arabian; 800 = Syrian; 810 = Turk, Turkish
# 820 = Armenian; 840 = South Pacific Islander other than 850 (incl native Hawaiian);
# 864 = Asian-American (exc specifically Japanese American)
aa_anes_data <- filter(anes_data, VCF0109 %in% c(650,660,670,680,690,700,710,720,730,740,
                                                 750,760,770,780,790,800,810,820,840,864))
# print(head(aa_anes_data))

sum_aa_7_anes_data <- filter(anes_data, VCF0105a == 3)
# print(head(sum_aa_7_anes_data))

# 10 = American Indian or Native American, tribal mentions; 
# 610 = Portuguese; 830 = African, from any African country excluding only Egypt
# (U.A.R.), South African; 850 = Australian, New Zealander, Tasmanian
# 862 = Black, Negro, American Black, African American;
# 874 = Other group, combinations not codeable above; 
# 877 = 'American', 'Just American', none, neither (response to 
# 'choice' question), NA
aa_7_anes_data <- filter(anes_data, VCF0105a == 3 |VCF0109 %in% c(650,660,670,680,690,
                                                                  700,710,720,730,740,
                                                                  750,760,770,780,790,
                                                                  800,810,820,840,864))

## just using specific ethnicities excludes multiracial respondents
## this only covers 16 years of the study, starting with 1972
## there are 21 unique ethnicities included in this set
aa_year_breakdown <- count(aa_anes_data, VCF0004, VCF0109)
# print(aa_year_breakdown)

## summary field allows for respondents who don't have specific ethnicities coded
## so this covers 22 years of the study, starting with 1966
## there are 18 unique ethnicities included in this set (not all typically 'Asian')
sum_aa_7_year_breakdown <- count(sum_aa_7_anes_data, VCF0004, VCF0109)
# print(sum_aa_7_year_breakdown)

## combining both options above increases total respondents & keeps breadth of years
## continues to cover 22 years of the study, starting with 1966
## there are 27 unique ethnicities included in this set (not all typically 'Asian')
## for the widest net, probably best to move forward with this one
aa_7_year_breakdown <- count(aa_7_anes_data, VCF0004, VCF0109)
aa_7_year_breakdown$VCF0109 <- factor(aa_7_year_breakdown$VCF0109)
# print(aa_7_year_breakdown)

sum_aa_7_register <- count(aa_7_anes_data,VCF0004, VCF0109, VCF0703)
sum_aa_7_register$VCF0109 <- factor(sum_aa_7_register$VCF0109)
sum_aa_7_register$VCF0703 <- factor(sum_aa_7_register$VCF0703)
# print(sum_aa_7_register)

## look at ethnicity breakdown over the years
## black line = NA, 2012 is only NA for specific ethnicities
aa_7_breakdown_chart <- ggplot(aa_7_year_breakdown, aes(x = VCF0004, y = n, 
                                                        colour = VCF0109)) +
  geom_path()
print(aa_7_breakdown_chart)

## in reality, 0 and NA (black bars) are the same
## 1 = Not registered/didn't vote; 2 = Yes registered/didn't vote;
## 3 = Yes registered/voted
## might want to dig into 670, 680, 760 in particular
## alternative, dig into 690, 700, 710, 720, 780
aa_7_register_chart <- ggplot(sum_aa_7_register, aes(x = VCF0004, y = n, fill = VCF0703)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ VCF0109)
print(aa_7_register_chart)

cps_data <- read.csv('cps_nov14.csv')
# print(head(cps_data))

aa_cps_data <- filter(cps_data, PRDASIAN > -1)
# print(head(aa_cps_data))

## top 3 ethnicities: Asian Indian, Chinese, Filipino
## 1 = Asian Indian; 2 = Chinese; 3 = Filipino; 4 = Japanese; 
## 5 = Korean; 6 = Vietnamese; 7 = Other
aa_ethnicity_breakdown <- count(aa_cps_data, PRDASIAN)
# print(aa_ethnicity_breakdown)

## 1 = Yes; 2 = No; -1 = Not in Universe
## -2 = Don't Know; -3 = Refused; -9 = No Response
aa_vote_ethnicity <- count(aa_cps_data, PRDASIAN, PES1)
aa_vote_ethnicity$PRDASIAN <- factor(aa_vote_ethnicity$PRDASIAN)
aa_vote_ethnicity$PES1 <- factor(aa_vote_ethnicity$PES1)
# print(aa_vote_ethnicity)


aa_register_ethnicity <- count(aa_cps_data, PRDASIAN, PES2)
aa_register_ethnicity$PRDASIAN <- factor(aa_register_ethnicity$PRDASIAN)
aa_register_ethnicity$PES2 <- factor(aa_register_ethnicity$PES2)
# print(aa_register_ethnicity)

## breakdown of the voting responses about 2014 election
## voting responses by ethnicity
## lots of "Not in Universe" responses
aa_vote_ethnicity_chart <- ggplot(aa_vote_ethnicity, aes(x = PES1, y = n, fill = PRDASIAN)) +
  geom_bar(stat = 'identity', position = 'dodge')
print(aa_vote_ethnicity_chart)

## breakdown of the voting responses about 2014 election
## ethnicity by voting responses
## most ethnicities have "Not in Universe" as top response
## Japanese only one have Yes as top response
aa_ethnicity_vote_chart <- ggplot(aa_vote_ethnicity, aes(x = PRDASIAN, y = n, fill = PES1)) +
  geom_bar(stat = 'identity', position = 'dodge')
print(aa_ethnicity_vote_chart)

## breakdown of the voting responses about 2014 election
## registration responses by ethnicity
## lots of "Not in Universe" responses
aa_register_ethnicity_chart <- ggplot(aa_register_ethnicity, aes(x = PES2, y = n, fill = PRDASIAN)) +
  geom_bar(stat = 'identity', position = 'dodge')
print(aa_register_ethnicity_chart)

## breakdown of the voting responses about 2014 election
## registration responses by ethnicity
## no ethnicity has a "Yes" response as the top one, just about all are "Not in Universe"
## 2nd top response across the board is "No"
aa_ethnicity_register_chart <- ggplot(aa_register_ethnicity, aes(x = PRDASIAN, y = n, fill = PES2)) +
  geom_bar(stat = 'identity', position = 'dodge')
print(aa_ethnicity_register_chart)