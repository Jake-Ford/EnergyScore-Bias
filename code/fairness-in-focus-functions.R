library(rmdformats)
library(ggplot2)
library(dplyr)
library(readxl)
library(reshape)
require(gridExtra)
library(kableExtra)
library(knitr)
library(kableExtra)
library(webshot)

library(missRanger)
library(formattable)
source("./util/helper_functions.R")

library(plotly)
options(scipen = 100)



sample_data <- read.csv('/Users/jacobford/Library/CloudStorage/GoogleDrive-jake@solstice.us/Shared drives/*Solstice | Shared Project/The EnergyScore/Google Bias Fairness Analysis/Master ES Dataset/Stata/full_data.csv')

output = read.csv("/Users/jacobford/Library/CloudStorage/GoogleDrive-jake@solstice.us/My Drive/EnergyScore/Bias/Full Dataset/Archive/EnergyScore_output_jf_test_4.5.22.csv", header=FALSE)


output$RECORD_NB <- sample_data$RECORD_NB

joined <- merge(sample_data,output, by="RECORD_NB")
drop(sample_data)
drop(output)


## There are two Race variables in the data set: https://docs.google.com/spreadsheets/d/1WpOzqD7lTUWzq3PcbAOayJQrAU3xbNpw/edit#gid=1630953880

# Note: the variable used in the white paper is axciom9528, which has a large 'other' population, looks to have been combined with general white population in the other race category, axiom3101; pros and cons to each, however to avoid simply lumping 'other' into white, and to maintain consistency with previous work, will use axciom9528 

joined <- joined %>% 
  mutate( 
    Race = case_when(
      axciom9528 == "A" | axciom9528 == "C" | axciom9528 == "N"  ~ "Asian" ,
      axciom9528 == "B" ~ "Black",
      axciom9528 == "H" ~ "Hispanic",
      axciom9528 == "W" ~ "White"),
    Race2 = case_when(
      axciom3101 =="A" ~ "Asian",
      axciom3101 == "B" ~ "Black",
      axciom3101 == "H" ~ "Hispanic",
      axciom3101 == "W" ~ "White"),
    Education = case_when(
      axciom7650_1 == 1 ~ "High School",
      axciom7650_1 == 2 ~ "College",
      axciom7650_1 == 3 ~ "Graduate School", 
      axciom7650_1 == 4 ~ "Vocational/Technical"
    ),
    EnergyScore = Prob_Not_Def*100,
    dummy=1,
    HomeOwnership = case_when(
      axciom7606_1 =="O" ~ "Own",
      axciom7606_1 == "R" ~ "Rent"
    ))


joined <- joined %>%
  mutate(Income = case_when(
    axciom7641_1 <=4 ~ "Low",
    axciom7641_1 == 5 | axciom7641_1 ==6 ~ "Medium",
    axciom7641_1 > 6 ~ "High"
  )) 

joined$Income <- factor(joined$Income, labels=c("Low", "Medium", "High"))

race_data <- joined %>%
  filter(!is.na(Race))


#Late
late_table <- joined %>%
  mutate(Late_Payment = case_when(
    DPD90_KEYCD == 1 ~ "Late",
    DPD90_KEYCD == 0 ~ "Not Late"
  )) %>%
  group_by(Variable = Late_Payment) %>%
  summarize(N=n(),
            `Average FICO` = mean(FICOCLV8_SCORE, na.rm=T),
            `Late Payment %` = NA) %>%
  mutate(Frequency = paste0(round(N/sum(N)*100, 1),"%")) %>%
  relocate(Frequency, .after = N) 

#Race
race_table <- joined %>%
  group_by(Variable = Race) %>%
  filter(!is.na(Race)) %>%
  summarize(N=n(),
            `Average FICO` = mean(FICOCLV8_SCORE, na.rm=T),
            `Late Payment %` = mean(DPD90_KEYCD, na.rm=T)) %>%
  mutate(Frequency = paste0(round(N/sum(N)*100, 1),"%")) %>%
  relocate(Frequency, .after = N) 

#Homeowner
hs_table <- joined %>%
  group_by(Variable=HomeOwnership) %>%
  filter(!is.na(HomeOwnership)) %>%
  summarize(N=n(),
            `Average FICO` = mean(FICOCLV8_SCORE, na.rm=T),
            `Late Payment %` = mean(DPD90_KEYCD, na.rm=T)) %>%
  mutate(Frequency = paste0(round(N/sum(N)*100, 1),"%")) %>%
  relocate(Frequency, .after = N)

#Income
income_table <- joined %>%
  group_by(Variable=Income) %>%
  filter(!is.na(Income)) %>%
  summarize(N=n(),
            `Average FICO` = mean(FICOCLV8_SCORE, na.rm=T),
            `Late Payment %` = mean(DPD90_KEYCD, na.rm=T)) %>%
  mutate(Frequency = paste0(round(N/sum(N)*100, 1),"%")) %>%
  relocate(Frequency, .after = N) 

# Education
edu_table <- joined %>%
  group_by(Variable=Education) %>%
  filter(!is.na(Education)) %>%
  summarize(N=n(),
            `Average FICO` = mean(FICOCLV8_SCORE, na.rm=T),
            `Late Payment %` = mean(DPD90_KEYCD, na.rm=T)) %>%
  mutate(Frequency = paste0(round(N/sum(N)*100, 1),"%")) %>%
  relocate(Frequency, .after = N)

temp <- rbind(late_table, race_table)
temp <- rbind(temp, hs_table)
temp <- rbind(temp, income_table)
temp <- rbind(temp, edu_table)







table1 <- kable( temp,
                 digits = 3, format.args = list(big.mark = ",",scientific = FALSE),
                 # list(race_table, hs_table),
                 caption = 'Descriptive Statistics') %>%
  kable_paper("striped", full_width =T) %>%
  #  booktabs = TRUE, valign = 't') 
  pack_rows("Late Payment", 1, 2) %>%
  pack_rows("Race", 3, 6) %>%
  pack_rows("Home Ownershp", 7,8) %>%
  pack_rows("Income", 9,11) %>%
  pack_rows("Education", 12,15)


########################################################

# Save the table as an HTML file
html_file <- "whitepaper/figures/table1.html"
save_kable(table1, file = html_file)

# Save the table as a PDF file
pdf_file <- "whitepaper/figures/table1.pdf"
webshot(html_file, pdf_file)