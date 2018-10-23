rm(list=ls())

library(readxl)
library(tidyverse)
source("narms_func.R")

# This is NOT plug and play. 
# These files had to have many columns removed, due to errors that led to some columns not getting read. 
# mostly due to the fact that they had commas and > = < that got read as logical statements. 
# looking through, i deleted all the columns with > = < and based cutoffs on the MIC numbers instead. 
# assumptions were that the number would be >= only if resistant, and <= only if susceptible.

haacp_full <- read_excel("NARMS_HACCP_06to15_8.2017_clean.xlsx")
retail_full <- read_excel("NARMS_Retail_Meat_8.2017_clean.xlsx")
cecal_full <- read_excel("NARMS_Cecal_13to15_10.2017_clean.xlsx")


year  <- 2015
haacp_salm_2015 <- salmonella(haacp_full, year)

retail_entc_2015 <- entc(retail_full, year)
retail_ecoli_2015 <- ecoli(retail_full, year)
retail_salm_2015 <- salmonella(retail_full, year)

cecal_entc_2015 <- entc(cecal_full, year)
cecal_ecoli_2015 <- ecoli(cecal_full, year)
cecal_salm_2015 <- salmonella(cecal_full, year)

#cleaning up
rm(haacp_full, retail_full, cecal_full)

# count the number of times resistant to 3 or more drugs comes up 

#reduce to classes instead of antibiotics
#retail_salm_2015 <- salm_ecoli_class(retail_salm_2015)
# figure out resistant percentage
#pleasework <- resistant(retail_salm_2015, 10)
#ggplot(pleasework) + geom_col(aes(x = host, y = percent)) + labs(title = "% of samples resistant to 3 or more classes", subtitle = "of Salmonella strains found on retail meat")


#combined

# num_cols for salmonella and ecoli are 10, enterococcus is 15. 

retail_ecoli <- salm_ecoli_resistant(retail_ecoli_2015, 10)
cecal_ecoli <- salm_ecoli_resistant(cecal_ecoli_2015, 10)

retail_salm <- salm_ecoli_resistant(retail_salm_2015, 10)
cecal_salm <- salm_ecoli_resistant(cecal_salm_2015, 10)
haacp_salm <- salm_ecoli_resistant(haacp_salm_2015, 10)

cecal_entc <- entc_resistant(cecal_entc_2015, 15)
retail_entc <- entc_resistant(retail_entc_2015, 15)


#cleanup

rm(cecal_ecoli_2015, cecal_salm_2015, cecal_entc_2015, 
   retail_ecoli_2015, retail_entc_2015, retail_salm_2015, 
   haacp_salm_2015)

retail_ecoli$source <- "retail"
retail_entc$source <- "retail"
retail_salm$source <- "retail"

haacp_salm$source <- "haacp"

cecal_ecoli$source <- "cecal"
cecal_entc$source <- "cecal"
cecal_salm$source <- "cecal"

cecal <- bind_rows(cecal_ecoli, cecal_entc, cecal_salm, .id=NULL)
retail <- bind_rows(retail_ecoli, retail_entc, retail_salm, .id=NULL)

salm <- bind_rows(cecal_salm, retail_salm, haacp_salm, .id=NULL)
entc <- bind_rows(cecal_entc, retail_entc, .id=NULL)
ecoli <- bind_rows(cecal_ecoli, retail_ecoli, .id=NULL)



ggplot(subset(retail, yes + no > 25)) + geom_col(aes(x=genus, fill = host, y = percent), position = position_dodge()) + 
  labs(title = "Resistant Bacteria on Retail Meat", 
       subtitle = "Percentage of Bacteria on Retail Meat Resistant to 3 or More Classes of Drugs", 
       caption = "entries with fewer than 25 total samples were removed from analysis") + scale_x_discrete(labels = c("E. coli", "Enterococcus", "Salmonella")) +  
  + theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line())
ggsave("3_or_more_resistant_retailmeat.png")

ggplot(subset(cecal, no + yes >= 25)) + geom_col(aes(x=genus, fill = host, y = percent), position = position_dodge()) + 
  labs(title = "Resistant Bacteria in Cecal Samples", 
       subtitle = "Percentage of Bacteria in Cecal Samples Resistant to 3 or More Classes of Drugs", 
       caption = "entries with fewer than 30 total samples were removed from analysis") + scale_x_discrete(labels = c("E. coli", "Enterococcus", "Salmonella")) + 
  + theme(axis.title.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line())
ggsave("3_or_more_resistant_cecal.png")
