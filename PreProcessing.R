
#Libraries
library(ggpubr)
library(rstatix)
library(tidyverse)
library(rstatix)
library(readxl)

load(file = "aida.Rdata")

#Verifying missing records
na_count <-sapply(aida, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

#Rename some variables
aida <- aida %>% 
  rename ('Region' = 'Registered office address - Region',
          'Last year' = 'Last accounting closing date',  
          'Bank turnover' = 'Banks/turnover%Last avail. yr',
          'Cash flowth' = 'Cash Flowth EURLast avail. yr',
          'Cost debit' = 'Cost of debit (%)%Last avail. yr',
          'Current liabilities' = 'Current liabilities/Tot ass.%Last avail. yr',
          'Current ratio' = 'Current ratioLast avail. yr',
          'Debt/EBITDA' = 'Debt/EBITDA ratio%Last avail. yr', 
          'Debt/equity' = 'Debt/equity ratio%Last avail. yr',
          'EBITDA Vendite' = 'EBITDA/Vendite%Last avail. yr',
          'EBITDAth' = 'EBITDAth EURLast avail. yr',
          'Leverage' = 'LeverageLast avail. yr' , 
          'Liquidity ratio' = 'Liquidity ratioLast avail. yr' ,
          'NET' = 'Net financial positionth EURLast avail. yr',
          'Capital' = 'Net working capitalth EURLast avail. yr',
          'Number of employees' = 'Number of employeesLast avail. yr',
          'Profit loss' = 'Profit (loss)th EURLast avail. yr',
          'ROA' = 'Return on asset (ROA)%Last avail. yr',
          'ROI' = 'Return on investment (ROI) (%)%Last avail. yr',
          'ROS' = 'Return on sales (ROS)%Last avail. yr',
          'ROE' = 'Return on equity (ROE)%Last avail. yr',
          'Total assets turnover' = 'Total assets turnover (times)Last avail. yr',	
          'Total assets' = 'Total assetsth EURLast avail. yr',
          'Solvency ratio' = 'Solvency ratio (%)%Last avail. yr'
  )

#Rename some values
aida$Region <- as.character(aida$Region)
aida$Region = gsub("Valle d'Aosta/Vallée d'Aoste","Valle d'Aosta",aida$Region)
aida$Region = gsub("Trentino-Alto Adige","Trentino",aida$Region)
aida$Region = gsub("Friuli-Venezia Giulia","Friuli",aida$Region)


#Segmenting companies between failed and active or successfull
aida <- aida %>% 
  add_column(
    `Status` = if_else(
      .$`Legal status` %in% c("Active","Active (default of payments)","Active (receivership)"), "Active", "Failed" ))

aida$Status <- as.factor(aida$Status)
addmargins (table(aida$Status)*100/nrow(aida))

#Company Size calculated according to the European Union division
aida <- aida %>% 
  add_column(
    Size = case_when(
      .$`Number of employees` > 250 | .$`Total assets` > 43000000   ~ "Large",
      .$`Number of employees` > 50 |  .$`Total assets` > 10000000  ~ "Medium",
      .$`Number of employees` > 10 |  .$`Total assets` > 200000   ~ "Small",
      TRUE ~ "Micro"
    ))
#The variable Size is created has string value so we change to factor and we put in order the values micro, small, medium, large
aida$Size <- factor(aida$Size,
                    levels = c("Micro","Small","Medium","Large"))
aida %>% 
  select(Size) %>% 
  count(Size) %>% 
  arrange(desc(n)) %>% 
  View()

#Calculating age as the difference of 'Last accounting closing date' and 'Incorporation year'
aida$'Company_age' = aida$`Last year` - aida$`Incorporation year`
aida %>% 
  select(Company_age) %>% 
  count(Company_age) %>% 
  arrange(desc(n)) %>% 
  View()

# Eliminating records with errors (negative ages)
aida=aida[!(aida$Company_age < 0), ]

#Uniting smaller Legal groups into Other and also aggregation the NA values 
aida <-aida %>% 
  mutate(`Legal form` = replace_na(`Legal form`,"Other")) %>% 
  mutate(`Legal form` = replace(`Legal form`, 
                                `Legal form` %in% c("Association",
                                  "Foundation",
                                  "Public agency",
                                  "S.C.A.R.I.",
                                  "S.A.P.A.",
                                  "Mutual aid society",
                                  "Foreign company"), "Other"))

aida %>% 
  select(`Legal form`) %>% 
  count(`Legal form`) %>% 
  arrange(desc(n)) %>% 
  View()

#Load excel document with the structure of the ATECO sectors
ateco <- read_excel("Struttura-ATECO-2007.xlsx")

#Take the title of each sector
ATECO_SECTOR <- ateco %>% 
  select(`Codice Ateco 2007`,`Titolo Ateco 2007`) %>% 
  filter(`Codice Ateco 2007`=="A" | 
           `Codice Ateco 2007`=="B" |
           `Codice Ateco 2007`=="C" |
           `Codice Ateco 2007`=="D" |
           `Codice Ateco 2007`=="E" |
           `Codice Ateco 2007`=="E" |
           `Codice Ateco 2007`=="F" |
           `Codice Ateco 2007`=="G" |
           `Codice Ateco 2007`=="H" |
           `Codice Ateco 2007`=="I" |
           `Codice Ateco 2007`=="J" |
           `Codice Ateco 2007`=="K" |
           `Codice Ateco 2007`=="L" |
           `Codice Ateco 2007`=="M" |
           `Codice Ateco 2007`=="N" |
           `Codice Ateco 2007`=="O" |
           `Codice Ateco 2007`=="P" |
           `Codice Ateco 2007`=="Q" |
           `Codice Ateco 2007`=="R" |
           `Codice Ateco 2007`=="S" |
           `Codice Ateco 2007`=="T" |
           `Codice Ateco 2007`=="U" ) 

#Taking the first two characters to identify the section
aida <- aida %>% 
  add_column(
    'ATECO sector code' = substr(.$`ATECO 2007code`, start = 0, stop = 2) )

# Merge ISTAT sectors with AIDA dataframe by ATECO code identifier
aida <- aida %>% 
  add_column(
    'ATECO' = case_when(
      .$`ATECO sector code` >= "01" & .$`ATECO sector code` <= "03" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[1], ATECO_SECTOR$`Titolo Ateco 2007`[1],sep="-"),
      .$`ATECO sector code` >= "05" & .$`ATECO sector code` <= "09" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[2], ATECO_SECTOR$`Titolo Ateco 2007`[2],sep="-"),
      .$`ATECO sector code` >= "10" & .$`ATECO sector code` <= "33" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[3], ATECO_SECTOR$`Titolo Ateco 2007`[3],sep="-"),
      .$`ATECO sector code` == "35"  ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[4], ATECO_SECTOR$`Titolo Ateco 2007`[4],sep="-"),
      .$`ATECO sector code` >= "36" & .$`ATECO sector code` <= "39" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[5], ATECO_SECTOR$`Titolo Ateco 2007`[5],sep="-"),
      .$`ATECO sector code` >= "41" & .$`ATECO sector code` <= "43" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[6], ATECO_SECTOR$`Titolo Ateco 2007`[6],sep="-"),
      .$`ATECO sector code` >= "45" & .$`ATECO sector code` <= "47" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[7], ATECO_SECTOR$`Titolo Ateco 2007`[7],sep="-"),
      .$`ATECO sector code` >= "49" & .$`ATECO sector code` <= "53" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[8], ATECO_SECTOR$`Titolo Ateco 2007`[8],sep="-"),
      .$`ATECO sector code` >= "55" & .$`ATECO sector code` <= "56" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[9], ATECO_SECTOR$`Titolo Ateco 2007`[9],sep="-"),
      .$`ATECO sector code` >= "58" & .$`ATECO sector code` <= "63" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[10], ATECO_SECTOR$`Titolo Ateco 2007`[10],sep="-"),
      .$`ATECO sector code` >= "64" & .$`ATECO sector code` <= "66" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[11], ATECO_SECTOR$`Titolo Ateco 2007`[11],sep="-"),
      .$`ATECO sector code` == "68"  ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[12], ATECO_SECTOR$`Titolo Ateco 2007`[12],sep="-"),
      .$`ATECO sector code` >= "69" & .$`ATECO sector code` <= "75" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[13], ATECO_SECTOR$`Titolo Ateco 2007`[13],sep="-"),
      .$`ATECO sector code` >= "77" & .$`ATECO sector code` <= "82" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[14], ATECO_SECTOR$`Titolo Ateco 2007`[14],sep="-"),
      .$`ATECO sector code` == "84"  ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[15], ATECO_SECTOR$`Titolo Ateco 2007`[15],sep="-"),
      .$`ATECO sector code` == "85"  ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[16], ATECO_SECTOR$`Titolo Ateco 2007`[16],sep="-"),
      .$`ATECO sector code` >= "86" & .$`ATECO sector code` <= "88" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[17], ATECO_SECTOR$`Titolo Ateco 2007`[17],sep="-"),
      .$`ATECO sector code` >= "90" & .$`ATECO sector code` <= "93" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[18], ATECO_SECTOR$`Titolo Ateco 2007`[18],sep="-"),
      .$`ATECO sector code` >= "94" & .$`ATECO sector code` <= "96" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[19], ATECO_SECTOR$`Titolo Ateco 2007`[19],sep="-"),
      .$`ATECO sector code` >= "97" & .$`ATECO sector code` <= "98" ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[20], ATECO_SECTOR$`Titolo Ateco 2007`[20],sep="-"),
      .$`ATECO sector code` == "99"  ~ paste(ATECO_SECTOR$`Codice Ateco 2007`[21], ATECO_SECTOR$`Titolo Ateco 2007`[21],sep="-"),
      TRUE ~ "NO SECTOR"
    ))

aida %>% 
  select(`ATECO`) %>% 
  count(`ATECO`) %>% 
  View()

#Remove objects that are not useful
aida$`ATECO sector code` <- NULL
rm(ateco)
rm(ATECO_SECTOR)

#Datasets for the questions
#For questions A,B and C
df_ABC = aida[, c("Legal form", "Company_age", "Last year", "Region", "ATECO", "Size", "Status")]
df_ABC = df_ABC[complete.cases(df_ABC),] #Taking complete records for selected colums
save(df_ABC, file = "df_ABC.Rdata")

#For question D
df_D <- aida %>% 
  select(`Bank turnover`,
         `Cash flowth`,
         `Cost debit`,
         `Current liabilities`,
         `Current ratio`,
         `Debt/EBITDA`,
         `Debt/equity`,
         `EBITDA Vendite`,
         `EBITDAth`, 
         `Incorporation year` ,
         `Leverage` , 
         `Liquidity ratio` ,
         NET, 
         Capital,
         `Number of employees`,
         `Profit loss`,
         ROA, 
         ROE,
         ROI,
         ROS,
         `Solvency ratio`,
         `Total assets turnover`,
         `Total assets` ,
         Status,
         `Incorporation year`,
         `Last year`.
		 Company_age
         ) %>% 
  na.omit()  

save(df_D, file = "df_D.Rdata")
