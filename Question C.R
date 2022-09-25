#QUESTION C

library(plotly)
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(tidyverse)

load(file = "df_ABC.Rdata")

'
What is the probability of failures conditional to age of
firms at year 2017?
' 

#To answer this question we calculate the probabily of failure
#P( Status=Failed | Age/Size = x ) = P(Status=Failed,Age/Size=x) / P(Age/Size=x)

df_ABC %>% 
  group_by(`Status`,`Last year`) %>% 
  summarise(number = n(),.groups = 'drop') %>% 
  pivot_wider(names_from = `Status`,
              values_from = number,
              values_fill = list(number = 0)) %>% 
  arrange(desc(`Last year`)) %>% 
  View()

# Filter the data for the year 2017
Year = 2017
df_c <- df_ABC %>% 
  filter(`Last year`==Year)

#Distribution of active and failed firms
addmargins (table(df_c$`Status`)*100/nrow(df_c))

#Group by the age and status of the company
Finantial_State_Age <- df_c %>% 
  group_by(`Status`,Company_age) %>% 
  summarise(number = n(), 
            .groups = 'drop') %>% 
  pivot_wider(names_from = `Status`,
              values_from = number,
              values_fill = list(number = 0)) 

Age_Stats <-Finantial_State_Age %>% 
  mutate(Total=Failed+Active,
         Prob_Failed=Failed/Total) %>%
  View()


  mutate(Prob_Failed = if_else(Prob_Failed >= 0.5, 0,Prob_Failed)) 

tit = paste("Year = ",Year)
# Plot the distribution of companies in bankruptcy in the specific year 2017
barplot(Age_Stats$Prob_Failed, main = tit, xlab = 'Age', ylab = 'P(Bankruptcy | Age=x)')
#Plot the companies that have an age less than 50
Age_Stats %>% 
  filter(Prob_Failed < 0.7) %>% 
  ggplot(aes(x=Company_age, 
             y=Prob_Failed))+
  geom_point(size = 3, alpha = 0.3)+
  geom_line(size=2) +
  labs(
    x = "Age" ,
    y = "P(Bankruptcy | Age=x)",
    title = tit
  )+
  theme_minimal()
  
Age_Stats %>% 
  filter(Prob_Failed < 0.5) %>% 
  ggplot(aes(x=Company_age, 
             y=Prob_Failed))+
  geom_bar(stat = "identity") +
  labs(
    x = "Age" ,
    y = "P(Bankruptcy | Age=x)",
    title = tit
  )+
  theme_minimal()

'Does it change for a specific company form (SPA, SRL, etc.)'
for(x in unique(df_c$`Legal form`)){
  tt <- paste(tit,", Legal form = ",x )
  
  Finantial_State <- df_c %>% 
    filter(`Legal form`==x ) %>% 
    group_by(`Status`,Company_age) %>% 
    summarise(number = n(), 
              .groups = 'drop') %>% 
    pivot_wider(names_from = `Status`,
                values_from = number,
                values_fill = list(number = 0)) 
  
  
  Age_Stats_tmp <-Finantial_State %>% 
    mutate(Total=Failed+Active,
           Prob_Failed=Failed/Total)
  
  barplot(Age_Stats_tmp$Prob_Failed,main = tt, xlab = 'Age', ylab = 'P(Bankruptcy | Age=x)' )
  plot(Age_Stats_tmp$Prob_Failed,type = 'h',main = tt, xlab = 'Age', ylab = 'P(Bankruptcy | Age=x)' )
  plot(Age_Stats_tmp$Prob_Failed,type = 'l',main = tt, xlab = 'Age', ylab = 'P(Bankruptcy | Age=x)' )

}
'Does it change for a specific industry sector? (see ATECO sectors)'
for(x  in unique(df_c$ATECO)){
  tt <- paste(tit," ATECO = ",x )
  
  Finantial_State <- df_c %>% 
    filter(`ATECO`==x ) %>% 
    group_by(`Status`,Company_age) %>% 
    summarise(number = n(), 
              .groups = 'drop') %>% 
    pivot_wider(names_from = `Status`,
                values_from = number,
                values_fill = list(number = 0)) 
  
  
  Age_Stats_tmp <-Finantial_State %>% 
    mutate(Total=Failed+Active,
           Prob_Failed=Failed/Total)
  
  barplot(Age_Stats_tmp$Prob_Failed,main = tt, xlab = 'Age', ylab = 'P(Bankruptcy | Age=x)' )
  plot(Age_Stats_tmp$Prob_Failed,type = 'h',main = tt, xlab = 'Age', ylab = 'P(Bankruptcy | Age=x)' )
  plot(Age_Stats_tmp$Prob_Failed,type = 'l',main = tt, xlab = 'Age', ylab = 'P(Bankruptcy | Age=x)' )
  
}

'Does it change for a specific location? (eg., Tuscany, Lombardy, etc.)'

for(x in unique(df_c$Region)){
  tt <- paste(tit,", Region = ",x)
  
  Finantial_State <- df_c %>% 
    filter(`Region`==x ) %>% 
    group_by(`Status`,Company_age) %>% 
    summarise(number = n(), 
              .groups = 'drop') %>% 
    pivot_wider(names_from = `Status`,
                values_from = number,
                values_fill = list(number = 0)) 
  
  
  Age_Stats_tmp <-Finantial_State %>% 
    mutate(Total=Failed+Active,
           Prob_Failed=Failed/Total)
  
  barplot(Age_Stats_tmp$Prob_Failed,main = tt, xlab = 'Age', ylab = 'P(Bankruptcy | Age=x)' )
  plot(Age_Stats_tmp$Prob_Failed,type = 'h',main = tt, xlab = 'Age', ylab = 'P(Bankruptcy | Age=x)' )
  plot(Age_Stats_tmp$Prob_Failed,type = 'l',main = tt, xlab = 'Age', ylab = 'P(Bankruptcy | Age=x)' )
  
}

'What is the probability of failures conditional to SIZE of
  firms at year 2017?'
#Group by the age and status of the company
Finantial_State_Size <- df_c %>% 
  group_by(`Status`,Size) %>% 
  summarise(number = n(), 
            .groups = 'drop') %>% 
  pivot_wider(names_from = `Status`,
              values_from = number,
              values_fill = list(number = 0)) 

addmargins (table(Finantial_State_Size$Size)*100/nrow(Finantial_State_Size))

Size_Stats <-Finantial_State_Size %>% 
  mutate(Total=Failed+Active,
         Prob_Failed=Failed/Total) %>% 
  View()

addmargins (table(Size_Stats$Active)*100/nrow(Size_Stats))
addmargins (table(Size_Stats$Failed)*100/nrow(Size_Stats))

barplot(Size_Stats$Prob_Failed ,main = tit, xlab = 'Size', ylab = 'P(Bankruptcy | Size=x)')
#PLot the companies that have an age less than 50
Size_Stats %>% 
  ggplot(aes(x=Size, 
             y=Prob_Failed,
             fill = Failed))+
  #geom_point(aes(colour = Size),size = 3, alpha = 0.3)+
  geom_bar(stat = "identity")
  labs(
    x = "Size" ,
    y = "P(Bankruptcy | Size=x)",
    title = tit
  )+
  theme_minimal() %>% 
  

'Does it change for a specific company form (SPA, SRL, etc.)'
for(x in unique(df_c$`Legal form`)){
  tt <- paste(tit,", Legal form = ",x )
  
  Finantial_State <- df_c %>% 
    filter(`Legal form`==x ) %>% 
    group_by(`Status`,Size) %>% 
    summarise(number = n(), 
              .groups = 'drop') %>% 
    pivot_wider(names_from = `Status`,
                values_from = number,
                values_fill = list(number = 0)) 
  
  
  Size_Stats_tmp <-Finantial_State %>% 
    mutate(Total=Failed+Active,
           Prob_Failed=Failed/Total)
  
  barplot(Size_Stats_tmp$Prob_Failed, main = tt, xlab = 'Size', ylab = 'P(Bankruptcy | Size=x)', names.arg =Size_Stats_tmp$Size )
  plot(Size_Stats_tmp$Prob_Failed,type = 'h',main = tt, xlab = 'Size', ylab = 'P(Bankruptcy | Size=x)' )
  plot(Size_Stats_tmp$Prob_Failed,type = 'l',main = tt, xlab = 'Size', ylab = 'P(Bankruptcy | Size=x)' )
  
}


'Does it change for a specific industry sector? (see ATECO sectors)'
for(x in unique(df_c$ATECO)){
  tt <- paste(tit,", ATECO = ",x )
  
  Finantial_State <- df_c %>% 
    filter(ATECO==x ) %>% 
    group_by(`Status`,Size) %>% 
    summarise(number = n(), 
              .groups = 'drop') %>% 
    pivot_wider(names_from = `Status`,
                values_from = number,
                values_fill = list(number = 0)) 
  
  
  Size_Stats_tmp <-Finantial_State %>% 
    mutate(Total=Failed+Active,
           Prob_Failed=Failed/Total)
  
  barplot(Size_Stats_tmp$Prob_Failed,main = tt, xlab = 'Size', ylab = 'P(Bankruptcy | Size=x)' , names.arg =Size_Stats_tmp$Size )
  plot(Size_Stats_tmp$Prob_Failed,type = 'h',main = tt, xlab = 'Size', ylab = 'P(Bankruptcy | Size=x)' )
  plot(Size_Stats_tmp$Prob_Failed,type = 'l',main = tt, xlab = 'Size', ylab = 'P(Bankruptcy | Size=x)' )
  
}
'Does it change for a specific location? (eg., Tuscany, Lombardy, etc.)'
for(x in unique(df_c$Region)){
  tt <- paste(tit,", Region = ",x )
  
  Finantial_State <- df_c %>% 
    filter(Region==x ) %>% 
    group_by(`Status`,Size) %>% 
    summarise(number = n(), 
              .groups = 'drop') %>% 
    pivot_wider(names_from = `Status`,
                values_from = number,
                values_fill = list(number = 0)) 
  
  
  Size_Stats_tmp <-Finantial_State %>% 
    mutate(Total=Failed+Active,
           Prob_Failed=Failed/Total)
  
  barplot(Size_Stats_tmp$Prob_Failed,main = tt, xlab = 'Size', ylab = 'P(Bankruptcy | Size=x)',names.arg =Size_Stats_tmp$Size )
  plot(Size_Stats_tmp$Prob_Failed,type = 'h',main = tt, xlab = 'Size', ylab = 'P(Bankruptcy | Size=x)' )
  plot(Size_Stats_tmp$Prob_Failed,type = 'l',main = tt, xlab = 'Size', ylab = 'P(Bankruptcy | Size=x)' )
  
}


Finantial_State <- df_c %>% 
  filter(Region=="Emilia-Romagna" ) %>% 
  group_by(`Status`,Size) %>% 
  summarise(number = n(), 
            .groups = 'drop') %>% 
  pivot_wider(names_from = `Status`,
              values_from = number,
              values_fill = list(number = 0)) 


Size_Stats_tmp <-Finantial_State %>% 
  mutate(Total=Failed+Active,
         Prob_Failed=Failed/Total) %>% 
  View()
