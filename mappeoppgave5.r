library(ggplot2)
library(gglorenz)
library(tidyverse)
library(PxWebApiData)
library(dplyr)
library(rjstat)
library(ineq)
library(janitor)
library(httr)
library(scales)


#laster inn data fra ssb slik vi har gjort før
ssb <- "https://data.ssb.no/api/v0/no/table/05185/"


df <- '{ "query": [ { "code": "Kjonn", "selection": { "filter": "item", "values": [ "1", "2" ] } }, { "code": "Landbakgrunn", "selection": { "filter": "agg:Verdensdel2", "values": [ "b11", "b12", "b13", "b14", "b2", "b3", "b4", "b5", "b6", "b8", "b9" ] } }, { "code": "Tid", "selection": { "filter": "item", "values": [ "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022" ] } } ], "response": { "format": "json-stat2" } }'

df1 <- POST(ssb, body = df, encode = "json", verbose())

df_bakgrunn <- fromJSONstat(content(df1, "text")) %>% 
  clean_names() %>% 
  as.data.frame()

#filter data
df_bakgrunn <- df_bakgrunn %>%
  group_by(landbakgrunn, ar) %>% 
  mutate(total_innvandring = sum(value)) %>% 
  filter(kjonn == "Menn")


#lager plot
ggplot(df_bakgrunn, aes(ar , total_innvandring, fill = landbakgrunn))+
  geom_col()+
  scale_y_continuous(labels = scales::comma)+
  labs(x = "År",
       y= "Antall innvandre") +
  theme(panel.background = element_rect(fill = "white", color = "white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  labs(fill = "Kontinenter") +
  theme_bw()

#lager plot

ggplot(df_bakgrunn, aes(ar, total_innvandring, group = landbakgrunn, col = landbakgrunn)) +
  geom_line(aes(group = landbakgrunn))+
  geom_line(size = 0.9) +
  labs(x = "År",
       y= "Antall innvandre",
       title= "Oversikt over innvandring til Norge siden 2005") +
  scale_y_continuous(labels = scales::comma) + 
  scale_color_brewer(palette="Accent")


##oppgave 5.1.2
#laster inn data fra ssb slik vi har gjort før

ssb2 <- 'https://data.ssb.no/api/v0/no/table/13215/'
  
df2 <- '{ "query": [ { "code": "Kjonn", "selection": { "filter": "item", "values": [ "0" ] } }, { "code": "Alder", "selection": { "filter": "item", "values": [ "15-74" ] } }, { "code": "InnvandrKat", "selection": { "filter": "item", "values": [ "B" ] } }, { "code": "Landbakgrunn", "selection": { "filter": "item", "values": [ "015a" ] } }, { "code": "NACE2007", "selection": { "filter": "agg:NACE260InnvGrupp2", "values": [ "SNI-01-03", "SNI-05-09", "SNI-10-33", "SNI-35-39", "SNI-41-43", "SNI-45-47", "SNI-49-53", "SNI-49.3", "SNI-55", "SNI-56", "SNI-58-63", "SNI-64-66", "SNI-68-75", "SNI-77-82", "SNI-78.2", "SNI-81.2", "SNI-84", "SNI-85", "SNI-86-88", "SNI-90-99", "SNI-00" ] } }, { "code": "Tid", "selection": { "filter": "item", "values": [ "2021" ] } } ], "response": { "format": "json-stat2" } }'

df3 <- POST(ssb2, body = df2, encode = "json", verbose())

df_grupper <- fromJSONstat(content(df3, "text")) %>% 
  clean_names() %>% 
  as.data.frame()
#filter data
df_grupper <- df_grupper %>% 
  rename('Sektorer' = 'naering_sn2007',
         "År" = "ar")  %>% 
  subset(select= -c(statistikkvariabel))

#forkorter navenene
df_grupper$Sektorer <- gsub("[0-9.-]","", df_grupper$Sektorer)

#lager plot

df_grupper %>% 
  ggplot(aes(Sektorer , value, fill = Sektorer)) + 
  geom_col() +
  scale_y_continuous(labels = scales::comma)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "Antall sysselsatte",
       title = "Antall innvandrere i arbeid fordelt på sektor")




















  
