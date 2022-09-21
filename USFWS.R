library(ggplot2)
library(tidyverse)
library(skimr)

setwd("~/Documents/R/USFWS")
lemis<-read_csv("lemis.csv")

str(lemis)
summary(lemis)


lemis %>%
  count(taxa)

fish <- lemis %>%
  filter(taxa=="fish")

str(fish)
summary(fish)

smfish <-fish %>%
  select(taxa, value, shipment_year)

skim(smfish)
summary(smfish)

ggplot(smfish, aes(shipment_year)) + geom_bar() + xlab ("Year") + 
  ylab ("Number of Fish Shipments to US") + 
  ggtitle(label="Imported Fish Shipments Between 2000-2014", subtitle="Katy Baker")

ggplot(smfish, aes(taxa, value)) + geom_jitter(height=0, width= .5, alpha=.7, size=1, color="blue")

outlier<-lemis %>%
  filter(value>1e7 & taxa=="fish") %>%
  arrange(value)
outlier

smsmfish<- smfish %>%
  filter(value<2e7) 

outlier2<-lemis %>%
  filter(value<1 & taxa=="fish") %>%
  arrange(desc(value))
outlier2

smsmfish<- smfish %>%
  filter(between(value,1, 2e7))


skim(smsmfish)

ggplot(smsmfish, aes(taxa, value)) + 
  geom_jitter(height=0, width= .5, alpha=.7, size=1, color="blue") + 
  xlab ("") + ylab ("Value of Fish Shipments") + 
  ggtitle(label="Declared Value of Imported Fish Shipments Between 2000-2014", subtitle="Katy Baker")


smsmfish<- smfish %>%
  filter(between(value,5, 2500))

ggplot(smsmfish, aes(taxa, value)) + geom_boxplot(color="blue") + 
  xlab ("") + ylab ("Value of Fish Shipments") + 
  ggtitle(label="Declared Value of Imported Fish Shipments Under $2500 Between 2000-2014", subtitle="Katy Baker")


