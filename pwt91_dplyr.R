# PWT Deeper Structures using dplyr
library(tidyverse)

library(dplyr)

library(data.table)
library(ggplot2)
library("rio")
library(matlib)
library(gdata)
library(tinytex)
library(scales)
library(ggplot2)
library(foreign)
library(rmarkdown)

options(scipen=10000)
options(digits=4)

#setwd("C:/Users/User/Documents/GitHub/deeper_structures")

x  <- read_csv("pwt91.csv")  # PWT 9.1
x<-x %>% filter(year>1953) # Drops data < 1954
x<-x %>% rename(c=csh_c) # Consumption share
x<-x %>% mutate(c=ifelse(c<0|c>1,NA,c)) # Out of bound consumption share -> NA

x<-x %>% rename(Y=rgdpo, # Output-method real GDP
                Ye=rgdpe, # Expenditure-method real GDP (Kaleckian assumption)
                om=labsh, # Wage share
                l=avh, # Per-worker hours worked (lots of missing data)
                Ks=rkna, # Real capital services (annual flow)
                K=rnna,  # Real capital stock
                N=pop, # Population
                E=emp) # Engaged workers (employment)


x<-x %>% mutate(W=om*Y, # Real wages
                C=c*Ye, # Consumption spending
                L=l*E) # Labor time (estimated of annual hours worked)



x<-x %>% select(c(countrycode,country,currency_unit ,year,c,Y,Ye,om,l,Ks,W,C,K,N,E,L)) # Selects variables
str(x)
x0<-x

#weights

global<-x %>% group_by(year) %>% 
  summarise(NO=sum(N,na.rm=TRUE),Y0=sum(Y,na.rm=TRUE)) %>%  # Aggregates global population and real GDP (year series)
  ungroup()

x0<-x0 %>% mutate(theta=Y/rep(global$Y0,182), delta=N/rep(global$NO,182))

summary(x0$delta)
summary(x0$theta)
# Test of weights
x0 %>% filter(theta>.346) # USA
x0 %>% filter(delta>0.29) # CHN

# STRUCTURAL PARAMETERS FOR EACH COUNTRY/YEAR
x<-x %>% mutate(yN=Y/N, # Per capita productivity
                E=Y/E,  # Per worker productivity
                yL=Y/L, # Per hour productivity
                wN=om*Y/N, # Wage per capita
                wE=(om*Y)/E, # Wage per worker
                wL=om*Y/L,   # Wage per hour
                s=(1-om)/om, # Exploitation rate
                ss=(1-c)/c, # Exploitation rate (Kalecki)
                h=Ks/W,    # Capital composition (flow-flow)
                rho=Y/K,   # Capital productivity (flow-stock)
                r=(1-om)*Y/K,  # Profit rate
                ka=(1-c)*Y/K)  # Accumulation rate (Kalecki profit rate)
  
# GLOBAL PRIMARY VARIABLES (YEAR SERIES)
#g: Global data set: levels and structural proportions
g<-x %>% group_by(year) %>% 
  summarise(N=sum(N, na.rm=TRUE), # Aggregates population (year series)
            E=sum(E, na.rm=TRUE), # Aggregates employment (year series)
            L=sum(L, na.rm=TRUE), # Aggregates labor hours (year series)
            Y=sum(Y, na.rm=TRUE), # Aggregates real GDP (year series)
            Ye=sum(Ye, na.rm=TRUE), # Aggregates real GDP exp. (year series)
            W=sum(W, na.rm=TRUE),  # Aggregates wages (year series)
            C=sum(C, na.rm=TRUE), # Aggregates consumption spend. (year series)
            K=sum(K, na.rm=TRUE), # Aggregates capital stock (year series)
            Ks=sum(Ks, na.rm=TRUE),) %>%# Aggregates capital services flow (year series) 
  ungroup()
   
# GLOBAL STRUCTURES (YEAR SERIES)

g<-g %>% mutate(yN=Y/N, # Global per-capita productivity
                yE=Y/E, # Global per-worker productivity
                yL=Y/L, # Global per-hour productivity
                wN=W/N, # Global per-capita wage
                wE=W/E, # Global per-worker wage
                wL=W/L, # Global per-hour wage
                s=(Y-W)/W, # Global exploitation rate
                ss=(Y-C)/C, # Global exploitation rate (Kalecki)
                h=Ks/W, # Global capital composition (flow-flow)
                rho=Y/K, # Global capital productivity (flow-stock)
                r=(Y-W)/K, # Global profit rate
                ka=(Y-C)/K,
                year=c(1954:2017)) # Global accumulation rate



