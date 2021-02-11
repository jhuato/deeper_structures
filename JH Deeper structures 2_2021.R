# JH 
# DSPWT 2/2021

## Attention: Jesús

library(tidyverse)

x  <- read_csv("Downloads/pwt100.csv") 

head(x)
x <- x %>% 
  select(countrycode, country, year, pop, emp, avh, rgdpe, rgdpo, labsh, 
         csh_c, rkna, rnna) %>%
  filter(year > 1953) %>%
  rename(N=pop, E=emp, l=avh, Ye=rgdpe, Y=rgdpo, om=labsh, c=csh_c, 
         Ks=rkna, K=rnna)  
x$c[x$c<0 | x$c>1] <- NA 

x <- x %>% 
  mutate(W=om*Y, C=c*Ye, L=l*E)
glimpse(x)
summary(x)

# Global parameters by year:
g <- x %>%
  group_by(year) %>%
  summarise_at(vars(-c(countrycode, country, l, om, c)), sum, na.rm = TRUE)
glimpse(g)
gp <- g %>%
  mutate(yN=Y/N, yE=Y/E, yL=Y/L, yeN=Ye/N, yeE=Ye/E, yeL=Ye/L, wN=W/N, 
         wE=W/E, wL=W/L, cN=C/N, cE=C/E, cL=C/L, s=(Y-W)/W, ss=(Ye-C)/C, 
         h=Ks/W, hs=K/E, rho=Y/K, r=(Y-W)/K, k=(Ye-C)/K) %>%
  select(year, yN, yE, yL, yeN, yeE, yeL, wN, wE, wL, cN, cE, cL, s, ss, 
         h, hs, rho, r, k)
glimpse(gp)
summary(gp)

# International by country and year:
glimpse(x)
p <- x %>%
  mutate(yN=Y/N, yE=Y/E, yL=Y/L, yeN=Ye/N, yeE=Ye/E, yeL=Ye/L, wN=W/N, 
         wE=W/E, wL=W/L, cN=C/N, cE=C/E, cL=C/L, s=(Y-W)/W, ss=(Ye-C)/C, 
         h=Ks/W, hs=K/E, rho=Y/K, r=(Y-W)/K, k=(Ye-C)/K) %>%
  select(countrycode, country, year, N, Y, Ye, yN, yE, yL, yeN, yeE, yeL, 
         wN, wE, wL, cN, cE, cL, s, ss, h, hs, rho, r, k)
glimpse(p)

ggplot(p, aes(x=year, y=yN, group=country)) + 
  geom_path() + ggtitle("Per-capita output", 
                        subtitle = "Source: PWT 10.0") + 
  ylab("2017 USD") + xlab("Year") + facet_wrap(~ country)

## Too crowded.  Perhaps in groups of 20 countries each?

# International unweighted parameters by year:
up <- p %>%
  group_by(year) %>%
  summarise_at(vars(-countrycode, -country, -N, -Y, -Ye), mean, na.rm = TRUE)
glimpse(up)

# International unweighted time-series plots:

## 


# International N-weighted by year:

Nwp <- p %>%
  group_by(year) %>%
  mutate(w=N/sum(N, na.rm = TRUE)) 
glimpse(Nwp)

Nwp <- Nwp %>%
  mutate(yN=yN*w, yE=yE*w, yL=yL*w, yeN=yeN*w, yeE=yeE*w, yeL=yeL*w, wN=wN*w, 
         wE=wE*w, wL=wL*w, cN=cN*w, cE=cE*w, cL=cL*w, s=s*w, ss=ss*w, h=h*w, 
         rho=rho*w, r=r*w, k=k*w) %>%
  summarise_at(vars(yN, yE, yL, yeN, yeE, yeL, wN, wE, wL, cN, cE, cL, s, ss, 
                    h, rho, r, k), sum, na.rm = TRUE)
glimpse(Nwp)

# International N-weighted time series plots:

## 

# International Y-weighted by year:

Ywp <- p %>%
  group_by(year) %>%
  mutate(w=Y/sum(Y, na.rm = TRUE)) 
glimpse(Ywp)

Ywp <- Ywp %>%
  mutate(yN=yN*w, yE=yE*w, yL=yL*w, yeN=yeN*w, yeE=yeE*w, yeL=yeL*w, wN=wN*w, 
         wE=wE*w, wL=wL*w, cN=cN*w, cE=cE*w, cL=cL*w, s=s*w, ss=ss*w, h=h*w, 
         rho=rho*w, r=r*w, k=k*w) %>%
  summarise_at(vars(yN, yE, yL, yeN, yeE, yeL, wN, wE, wL, cN, cE, cL, s, ss,
                    h, rho, r, k), sum, na.rm = TRUE)
glimpse(Ywp)

# International Y-weighted time series plots:

##


# Global time-series plots:

## These plots need legends etc.

ggplot(gp, aes(x=year)) + 
  geom_step(aes(y = yN), color = "red") + 
  geom_step(aes(y = yeN), color="blue") +
  ggtitle("Global per-capita output and spending", 
          subtitle = "Source: PWT 10.0") + 
  ylab("2017 USD") + xlab("Year") + 
  scale_color_manual(values = c("blue","red")) +
  scale_x_continuous(breaks= seq(1954,2019,by=5))

ggplot(gp, aes(x=year)) + 
  geom_step(aes(y = yE), color = "red") + 
  geom_step(aes(y = yeE), color="blue") +
  ggtitle("Global per-worker output and spending", 
          subtitle = "Source: PWT 10.0") + 
  ylab("2017 USD") + xlab("Year") + 
  scale_color_manual(values = c("blue","red")) +
  scale_x_continuous(breaks= seq(1954,2019,by=5))

ggplot(gp, aes(x=year)) + 
  geom_step(aes(y = yL), color = "red") + 
  geom_step(aes(y = yeL), color="blue") +
  ggtitle("Global per-hour output and spending", 
          subtitle = "Source: PWT 10.0") + 
  ylab("2017 USD") + xlab("Year") + 
  scale_color_manual(values = c("blue","red")) +
  scale_x_continuous(breaks= seq(1954,2019,by=5))

ggplot(gp, aes(x=year)) + 
  geom_step(aes(y = s), color = "red") + 
  geom_step(aes(y = ss), color="blue") +
  ggtitle("Exploitation rate: output and spending", 
          subtitle = "Source: PWT 10.0") + 
  ylab(" ") + xlab("Year") +
  scale_color_manual(values = c("blue","red")) +
  scale_x_continuous(breaks= seq(1954,2019,by=5))

ggplot(gp, aes(x=year, y=h, group=1)) + 
  geom_step() + ggtitle("Flow-flow capital composition", 
                        subtitle = "Source: PWT 10.0") + 
  ylab(" ") + xlab("Year") +
  scale_x_continuous(breaks= seq(1954,2019, by=5))

ggplot(gp, aes(x=year, y=hs, group=1)) + 
  geom_step() + ggtitle("Capital composition (stock)", 
                        subtitle = "Source: PWT 10.0") + 
  ylab("2017 USD/worker") + xlab("Year") +
  scale_x_continuous(breaks= seq(1954,2019, by=5))

ggplot(gp, aes(x=year, y=rho, group=1)) + 
  geom_step() + ggtitle("Output-capital ratio", 
                        subtitle = "Source: PWT 10.0") + 
  ylab(" ") + xlab("Year") +
  scale_x_continuous(breaks= seq(1954,2019, by=5))

ggplot(gp, aes(x=year)) + 
  geom_step(aes(y = r), color = "blue") + 
  geom_step(aes(y = k), color="red") + 
  ggtitle("Rates of profit and accumulation", 
          subtitle = "Source: PWT 10.0") + 
  ylab(" ") + xlab("Year") + 
  scale_color_manual(values = c("blue","red")) + 
  scale_x_continuous(breaks= seq(1954,2019,by=5))
  
  # Regional analysis in a separate R file.
