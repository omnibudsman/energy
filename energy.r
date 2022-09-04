library(tidyverse)
library(scales)
library(ggpubr)

#################### Set directory

# https://electionstudies.org/data-center/2020-time-series-study/ 
setwd(Sys.getenv(ENERGY_DIR))

#################### Read in data

# https://ourworldindata.org/energy-production-consumption#per-capita-where-do-people-consume-the-most-energy
energy.use.raw <- read.csv("per-capita-energy-use.csv") %>% select(-Code)

# https://ourworldindata.org/grapher/infant-mortality
infant.mortality.raw <- read.csv("infant-mortality.csv") %>% select(-Code)

# $1.90 poverty
poverty.raw <- read.csv("share-of-population-in-extreme-poverty.csv") %>% select(-Code)

# https://ourworldindata.org/happiness-and-life-satisfaction
happiness.raw <- read.csv("gdp-vs-happiness.csv") %>% select(-Code)

# https://ourworldindata.org/grapher/gross-enrollment-ratio-in-secondary-education?tab=table
secondary.education.raw <- read.csv("gross-enrollment-ratio-in-secondary-education.csv") %>% select(-Code)

##########################################
# Simple Visualizations
##########################################

# new merges
energy.x.poverty <- merge(energy.use.raw, poverty.raw, by=c("Entity","Year")) %>% group_by(Entity) %>% mutate(maxyear = (Year == max(Year))) %>% subset(maxyear == 1)
energy.x.happiness <- merge(energy.use.raw, happiness.raw, by=c("Entity","Year")) %>% group_by(Entity) %>% mutate(maxyear = (Year == max(Year))) %>% subset(maxyear == 1)
energy.x.mortality <- merge(energy.use.raw, infant.mortality.raw, by=c("Entity","Year")) %>% group_by(Entity) %>% mutate(maxyear = (Year == max(Year))) %>% subset(maxyear == 1)
energy.x.education <- merge(energy.use.raw, secondary.education.raw, by=c("Entity","Year")) %>% group_by(Entity) %>% mutate(maxyear = (Year == max(Year))) %>% subset(maxyear == 1)

# energy vs poverty
g1 <- ggplot(energy.x.poverty, aes(x = Primary.energy.consumption.per.capita..kWh.person., y = X.1.90.a.day...share.of.population.below.poverty.line)) +
  geom_point(color="#676a70") +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  theme_minimal() +
  geom_smooth(method="glm", formula="y ~ x + poly(x,2)", se = F, color="#c4aeb5") +
  xlab("Primary energy consumption per capita\n(kwH, log scale)") +
  ylab("Share of population below $1.90/day\npoverty line (log scale)\n") +
  ggtitle("Energy consumption versus extreme poverty") +
  scale_fill_manual(values=omni.colors) + # currently not doing anything
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt"))

# energy vs infant mortality
g2 <- ggplot(energy.x.mortality, aes(x = Primary.energy.consumption.per.capita..kWh.person., y = Mortality.rate..infant..per.1.000.live.births.)) +
  geom_point(color="#676a70") +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  theme_minimal() +
  geom_smooth(method="glm", formula="y ~ x + poly(x,2)", se = F, color="#c4aeb5") +
  xlab("Primary energy consumption per capita\n(kwH, log scale)") +
  ylab("Infant deaths per 1000 live births\n(log scale)\n") +
  ggtitle("Energy consumption versus infant mortality") +
  scale_fill_manual(values=omni.colors) + # currently not doing anything
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt"))

# energy vs happiness
g3 <- ggplot(energy.x.happiness, aes(x = Primary.energy.consumption.per.capita..kWh.person., y = Life.satisfaction.in.Cantril.Ladder..World.Happiness.Report.2022.)) +
  geom_point(color="#676a70") +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  theme_minimal() +
  geom_smooth(method="glm", formula="y ~ x + poly(x,2)", se = F, color="#c4aeb5") +
  xlab("Primary energy consumption per capita\n(kwH, log scale)") +
  ylab("Self-reported life satisfaction (1-10 scale)\n") +
  ggtitle("Energy consumption versus self-reported life satisfaction") +
  scale_fill_manual(values=omni.colors) + # currently not doing anything
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt")) +
  labs(caption = "\n(Note: Life satisfaction data available for a subset of countries)")

# working hours
g4 <- ggplot(energy.x.education, aes(x = Primary.energy.consumption.per.capita..kWh.person., y = Gross.enrolment.ratio..secondary..both.sexes....)) +
  geom_point(color="#676a70") +
  scale_x_log10(labels = comma) +
  theme_minimal() +
  geom_smooth(method="glm", formula="y ~ x + poly(x,2)", se = F, color="#c4aeb5") +
  xlab("Primary energy consumption per capita\n(kwH, log scale)") +
  ylab("Gross enrollment ratio in secondary education\n") +
  ggtitle("Energy consumption versus enrollment\nin secondary education") +
  scale_fill_manual(values=omni.colors) + # currently not doing anything
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt")) +
  labs(caption = "\n(Note: Can exceed 100% due to grade repetition\nover- or under-aged students, and other factors)")

##########################################
# Combine viz
##########################################

g <- ggarrange(g1,g2,g3,g4)
g

