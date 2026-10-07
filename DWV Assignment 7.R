#Author: Leah Kim 
#Title: Assignment 7
#Topic: The ANES
#Date: 10/1/26

#Load all libraries
library(expss)
library(tidyverse)
library(foreign)
library(haven)
library(dplyr)
library(ggplot2)
library(lattice)

#Load the data 
anes <- read_dta('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/anes_timeseries_cdf_stata_20220916.dta')

#party identification, 7pt scale 
anes$VCF0301

anes_sub <- anes |> select(VCF0301, VCF0004)
anes_sub <- filter(anes_sub, VCF0004 >= 1952 )
table(anes_sub$VCF0301)
unique(anes_sub$VCF0301)

#Questions 1 and 2
#Figure 1(a): The Distribution of Party Identification, 1952-2026
#Recode for "Strong" and "Weak" Identifiers 
figure1 <- anes_sub |>
  filter(VCF0004 >= 1952, VCF0301 %in% 1:7) |>
  mutate(
    pid_strength = case_when(
      VCF0301 %in% c(1,7) ~ "Strong", 
      VCF0301 %in% c(2,6) ~ "Weak", 
      TRUE ~ "Other"
    )
  ) |>
  count(year = VCF0004, pid_strength) |>
  group_by(year) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  filter(pid_strength != "Other")

#Plot Figure 1
ggplot(figure1, aes(year, prop, linetype = pid_strength, shape = pid_strength)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(limits=c(0,0.5)) +
  scale_x_continuous(breaks = seq(1952, 2020, 8))+
  labs(x=NULL, y=NULL, title="The Distribution of Party Identification, 1952-2026")

#Figure 1(b): The Distribution of Party Identification, 1952-2026
#Recode for Leaner and Pure 
figure2 <- anes_sub |>
  filter(VCF0004 >= 1952, VCF0301 %in% 1:7) |>
  mutate(
    pid_sway = case_when(
      VCF0301 %in% c(3,5) ~ "Leaner", 
      VCF0301 == 4 ~ "Pure", 
      TRUE ~ "Other"
    )
  ) |>
  count(year = VCF0004, pid_sway) |>
  group_by(year) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  filter(pid_sway != "Other")

#Plot Figure 2
ggplot(figure2, aes(year, prop, linetype = pid_sway, shape = pid_sway)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(limits=c(0,0.5)) +
  scale_x_continuous(breaks = seq(1952, 2020, 8))+
  labs(x=NULL, y=NULL, title="The Distribution of Party Identification, 1952-2026")

#Questions 3
anes2024 <- read_dta('/Users/leahkim/Desktop/academics ✩ ° ｡⋆/PAI 741/Data for HW/anes_timeseries_2024_stata_20250808.dta')
anes2024$V241227x

#Mutate and Recode 
anes2024_sub <- anes2024 |>
  transmute(
    VCF0004 = 2024, 
    VCF0301 = as.numeric(V241227x)
  )
anes_sub <- bind_rows(anes_sub, anes2024_sub)
anes_sub <- filter(anes_sub, VCF0004 >= 1952)
table(anes_sub$VCF0301)
unique(anes_sub$VCF0004)

#Recode for Figure 1 and 2 
figure1 <- anes_sub |>
  filter(VCF0004 >= 1952, VCF0301 %in% 1:7) |>
  mutate(pid_strength = case_when(
    VCF0301 %in% c(1,7) ~ "Strong", 
    VCF0301 %in% c(2,6) ~ "Weak", 
    TRUE ~ "Other")
  ) |>
  count(year = VCF0004, pid_strength) |> 
  group_by(year) |>
  mutate(prop = n / sum(n)) |> 
  ungroup() |> 
  filter(pid_strength != "Other")

figure2 <- anes_sub |>
  filter(VCF0004 >= 1952, VCF0301 %in% 1:7) |>
  mutate(
    pid_sway = case_when(
      VCF0301 %in% c(3,5) ~ "Leaner", 
      VCF0301 == 4 ~ "Pure", 
      TRUE ~ "Other"
    )
  ) |>
  count(year=VCF0004, pid_sway) |>
  group_by(year) |>
  mutate(prop = n / sum(n)) |>
  ungroup() |>
  filter(pid_sway != "Other")

#Plots for Fig 1(a) New 
ggplot(figure1, aes(year, prop, linetype = pid_strength, shape = pid_strength)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(limits=c(0,0.5)) +
  scale_x_continuous(breaks = seq(1952, 2024, 8))+
  labs(x=NULL, y=NULL, title="The Distribution of Party Identification, 1952-2026")

#Plots for Fig 1(b) New
ggplot(figure2, aes(year, prop, linetype = pid_sway, shape = pid_sway)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(limits=c(0,0.5)) +
  scale_x_continuous(breaks = seq(1952, 2024, 8))+
  labs(x=NULL, y=NULL, title="The Distribution of Party Identification, 1952-2026")

#Adding midterm elections 
#Fig 1(a)
figure1 <- figure1 |>
  mutate(election = if_else(year %% 4 == 0, "Presidential", "Midterm"))
#Fig 1(b)
figure2 <- figure2 |>
  mutate(election = if_else(year %% 4 == 0, "Presidential", "Midterm"))
#Check work
figure1 |> distinct(year,election) |> arrange(year) |> print(n=Inf)
#Now plot 
#Plots for Fig 1(a) New 
ggplot(figure1, aes(year, prop, linetype = pid_strength, shape = pid_strength)) + 
  geom_line() + 
  geom_point(aes(shape = election)) + 
  scale_shape_manual(values = c(Presidential = 16, Midterm =1)) + 
  scale_y_continuous(limits=c(0,0.5)) +
  scale_x_continuous(breaks = seq(1952, 2024, 8))+
  labs(x=NULL, y=NULL, title="The Distribution of Party Identification, 1952-2026")

#Plots for Fig 1(b) New
ggplot(figure2, aes(year, prop, linetype = pid_sway, shape = pid_sway)) + 
  geom_line() + 
  geom_point(aes(shape = election)) + 
  scale_shape_manual(values = c(Presidential = 16, Midterm = 1))+
  scale_y_continuous(limits=c(0,0.5)) +
  scale_x_continuous(breaks = seq(1952, 2024, 8))+
  labs(x=NULL, y=NULL, title="The Distribution of Party Identification, 1952-2026")


#OWN WORK 
#Approve/Disapprove House Incumbent V242163x 
anes2024 <- anes2024 |>
  mutate(approval_incumbent = case_when(
    V242163x == 1 ~ 1, 
    V242163x == 2 ~ 2, 
    V242163x == 3 ~ 3, 
    V242163x == 4 ~ 4,
    TRUE ~ NA
  ))
#CLT 
mean(anes2024$approval_incumbent, na.rm=TRUE)
sd(anes2024$approval_incumbent, na.rm=TRUE)
#Distribution 
barplot(table(anes2024$approval_incumbent),
        main = "Distribution of Approval Ratings of the House Incumbent", 
        xlab = "Scale of Approval",
        ylab = "Number of Respondents", 
        names.arg = c("Approve Strongly", "Approve not Strongly", 
                      "Disapprove not strongly", "Disapprove Srongly"))
#Favor/Oppose New Limits on Imports V242226x
anes2024 <- anes2024 |>
  mutate(import_favor = case_when(
    V242226x == 1 ~ 1, 
    V242226x == 2 ~ 2, 
    V242226x == 3 ~ 3, 
    V242226x == 4 ~ 4,
    TRUE ~ NA
  ))
#CLT 
mean(anes2024$import_favor, na.rm=TRUE)
sd(anes2024$import_favor, na.rm=TRUE)
#Distribution 
barplot(table(anes2024$import_favor),
        main = "Distribution of Opinion on New Limits on Imports", 
        xlab = "Scale of Opinion(Favor/Oppose)",
        ylab = "Number of Respondents", 
        names.arg = c("Favor Strongly", "Favor Not Strongly", 
                      "Oppose Not Strongly", "Oppose strongly"))