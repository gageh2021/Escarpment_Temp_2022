install.packages("tidyverse")
install.packages("dplyr")
install.packages("purrr")

library(tidyverse)
library(dplyr)
library(readr)
library(purrr)

#reading csv files into data table
SydenhamHill_1.1 <- read.csv("SydenhamHill_Probe1.1.csv")
SydenhamHill_1.2 <- read.csv("SydenhamHill_Probe1.2.csv")
SydenhamHill_1.3 <- read.csv("SydenhamHill_Probe1.3.csv")
SydenhamHill_1.4 <- read.csv("SydenhamHill_Probe1.4.csv")
SydenhamHill_1.5 <- read.csv("SydenhamHill_Probe1.5.csv")
SydenhamHill_1.6 <- read.csv("SydenhamHill_Probe1.6.csv")
SydenhamHill_1.7 <- read.csv("SydenhamHill_Probe1.7.csv")
SydenhamHill_1.8 <- read.csv("SydenhamHill_Probe1.8.csv")
SydenhamHill_1.9 <- read.csv("SydenhamHill_Probe1.9.csv")
SydenhamHill_1.10 <- read.csv("SydenhamHill_Probe1.10.csv")
SydenhamHill_1.11 <- read.csv("SydenhamHill_Probe1.11.csv")
SydenhamHill_1.12 <- read.csv("SydenhamHill_Probe1.12.csv")
SydenhamHill_1.13 <- read.csv("SydenhamHill_Probe1.13.csv")
SydenhamHill_1.14 <- read.csv("SydenhamHill_Probe1.14.csv")
SydenhamHill_1.15 <- read.csv("SydenhamHill_Probe1.15.csv")
SydenhamHill_1.16 <- read.csv("SydenhamHill_Probe1.16.csv")
SydenhamHill_1.17 <- read.csv("SydenhamHill_Probe1.17.csv")
SydenhamHill_1.18 <- read.csv("SydenhamHill_Probe1.18.csv")

SydenhamHill_2.1 <- read.csv("SydenhamHill_Probe2.1.csv")
SydenhamHill_2.2 <- read.csv("SydenhamHill_Probe2.2.csv")
SydenhamHill_2.3 <- read.csv("SydenhamHill_Probe2.3.csv")
SydenhamHill_2.4 <- read.csv("SydenhamHill_Probe2.4.csv")
SydenhamHill_2.5 <- read.csv("SydenhamHill_Probe2.5.csv")
SydenhamHill_2.6 <- read.csv("SydenhamHill_Probe2.6.csv")
SydenhamHill_2.7 <- read.csv("SydenhamHill_Probe2.7.csv")
SydenhamHill_2.8 <- read.csv("SydenhamHill_Probe2.8.csv")
SydenhamHill_2.9 <- read.csv("SydenhamHill_Probe2.9.csv")
SydenhamHill_2.10 <- read.csv("SydenhamHill_Probe2.10.csv")
SydenhamHill_2.11 <- read.csv("SydenhamHill_Probe2.11.csv")
SydenhamHill_2.12 <- read.csv("SydenhamHill_Probe2.12.csv")
SydenhamHill_2.13 <- read.csv("SydenhamHill_Probe2.13.csv")
SydenhamHill_2.14 <- read.csv("SydenhamHill_Probe2.14.csv")
SydenhamHill_2.15 <- read.csv("SydenhamHill_Probe2.15.csv")
SydenhamHill_2.16 <- read.csv("SydenhamHill_Probe2.16.csv")
SydenhamHill_2.17 <- read.csv("SydenhamHill_Probe2.17.csv")
SydenhamHill_2.18 <- read.csv("SydenhamHill_Probe2.18.csv")

#renaming column names
names(SydenhamHill_1.1) <- c("X.", "Date-Time (EST)", "FT", "ST", "y", "z")
         # SydenhamHill_1.1[nrow(SydenhamHill_1.1) +1]
         SydenhamHill_1.1 <- subset(SydenhamHill_1.1, select = -c(y,z))
names(SydenhamHill_1.2) <- c("X.", "Date-Time (EST)", "FT", "ST", "y", "z")
         SydenhamHill_1.2 <- subset(SydenhamHill_1.2, select = -c(y,z))
names(SydenhamHill_1.3) <- c("X.", "Date-Time (EST)", "FT", "ST", "y", "z")
         SydenhamHill_1.3 <- subset(SydenhamHill_1.3, select = -c(y,z))
names(SydenhamHill_1.4) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.5) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.6) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.7) <- c("X.", "Date-Time (EST)", "FT", "ST", "y", "z")
          SydenhamHill_1.7 <- subset(SydenhamHill_1.7, select = -c(y,z))
names(SydenhamHill_1.8) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.9) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.10) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.11) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.12) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.13) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.14) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.15) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.16) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.17) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_1.18) <- c("X.", "Date-Time (EST)", "FT", "ST", "a", "b", "c", "d", "e", "f")
          SydenhamHill_1.18 <- subset(SydenhamHill_1.18, select = -c(a,b,c,d,e,f))

names(SydenhamHill_2.1) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.2) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.3) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.4) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.5) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.6) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.7) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.8) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.9) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.10) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.11) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.12) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.13) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.14) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.15) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.16) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.17) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(SydenhamHill_2.18) <- c("X.", "Date-Time (EST)", "FT", "ST")
          

#combining CSV files
SydenhamHill_FullProbe1 <- unique (rbind (SydenhamHill_1.1, SydenhamHill_1.2, SydenhamHill_1.3, SydenhamHill_1.4, 
                                SydenhamHill_1.5, SydenhamHill_1.6, SydenhamHill_1.7, SydenhamHill_1.8, 
                                SydenhamHill_1.9, SydenhamHill_1.10, SydenhamHill_1.11, SydenhamHill_1.12, 
                                SydenhamHill_1.13, SydenhamHill_1.14, SydenhamHill_1.15, SydenhamHill_1.16, 
                                SydenhamHill_1.17, SydenhamHill_1.18))

SydenhamHill_FullProbe2 <- unique (rbind (SydenhamHill_2.1, SydenhamHill_2.2, SydenhamHill_2.3, SydenhamHill_2.4, 
                                          SydenhamHill_2.5, SydenhamHill_2.6, SydenhamHill_2.7, SydenhamHill_2.8, 
                                          SydenhamHill_2.9, SydenhamHill_2.10, SydenhamHill_2.11, SydenhamHill_2.12, 
                                          SydenhamHill_2.13, SydenhamHill_2.14, SydenhamHill_2.15, SydenhamHill_2.16, 
                                          SydenhamHill_2.17, SydenhamHill_2.18))

#Probe 1
require (dplyr)
#difference FT
difference_FT <- SydenhamHill_FullProbe1 %>% select(FT) %>% mutate(difference_FT = FT - lag(FT))
print (difference_FT)
difference_ST <- SydenhamHill_FullProbe1 %>% select(ST) %>% mutate(difference_ST = ST - lag(ST))
print (difference_ST)

SydenhamHill_FullProbe1 <- cbind(SydenhamHill_FullProbe1, difference_FT, difference_ST)

colnames(SydenhamHill_FullProbe1) <- c("X.", "Date-Time (EST)", "FT", "ST", "FT1", "difference_FT", "ST1", "difference_ST")
SydenhamHill_FullProbe1<- subset(SydenhamHill_FullProbe1, select = -c(FT1, ST1))

#thermal shock 
sum((SydenhamHill_FullProbe1$difference_FT) >= 1 | (SydenhamHill_FullProbe1$difference_FT) <= -1, na.rm = TRUE)
sum((SydenhamHill_FullProbe1$difference_ST) >= 1 | (SydenhamHill_FullProbe1$difference_ST) <= -1, na.rm = TRUE)


#Probe 2
require (dplyr)
#difference FT
difference_FT <- SydenhamHill_FullProbe2 %>% select(FT) %>% mutate(difference_FT = FT - lag(FT))
print (difference_FT)
difference_ST <- SydenhamHill_FullProbe2 %>% select(ST) %>% mutate(difference_ST = ST - lag(ST))
print (difference_ST)

SydenhamHill_FullProbe2 <- cbind(SydenhamHill_FullProbe2, difference_FT, difference_ST)

colnames(SydenhamHill_FullProbe2) <- c("X.", "Date-Time (EST)", "FT", "ST", "FT1", "difference_FT", "FT2", "difference_FT2", "ST1", "difference_ST")
SydenhamHill_FullProbe2<- subset(SydenhamHill_FullProbe2, select = -c(FT1, FT2, difference_FT2, ST1))

#thermal shock 
sum((SydenhamHill_FullProbe2$difference_FT) >= 1 | (SydenhamHill_FullProbe2$difference_FT) <= -1, na.rm = TRUE)
sum((SydenhamHill_FullProbe2$difference_ST) >= 1 | (SydenhamHill_FullProbe2$difference_ST) <= -1, na.rm = TRUE)
