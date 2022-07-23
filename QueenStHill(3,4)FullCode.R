install.packages("tidyverse")
install.packages("dplyr")
install.packages("purrr")

library (tidyverse)
library(dplyr)
library(readr)
library(purrr)

QueenStHill_3.1 <- read.csv("QueenStHill_Probe3.1.csv")
QueenStHill_3.2 <- read.csv("QueenStHill_Probe3.2.csv")
QueenStHill_3.3 <- read.csv("QueenStHill_Probe3.3.csv")
QueenStHill_3.4 <- read.csv("QueenStHill_Probe3.4.csv")
QueenStHill_3.5 <- read.csv("QueenStHill_Probe3.5.csv")
QueenStHill_3.6 <- read.csv("QueenStHill_Probe3.6.csv")
QueenStHill_3.7 <- read.csv("QueenStHill_Probe3.7.csv")
QueenStHill_3.8 <- read.csv("QueenStHill_Probe3.8.csv")
QueenStHill_3.9 <- read.csv("QueenStHill_Probe3.9.csv")
QueenStHill_3.10 <- read.csv("QueenStHill_Probe3.10.csv")
QueenStHill_3.11 <- read.csv("QueenStHill_Probe3.11.csv")
QueenStHill_3.12 <- read.csv("QueenStHill_Probe3.12.csv")
QueenStHill_3.13 <- read.csv("QueenStHill_Probe3.13.csv")
QueenStHill_3.14 <- read.csv("QueenStHill_Probe3.14.csv")
QueenStHill_3.15 <- read.csv("QueenStHill_Probe3.15.csv")
QueenStHill_3.16 <- read.csv("QueenStHill_Probe3.16.csv")
QueenStHill_3.17 <- read.csv("QueenStHill_Probe3.17.csv")

QueenStHill_4.1 <- read.csv("QueenStHill_Probe4.1.csv")
QueenStHill_4.2 <- read.csv("QueenStHill_Probe4.2.csv")
QueenStHill_4.3 <- read.csv("QueenStHill_Probe4.3.csv")
QueenStHill_4.4 <- read.csv("QueenStHill_Probe4.4.csv")
QueenStHill_4.5 <- read.csv("QueenStHill_Probe4.5.csv")
QueenStHill_4.6 <- read.csv("QueenStHill_Probe4.6.csv")
QueenStHill_4.7 <- read.csv("QueenStHill_Probe4.7.csv")
QueenStHill_4.8 <- read.csv("QueenStHill_Probe4.8.csv")
QueenStHill_4.9 <- read.csv("QueenStHill_Probe4.9.csv")
QueenStHill_4.10 <- read.csv("QueenStHill_Probe4.10.csv")
QueenStHill_4.11 <- read.csv("QueenStHill_Probe4.11.csv")
QueenStHill_4.12 <- read.csv("QueenStHill_Probe4.12.csv")
QueenStHill_4.13 <- read.csv("QueenStHill_Probe4.13.csv")
QueenStHill_4.14 <- read.csv("QueenStHill_Probe4.14.csv")
QueenStHill_4.15 <- read.csv("QueenStHill_Probe4.15.csv")
QueenStHill_4.16 <- read.csv("QueenStHill_Probe4.16.csv")
QueenStHill_4.17 <- read.csv("QueenStHill_Probe4.17.csv")

names(QueenStHill_3.1) <- c("X.", "Date-Time (EST)", "FT", "ST")
    #QueenStHill_3.1 <- subset(QueenStHill_3.1, select = -c(y,z))
names(QueenStHill_3.2) <- c("X.", "Date-Time (EST)", "FT", "ST")
    #QueenStHill_3.2 <- subset(QueenStHill_3.2, select = -c(y,z))
names(QueenStHill_3.3) <- c("X.", "Date-Time (EST)", "FT", "ST")
    #QueenStHill_3.3 <- subset(QueenStHill_3.3, select = -c(y,z))
names(QueenStHill_3.4) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.5) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.6) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.7) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.8) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.9) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.10) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.11) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.12) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.13) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.14) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.15) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.16) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_3.17) <- c("X.", "Date-Time (EST)", "FT", "ST")

names(QueenStHill_4.1) <- c("X.", "Date-Time (EST)", "FT", "ST")
  #QueenStHill_4.1 <- subset(QueenStHill_4.1, select = -c(y,z))
names(QueenStHill_4.2) <- c("X.", "Date-Time (EST)", "FT", "ST")
  #QueenStHill_4.2 <- subset(QueenStHill_4.2, select = -c(y,z))
names(QueenStHill_4.3) <- c("X.", "Date-Time (EST)", "FT", "ST")
  #QueenStHill_4.3 <- subset(QueenStHill_4.3, select = -c(y,z))
names(QueenStHill_4.4) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.5) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.6) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.7) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.8) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.9) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.10) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.11) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.12) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.13) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.14) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.15) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.16) <- c("X.", "Date-Time (EST)", "FT", "ST")
names(QueenStHill_4.17) <- c("X.", "Date-Time (EST)", "FT", "ST")

QueenStHill_FullProbe3 <- rbind(QueenStHill_3.1, QueenStHill_3.2, QueenStHill_3.3, QueenStHill_3.4, 
                                QueenStHill_3.5, QueenStHill_3.6, QueenStHill_3.7, QueenStHill_3.8, 
                                QueenStHill_3.9, QueenStHill_3.10, QueenStHill_3.11, QueenStHill_3.12,
                                QueenStHill_3.13, QueenStHill_3.14, QueenStHill_3.15, QueenStHill_3.16,
                                QueenStHill_3.17)

QueenStHill_FullProbe4 <- rbind(QueenStHill_4.1, QueenStHill_4.2, QueenStHill_4.3, QueenStHill_4.4, 
                                QueenStHill_4.5, QueenStHill_4.6, QueenStHill_4.7, QueenStHill_4.8, 
                                QueenStHill_4.9, QueenStHill_4.10, QueenStHill_4.11, QueenStHill_4.12,
                                QueenStHill_4.13, QueenStHill_4.14, QueenStHill_4.15, QueenStHill_4.16,
                                QueenStHill_4.17)

#Probe 3
require (dplyr)
#difference ST
difference_ST <- QueenStHill_FullProbe3 %>% select(ST) %>% mutate(difference_ST = ST - lag(ST))
print (difference_ST)
#difference_FT
difference_FT <- QueenStHill_FullProbe3 %>% select(FT) %>% mutate(difference_FT = FT - lag(FT))
print (difference_FT)

QueenStHill_FullProbe3 <- cbind(QueenStHill_FullProbe3, difference_ST, difference_FT)

colnames(QueenStHill_FullProbe3) <- c("X.", "Date-Time (EST)", "ST", "FT", "ST1", "difference_ST", "FT1", "difference_FT")
QueenStHill_FullProbe3<- subset(QueenStHill_FullProbe3, select = -c(ST1,FT1))

#thermal shock 
sum((QueenStHill_FullProbe3$difference_FT) >= 1 | (QueenStHill_FullProbe3$difference_FT) <= -1, na.rm = TRUE)
sum((QueenStHill_FullProbe3$difference_ST) >= 1 | (QueenStHill_FullProbe3$difference_ST) <= -1, na.rm = TRUE)


#Probe 4
require (dplyr)
#difference_FT
difference_FT <- QueenStHill_FullProbe4 %>% select(FT) %>% mutate(difference_FT = FT - lag(FT))
print (difference_FT)
#difference ST
difference_ST <- QueenStHill_FullProbe4 %>% select(ST) %>% mutate(difference_ST = ST - lag(ST))
print (difference_ST)


QueenStHill_FullProbe4 <- cbind(QueenStHill_FullProbe4, difference_ST, difference_FT)

colnames(QueenStHill_FullProbe4) <- c("X.", "Date-Time (EST)", "FT", "ST", "ST1", "difference_ST", "FT1", "difference_FT")
QueenStHill_FullProbe4<- subset(QueenStHill_FullProbe4, select = -c(ST1,FT1))

#thermal shock 
sum((QueenStHill_FullProbe4$difference_FT) >= 1 | (QueenStHill_FullProbe4$difference_FT) <= -1, na.rm = TRUE)
sum((QueenStHill_FullProbe4$difference_ST) >= 1 | (QueenStHill_FullProbe4$difference_ST) <= -1, na.rm = TRUE)
