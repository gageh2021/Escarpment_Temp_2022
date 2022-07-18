install.packages("tidyverse")
install.packages("dplyr")
install.packages("purrr")

library (tidyverse)
library(dplyr)
library(readr)
library(purrr)

Gasport_1.1 <- read.csv("Gasport_Probe1.1.csv")
Gasport_1.2 <- read.csv("Gasport_Probe1.2.csv")
Gasport_1.3 <- read.csv("Gasport_Probe1.3.csv")
Gasport_1.4 <- read.csv("Gasport_Probe1.4.csv")
Gasport_1.5 <- read.csv("Gasport_Probe1.5.csv")
Gasport_1.6 <- read.csv("Gasport_Probe1.6.csv")
Gasport_1.7 <- read.csv("Gasport_Probe1.7.csv")
Gasport_1.8 <- read.csv("Gasport_Probe1.8.csv")
Gasport_1.9 <- read.csv("Gasport_Probe1.9.csv")
Gasport_1.10 <- read.csv("Gasport_Probe1.10.csv")
Gasport_1.11 <- read.csv("Gasport_Probe1.11.csv")
Gasport_1.12 <- read.csv("Gasport_Probe1.12.csv")
Gasport_1.13 <- read.csv("Gasport_Probe1.13.csv")
Gasport_1.14 <- read.csv("Gasport_Probe1.14.csv")
Gasport_1.15 <- read.csv("Gasport_Probe1.15.csv")
Gasport_1.16 <- read.csv("Gasport_Probe1.16.csv")
Gasport_1.17 <- read.csv("Gasport_Probe1.17.csv")
Gasport_1.18 <- read.csv("Gasport_Probe1.18.csv")

Gasport_2.1 <- read.csv("Gasport_Probe2.1.csv")
Gasport_2.2 <- read.csv("Gasport_Probe2.2.csv")



names(Gasport_1.1) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)", "y", "z")
Gasport_1.1 <- subset(Gasport_1.1, select = -c(y,z))
names(Gasport_1.2) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)", "y", "z")
Gasport_1.2 <- subset(Gasport_1.2, select = -c(y,z))
names(Gasport_1.3) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)", "y", "z")
Gasport_1.3 <- subset(Gasport_1.3, select = -c(y,z))
names(Gasport_1.4) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)", "y", "z")
Gasport_1.4 <- subset(Gasport_1.4, select = -c(y,z))
names(Gasport_1.5) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)", "y", "z")
Gasport_1.5 <- subset(Gasport_1.5, select = -c(y,z))
names(Gasport_1.6) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.7) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.8) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.9) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.10) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.11) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.12) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.13) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.14) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.15) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.16) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.17) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_1.18) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")

names(Gasport_2.1) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Gasport_2.2) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")


Gasport_FullProbe1 <- rbind(Gasport_1.1, Gasport_1.2, Gasport_1.3, Gasport_1.4, 
                                   Gasport_1.5, Gasport_1.6, Gasport_1.7, Gasport_1.8, 
                                   Gasport_1.9, Gasport_1.10, Gasport_1.11, Gasport_1.12,
                                   Gasport_1.13, Gasport_1.14, Gasport_1.15, Gasport_1.16,
                                   Gasport_1.17, Gasport_1.18)

Gasport_FullProbe2 <- rbind(Gasport_2.1, Gasport_2.2)

