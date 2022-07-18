install.packages("tidyverse")
install.packages("dplyr")
install.packages("purrr")

library (tidyverse)
library(dplyr)
library(readr)
library(purrr)

MountviewFalls_5.1 <- read.csv("MountviewFalls_Probe5.1.csv")
MountviewFalls_5.2 <- read.csv("MountviewFalls_Probe5.2.csv")
MountviewFalls_5.3 <- read.csv("MountviewFalls_Probe5.3.csv")
MountviewFalls_5.4 <- read.csv("MountviewFalls_Probe5.4.csv")
MountviewFalls_5.5 <- read.csv("MountviewFalls_Probe5.5.csv")
MountviewFalls_5.6 <- read.csv("MountviewFalls_Probe5.6.csv")
MountviewFalls_5.7 <- read.csv("MountviewFalls_Probe5.7.csv")
MountviewFalls_5.8 <- read.csv("MountviewFalls_Probe5.8.csv")
MountviewFalls_5.9 <- read.csv("MountviewFalls_Probe5.9.csv")
MountviewFalls_5.10 <- read.csv("MountviewFalls_Probe5.10.csv")
MountviewFalls_5.11 <- read.csv("MountviewFalls_Probe5.11.csv")
MountviewFalls_5.12 <- read.csv("MountviewFalls_Probe5.12.csv")
MountviewFalls_5.13 <- read.csv("MountviewFalls_Probe5.13.csv")
MountviewFalls_5.14 <- read.csv("MountviewFalls_Probe5.14.csv")
MountviewFalls_5.15 <- read.csv("MountviewFalls_Probe5.15.csv")
MountviewFalls_5.16 <- read.csv("MountviewFalls_Probe5.16.csv")
MountviewFalls_5.17 <- read.csv("MountviewFalls_Probe5.17.csv")

MountviewFalls_6.1 <- read.csv("MountviewFalls_Probe6.1.csv")
MountviewFalls_6.2 <- read.csv("MountviewFalls_Probe6.2.csv")
MountviewFalls_6.3 <- read.csv("MountviewFalls_Probe6.3.csv")
MountviewFalls_6.4 <- read.csv("MountviewFalls_Probe6.4.csv")
MountviewFalls_6.5 <- read.csv("MountviewFalls_Probe6.5.csv")
MountviewFalls_6.6 <- read.csv("MountviewFalls_Probe6.6.csv")
MountviewFalls_6.7 <- read.csv("MountviewFalls_Probe6.7.csv")
MountviewFalls_6.8 <- read.csv("MountviewFalls_Probe6.8.csv")
MountviewFalls_6.9 <- read.csv("MountviewFalls_Probe6.9.csv")
MountviewFalls_6.10 <- read.csv("MountviewFalls_Probe6.10.csv")
MountviewFalls_6.11 <- read.csv("MountviewFalls_Probe6.11.csv")
MountviewFalls_6.12 <- read.csv("MountviewFalls_Probe6.12.csv")
MountviewFalls_6.13 <- read.csv("MountviewFalls_Probe6.13.csv")
MountviewFalls_6.14 <- read.csv("MountviewFalls_Probe6.14.csv")
MountviewFalls_6.15 <- read.csv("MountviewFalls_Probe6.15.csv")
MountviewFalls_6.16 <- read.csv("MountviewFalls_Probe6.16.csv")
MountviewFalls_6.17 <- read.csv("MountviewFalls_Probe6.17.csv")



names(MountviewFalls_5.1) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)", "y", "z")
MountviewFalls_5.1 <- subset(MountviewFalls_5.1, select = -c(y,z))
names(MountviewFalls_5.2) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)", "y", "z")
MountviewFalls_5.2 <- subset(MountviewFalls_5.2, select = -c(y,z))
names(MountviewFalls_5.3) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)", "y", "z")
MountviewFalls_5.3 <- subset(MountviewFalls_5.3, select = -c(y,z))
names(MountviewFalls_5.4) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.5) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.6) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.7) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.8) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.9) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.10) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.11) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.12) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.13) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.14) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.15) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.16) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_5.17) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")

names(MountviewFalls_6.1) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)", "y", "z")
MountviewFalls_6.1 <- subset(MountviewFalls_6.1, select = -c(y,z))
names(MountviewFalls_6.2) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)", "y", "z")
MountviewFalls_6.2 <- subset(MountviewFalls_6.2, select = -c(y,z))
names(MountviewFalls_6.3) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)", "y", "z")
MountviewFalls_6.3 <- subset(MountviewFalls_6.3, select = -c(y,z))
names(MountviewFalls_6.4) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.5) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.6) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.7) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.8) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.9) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.10) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.11) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.12) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.13) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.14) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.15) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.16) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(MountviewFalls_6.17) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")



MountviewFalls_FullProbe5 <- rbind(MountviewFalls_5.1, MountviewFalls_5.2, MountviewFalls_5.3, MountviewFalls_5.4, 
                                   MountviewFalls_5.5, MountviewFalls_5.6, MountviewFalls_5.7, MountviewFalls_5.8, 
                                   MountviewFalls_5.9, MountviewFalls_5.10, MountviewFalls_5.11, MountviewFalls_5.12,
                                   MountviewFalls_5.13, MountviewFalls_5.14, MountviewFalls_5.15, MountviewFalls_5.16,
                                   MountviewFalls_5.17)

MountviewFalls_FullProbe6 <- rbind(MountviewFalls_6.1, MountviewFalls_6.2, MountviewFalls_6.3, MountviewFalls_6.4, 
                                   MountviewFalls_6.5, MountviewFalls_6.6, MountviewFalls_6.7, MountviewFalls_6.8, 
                                   MountviewFalls_6.9, MountviewFalls_6.10, MountviewFalls_6.11, MountviewFalls_6.12,
                                   MountviewFalls_6.13, MountviewFalls_6.14, MountviewFalls_6.15, MountviewFalls_6.16,
                                   MountviewFalls_6.17)


