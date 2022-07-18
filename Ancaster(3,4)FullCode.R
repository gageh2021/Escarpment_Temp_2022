install.packages("tidyverse")
install.packages("dplyr")
install.packages("purrr")

library (tidyverse)
library(dplyr)
library(readr)
library(purrr)

Ancaster_3.1 <- read.csv("Ancaster_Probe3.1.csv")
Ancaster_3.2 <- read.csv("Ancaster_Probe3.2.csv")
Ancaster_3.3 <- read.csv("Ancaster_Probe3.3.csv")
Ancaster_3.4 <- read.csv("Ancaster_Probe3.4.csv")
Ancaster_3.5 <- read.csv("Ancaster_Probe3.5.csv")
Ancaster_3.6 <- read.csv("Ancaster_Probe3.6.csv")
Ancaster_3.7 <- read.csv("Ancaster_Probe3.7.csv")
Ancaster_3.8 <- read.csv("Ancaster_Probe3.8.csv")
Ancaster_3.9 <- read.csv("Ancaster_Probe3.9.csv")

Ancaster_4.1 <- read.csv("Ancaster_Probe4.1.csv")
Ancaster_4.2 <- read.csv("Ancaster_Probe4.1.csv")
Ancaster_4.3 <- read.csv("Ancaster_Probe4.1.csv")
Ancaster_4.4 <- read.csv("Ancaster_Probe4.1.csv")
Ancaster_4.5 <- read.csv("Ancaster_Probe4.1.csv")
Ancaster_4.6 <- read.csv("Ancaster_Probe4.1.csv")
Ancaster_4.7 <- read.csv("Ancaster_Probe4.1.csv")
Ancaster_4.8 <- read.csv("Ancaster_Probe4.1.csv")
Ancaster_4.9 <- read.csv("Ancaster_Probe4.1.csv")
Ancaster_4.10 <- read.csv("Ancaster_Probe4.1.csv")
Ancaster_4.11 <- read.csv("Ancaster_Probe4.1.csv")





names(Ancaster_3.1) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_3.2) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_3.3) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_3.4) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_3.5) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_3.6) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_3.7) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_3.8) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_3.9) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")

names(Ancaster_4.1) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_4.2) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_4.3) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_4.4) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_4.5) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_4.6) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_4.7) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_4.8) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_4.9) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_4.10) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")
names(Ancaster_4.11) <- c("X.", "Date-Time (EST)", "Ch: 1-Temperature (°C)", "Ch: 2-Temperature (°C)")


Ancaster_FullProbe3 <- rbind(Ancaster_3.1, Ancaster_3.2, Ancaster_3.3, Ancaster_3.4, 
                            Ancaster_3.5, Ancaster_3.6, Ancaster_3.7, Ancaster_3.8,
                            Ancaster_3.9)

Ancaster_FullProbe4 <- rbind(Ancaster_4.1, Ancaster_4.2)

