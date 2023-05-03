#Budyko Framework Figures - Watershed Theory Paper - CAMELS-Chem

library(data.table)
library(dplyr)
library(ggplot2)

setwd("C:/Users/Bryn/Desktop/WatershedTheory") #Set working directory

CAMELS <- fread("CAMELS-Chem_WaterQuality_Data.csv")  #Load CAMELS Data

CAMELS <- CAMELS %>% 
  mutate(AET_mmd = p_mean - q_mean_mmd, #Calculate AET (P - Q = AET)
         AET_P = AET_mmd/p_mean, #Calculate Evaporative Index (AET/P)
         PET_P = aridity) #Add column with aridity index renamed as PET/P for consistency

Budyko_BETA1 <- function(PET_P) ((((1/PET_P)^1)+1)^(1/1))^-1
Budyko_BETA2 <- function(PET_P) ((((1/PET_P)^2)+1)^(1/2))^-1
Budyko_BETA3 <- function(PET_P) ((((1/PET_P)^3)+1)^(1/3))^-1

Al <- ggplot(CAMELS, aes(PET_P, AET_P)) + #Plot AET/P versus PET/P
  geom_function(fun = Budyko_BETA1, colour = "red", lwd = 0.75, linetype = 1) + #Beta = 1 curve
  geom_function(fun = Budyko_BETA2, colour = "red", lwd = 1.5, linetype = 1) + #Beta = 2 curve
  geom_function(fun = Budyko_BETA3, colour = "red", lwd = 2, linetype = 1) + #Beta = 3 curve
  geom_point(aes(size = Al), shape = 21, colour = "black", fill = "darkgrey", stroke = 1) + #Set symbol size to mean concentration ("size = Al"), specify symbol color, etc.
  scale_size(name = "Al (mg/l)")+ #Legend title
  labs(x = "PET/P", y= "AET/P") + #Axis labels
  xlim(0, 3.5) + #x-axis limits
  ylim(0, 1.2) + #y-axis limits
  annotate("text", x=0.2, y=1.1, label= "Al", size = 14 , fontface = "bold") + #Add solute label
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18))

Ca <- ggplot(CAMELS, aes(PET_P, AET_P)) + #Plot AET/P versus PET/P
  geom_function(fun = Budyko_BETA1, colour = "red", lwd = 0.75, linetype = 1) + #Beta = 1 curve
  geom_function(fun = Budyko_BETA2, colour = "red", lwd = 1.5, linetype = 1) + #Beta = 2 curve
  geom_function(fun = Budyko_BETA3, colour = "red", lwd = 2, linetype = 1) + #Beta = 3 curve
  geom_point(aes(size = Ca), shape = 21, colour = "black", fill = "darkgrey", stroke = 1) + #Set symbol size to mean concentration ("size = Ca"), specify symbol color, etc.
  scale_size(name = "Ca (mg/l)", range = c(0.2, 8), breaks = c(10, 50, 100, 500))+ #Set breaks and size range manually, also legend title
  labs(x = "PET/P", y= "AET/P") + #Axis labels
  xlim(0, 3.5) + #x-axis limits
  ylim(0, 1.2) + #y-axis limits
  annotate("text", x=0.2, y=1.1, label= "Ca", size = 14, fontface = "bold") + #Add solute label
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=18))