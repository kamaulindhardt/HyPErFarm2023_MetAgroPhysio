#Johannes Wilhelmus Maria Pullens
#2024
#email: jwmp@agro.au.dk
#Figures for Nature paper


# Clear the workspace
rm(list = ls())

# Load required libraries
library(ggplot2) #Version 3.5.1
library(tidyverse) # Version 2.0.0
library(readxl) #Version 1.4.3
library(lme4) #Version 1.1-35.3
library(emmeans) #Version 1.10.2

base_sizeJP=16
# Set working directory to the specific folder


# Load the Yield data from an Excel file
Yield <- readxl::read_excel("Yield_2023.xlsx", sheet = "all")

# Define the plots to exclude from the dataset
N <- c("N1","N2","N3","N4","N5","N6","N25","N26","N27","N28","N29","N30")
S <- c("S31","S32","S33","S34","S35","S36","S55","S56","S57","S58","S59","S60")
Yield <- Yield[-which(Yield$Plot == N),]
Yield <- Yield[-which(Yield$Plot == S),]

# Convert certain columns to appropriate data types
Yield$PV_setup <- as.factor(Yield$PV_setup)
Yield$Location <- as.factor(Yield$Location)
Yield$Product <- as.factor(Yield$Product)
Yield$Dry_Weight_1ha <- as.numeric(Yield$Dry_Weight_1ha)
Yield$Side <- as.factor(Yield$Side)
Yield$Side <- factor(Yield$Side, levels = c("W", "Mid", "E", "S", "M", "N"))

# Fit a linear mixed-effects model
LM <- lmer(Dry_Weight_1ha ~ PV_setup * Location * Product + (1 | Plot), data = Yield)

# Remove 'Lupin' product from the dataset
Yield <- Yield[-which(Yield$Product == "Lupin"),]

# Update the PV_setup column
Yield$PV_setup <- as.character(Yield$PV_setup)
Yield$PV_setup[which(Yield$Location == "Out")] <- "Reference"
Yield$PV_setup <- as.factor(Yield$PV_setup)

# Rename levels of PV_setup

levels(Yield$PV_setup) <- c("Vertical", "Tilted south-oriented", "Open field")
# Fit a linear model for the updated dataset
LM <- lm(Dry_Weight_1ha ~ PV_setup * Product, data = Yield)

# Compute and plot estimated marginal means for the new model
emm <- emmeans(LM, list(pairwise ~ PV_setup * Product), adjust = "tukey")

# Rename columns in the Yield dataset
names(Yield) <- c("PV Setup", "Location", "Product", "Plot", "Dry weight (t/ha)")

# Reorder the Product factor levels
Yield$Product <- factor(Yield$Product, levels = c("Grain", "Straw", "Grass-Clover"))

scaleFUN <- function(x) sprintf("%.1f", x)

# Create a boxplot for the entire dataset
Allplot <- ggplot(data = Yield, aes(x = Product, y = `Dry weight (t/ha)`)) +
  geom_boxplot(aes(fill = `PV Setup`)) + 
  scale_fill_manual(values=c("#F98D06", "#1A91FF", "green"))+
  theme_bw(base_size = base_sizeJP)+theme(legend.position = "bottom")+ scale_y_continuous(limits = c(2,7),labels=scaleFUN)

# Define a data frame for Grass-Clover annotations
grassclover <- data.frame(a = c(2.75, 2.75, 3, 3, 3), b = c(6.4, 6.5, 6.5, 6.5, 6.4))
grassclover_1 <- data.frame(a = c(3, 3, 3.25, 3.25, 3.25), b = c(6.6, 6.7, 6.7, 6.7, 6.6))

# Add a line and annotation to the plot
Allplot <- Allplot + 
  geom_line(data = grassclover, aes(x = a, y = b)) +
  geom_line(data = grassclover_1, aes(x = a, y = b)) + 
  annotate("text", x = c(2.875,3.125), y = c(6.6,6.8), label = "**", size = 7)

# Load the Grass data from the Excel file
Grass <- read_excel("Yield_2023.xlsx", sheet = "Grassclover")
Grass <- subset(Grass, select = c("PV_setup", "Location", "Plot", "Dry_Weight_1ha, 03/07/2023", "Dry_Weight_1ha, 14/08/2023", "Dry_Weight_1ha, 21/09/2023"))

N <- c("N1","N2","N3","N4","N5","N6","N25","N26","N27","N28","N29","N30")
S <- c("S31","S32","S33","S34","S35","S36","S55","S56","S57","S58","S59","S60")
Grass <- Grass[-which(Grass$Plot %in% N),]
Grass <- Grass[-which(Grass$Plot %in% S),]

Grass$PV_setup[which(Grass$Location=="Out")] <- "Reference"
Grass$Location[which(Grass$Location=="Out")] <- "Reference"

# Fit linear models for each cut and compute estimated marginal means
# First cut
LM <- lm(`Dry_Weight_1ha, 03/07/2023` ~ PV_setup * Location, data = Grass)
emmeans(LM, list(pairwise ~ PV_setup * Location), adjust = "tukey")

# Second cut
LM <- lm(`Dry_Weight_1ha, 14/08/2023` ~ PV_setup * Location, data = Grass)
emmeans(LM, list(pairwise ~ PV_setup * Location), adjust = "tukey")

# Third cut
LM <- lm(`Dry_Weight_1ha, 21/09/2023` ~ PV_setup * Location, data = Grass)
emmeans(LM, list(pairwise ~ PV_setup * Location), adjust = "tukey")

# Reload and reformat Grass data for further plotting
Grass <- readxl::read_excel("Yield_2023.xlsx", sheet = "Sheet1")
names(Grass) <- c("PV_setup", "Location", "Product", "Plot", "Dry_Weight_1ha", "Date")
N <- c("N1","N2","N3","N4","N5","N6","N25","N26","N27","N28","N29","N30")
S <- c("S31","S32","S33","S34","S35","S36","S55","S56","S57","S58","S59","S60")
Grass <- Grass[-which(Grass$Plot %in% N),]
Grass <- Grass[-which(Grass$Plot %in% S),]

a <- tibble(Grass)

# Update PV_setup column
a$PV_setup <- as.character(a$PV_setup)
a$PV_setup[which(a$Location == "Out")] <- "Reference"
a$PV_setup <- as.factor(a$PV_setup)

# Rename levels of PV_setup
levels(a$PV_setup) <- c("Vertical", "Tilted south-oriented", "Open field")
a$Date <- factor(a$Date)

# Create subsets for each cut date
a1 <- subset(a, Date == "2023-07-03")
a2 <- subset(a, Date == "2023-08-14")
a3 <- subset(a, Date == "2023-09-21")
levels(a1$PV_setup) <- c("V", "T","O")
levels(a2$PV_setup) <- c("V", "T","O")
levels(a3$PV_setup) <- c("V", "T","O")

# Plot for the first cut
grass_plot1 <- ggplot(data = a1, aes(x = PV_setup, y = Dry_Weight_1ha)) + 
  geom_boxplot(aes(fill = `PV_setup`), show.legend = FALSE) + 
  scale_fill_manual(values=c("#F98D06", "#1A91FF", "green"))+ 
  theme_bw(base_size = base_sizeJP)  +
  labs(y = "Dry weight (t/ha)", x = "") + scale_x_discrete(labels = NULL) + 
  ylim(1.5, 2.8) + 
  ggtitle("2023-07-03") + 
  theme(plot.title = element_text(hjust = 0.5))

# Plot for the second cut
grass_plot2 <- ggplot(data = a2, aes(x = PV_setup, y = Dry_Weight_1ha)) + 
  geom_boxplot(aes(fill = `PV_setup`), show.legend = FALSE) + 
  scale_fill_manual(values=c("#F98D06", "#1A91FF", "green"))+  
  theme_bw(base_size = base_sizeJP)  +
  labs(y = "Dry weight (t/ha)" , x = "") + scale_x_discrete(labels = NULL) +
  ylim(1.5, 2.8) + 
  ggtitle("2023-08-14") + 
  theme(plot.title = element_text(hjust = 0.5))

# Plot for the third cut
grass_plot3 <- ggplot(data = a3, aes(x = PV_setup, y = Dry_Weight_1ha)) + 
  geom_boxplot(aes(fill = `PV_setup`), show.legend = FALSE) + 
  scale_fill_manual(values=c("#F98D06", "#1A91FF", "green"))+ 
  theme_bw(base_size = base_sizeJP) +
  labs(y = "Dry weight (t/ha)", x = "PV Setup") + 
  ylim(1.5, 2.8) + 
  ggtitle("2023-09-21") + 
  theme(plot.title = element_text(hjust = 0.5))

# Add a line and annotation to the second cut plot
cut2 <- data.frame(a = c(1, 1, 2, 2, 2), b = c(2.45, 2.55, 2.55, 2.55, 2.45))
cut2_1 <- data.frame(a = c(2, 2, 3, 3, 3), b = c(2.6, 2.7, 2.7, 2.7, 2.6))
grass_plot2 <- grass_plot2 + 
  geom_line(data = cut2, aes(x = a, y = b)) + 
  geom_line(data = cut2_1, aes(x = a, y = b)) +
  annotate("text", x = c(1.5,2.5), y = c(2.6,2.75), label = "*", size = 7)

# Arrange the three grass plots vertically
right <- ggpubr::ggarrange(grass_plot1, grass_plot2, grass_plot3,ncol = 1, legend = FALSE,heights = 100)

# Combine the Allplot with the arranged grass plots
complete_graph <- ggpubr::ggarrange(Allplot, right, labels = c("a", "b"),widths = c(3.5,1.5), common.legend = FALSE,font.label = c(size=base_sizeJP+1))
complete_graph

# Save the combined plot to a PDF file
pdf("arrangedplot_corrected04092024.pdf", width = 12, height = 7)
print(complete_graph)
dev.off()
#
