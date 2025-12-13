#NEEDED PACKAGES
library(nmbu)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra) 
library(grid)
library(lattice)
library(plotly)
library(dabestr)
library(reshape2)
library(rstatix)
library(ggtext)
library(viridis)
library(ggsignif)

#PREPARE DATASET
SDM <- read.table('clipboard', strip.white=FALSE, sep='\t', na.strings='NA', header=TRUE, dec='.') #importing from excel
str(SDM) #Look at
summary(SDM) #Summarize

# Making wanted order of types
SDM$Type <- factor(SDM$Type, levels = c("Mechanical","Lesion", "Pharmacologic", "Gene Editing", "Optogenetic", "Chemogenetic"))

#Making wanted order of Region
SDM$Region <- factor(SDM$Region, levels = c("Forebrain", "Brainstem", "N/A"))

#Making wanted order of Overarea
SDM$Overarea <- factor(SDM$Overarea, levels = c("Cortex", "Hypothalamus", "Thalamus", "Basal Forebrain", "Basal Ganglia", "Midbrain", "Pons", "Medulla", "N/A" ))


#RESULTS 4.1
# TABLE 2
#Amount of each experiment
summary_type <- SDM %>%
  group_by(Type) %>%
  summarise(Count = n(), .groups = 'drop')
print(summary_type)

# FIGURE 23
# MAKE AREA GRAPH FOR TYPE AND YEAR
summary_yeartype <- SDM %>%
  group_by(Year, Type) %>%
  summarise(Count = n(), .groups = 'drop')

# Make areagraph
area <- ggplot(summary_yeartype, aes(x = Year, y = Count, fill = Type)) +
  geom_area(position = "stack", alpha = 0.6) +  # Stacked area
  labs(title = "How method Usage Changes Yearly",
       x = "Year",
       y = "Experiments (n)") +
  theme_minimal() +
  scale_fill_brewer(name = "Method", palette ="Set1") +
  theme(plot.title = element_text(hjust = 0.5, size = 13))  # Move title to centre

#MAKE PIEGRAPH
# Count method types
summary_type <- SDM %>%
  group_by(Type) %>%
  summarise(Count = n(), .groups = 'drop')

# Make percentages with method type
summary_type$percentage <- round(summary_type$Count / sum(summary_type$Count) * 100, 1)

pie_method <- ggplot(summary_type, aes(x = "", y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 2, alpha = 0.6) +
  coord_polar("y") +
  geom_text(aes(x=, 1.4, label = paste(percentage, "%")), position = position_stack(vjust = 0.5), color = "black", size=3) +  # Show method inside pie chart
  labs(title = "All methods") +
  theme_void() +
  scale_fill_brewer(palette ="Set1") +
  theme(plot.title = element_text(hjust = 0.5, size = 13), legend.position = "none")  # Title centered, remove legend

#MAKE LINEGRAPH YEAR 2010 to 2025
#Normalize and make to percentage
percentage_filtered <- SDM %>%
  group_by(Year, Type) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Type) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ungroup()

#Filtered data from 2010 to 2025
#Filter from year
filtered_yeartype <- percentage_filtered %>%
  filter(Year >= 2010 & Year <= 2025)

#Linegraph
line <- ggplot(filtered_yeartype, aes(x = Year, y = Percentage, color = Type, group= Type)) +
  geom_line(size = 1) +  # Lag linjer
  geom_point(size = 2) +  # Legg til punkter på linjene
  labs(title = "Percentage of Methods Per Year (2010-2025)",
       x = "Year",
       y = "Experiments (%)") +
  scale_x_continuous(breaks=seq(2010,2025, by = 2)) +
  theme_minimal() +
  scale_color_brewer(palette ="Set1") +
  theme(plot.title = element_text(hjust = 0.5, size = 13), legend.position = "none")  # Move title to centre, remove legend

#Make layout for figure grid
layout <- rbind(c(1,1,1),
	        c(2,2,3))

grid.arrange(area, line, pie_method, ncol=1, layout_matrix=layout)


#FIGURE 24
percentage_subtypes <- SDM %>%
  group_by(Type, Subtype) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  filter(Count > 0) %>%  # Filtrer ut subtyper med Count 0
  mutate(TotalCount = sum(Count)) %>%
  mutate(Percentage = (Count / TotalCount) * 100) %>%
  select (Type, Subtype, Percentage)

# Define one palette per Method using your color families
pal_expand <- function(name, n) {
  base <- RColorBrewer::brewer.pal(max(3, min(9, n)), name)
  if (n <= length(base)) base[seq_len(n)] else grDevices::colorRampPalette(base)(n)
}

method_palettes <- list(
  Mechanical    = function(n) pal_expand("Reds",    n),
  Lesion        = function(n) pal_expand("Blues",   n),
  Pharmacologic = function(n) pal_expand("Greens",  n),   # handles 12+
  "Gene Editing" = function(n) pal_expand("Purples", n),
  Optogenetic   = function(n) pal_expand("Oranges", n),
  Chemogenetic  = function(n) pal_expand("YlOrBr",  n))

subs_by_type <- split(percentage_subtypes$Subtype, percentage_subtypes$Type)

# For each Type, generate n colors and name them "Type::Subtype"
color_list <- lapply(names(subs_by_type), function(ty) {
  subs <- sort(unique(subs_by_type[[ty]]))
  n <- length(subs)
  pal_fun <- method_palettes[[ty]]
  if (is.null(pal_fun)) pal_fun <- fallback_pal
  cols <- rev(pal_fun(n))
  setNames(cols, paste(ty, subs, sep = "::"))})

color_map <- unlist(color_list)

#Stacked bargraph with name of submethods inside
ggplot(percentage_subtypes, aes(x = Type, y = Percentage, fill = interaction(Type, Subtype, sep = "::"))) +
  geom_bar(stat = "identity", alpha = 0.8) +  # Stacked bargraph
  geom_text(aes(label = Subtype), position = position_stack(vjust = 0.5), color = "white", size = 3.5) +  # Name of subtype inside stacked bargraph
  geom_text(aes(label = Subtype), position = position_stack(vjust = 0.5), color = "black", size = 3.5) +
  scale_fill_manual(
    values = color_map) +
  labs(title = "The Usage of Sleep Deprivation Methods",
       x = "Method",
       y = "Experiments (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),	# Put title in centre
  legend.position="none")  

#FIGURE 25
# SUMMARIZE NUMBER OF METHODS PER RODENT TYPE
summary_table <- SDM %>%
  group_by(Rodent, Type) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = Type, values_from = Count, values_fill = list(Count = 0))

# Reformat summary_table to a long format
long_table <- summary_table %>%
  pivot_longer(cols = -Rodent, names_to = "Type", values_to = "Count")

summary_table <- SDM %>%
  group_by(Rodent, Type) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count/ sum(Count)) *100)
summary_table

# Make a stacked graph
ggplot(summary_table, aes(x = Rodent, y = Percentage, fill = Type)) +
  geom_bar(stat = "identity", alpha=0.6) +  # Using stat = "identity" to show the quanitity
  labs(title = "Amount of Experiments on Mice vs. Rats",
       x = "",
       y = "Experiments (%)") +
  theme_minimal() +
  scale_fill_brewer(name = "Method", palette ="Set1") +
  theme(plot.title = element_text(hjust = 0.5))  # Centre the title

#FIGURE 26
#WHICH AREAS ARE TARGETED FOR WHICH TYPE OF SLEEP?
experiment_summary <- SDM %>%
  group_by(Sleep) %>%
  summarise(Count = n(), .groups = 'drop') %>%  # Count number of experiments
  mutate(Percentage= (Count/sum(Count)) *100)

plot_per <- ggplot(experiment_summary, aes(x = Sleep, y = Percentage, fill=Sleep)) +
  geom_bar(stat = "identity", alpha = 0.6, position = position_dodge()) +  # Place bars next to eachother
  labs(x = "Targeted sleep",
       y = "Experiments (%)",
       title = "a)") +
  scale_fill_brewer(palette = "Set1") +  # Choose colour palette
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")  # Rotate x-axis

#Methodology piecharts
sleep_data <- SDM %>%
  filter(Sleep %in% c("REM", "NREM", "Both"))

#BOTH PIE
# Filter only rows that target both sleep
both_data <- sleep_data %>%
  filter(Sleep == "Both")

both_summary <- both_data %>%
  group_by(Type) %>%
  summarise(Count = n(), .groups = 'drop')  # Count number of methods that only target both sleep

both_summary$percentage <- round(both_summary$Count / sum(both_summary$Count) * 100, 1)

# Make pie chart for Both sleep
both_pie <- ggplot(both_summary, aes(x = "", y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 2, alpha=0.6) +
  coord_polar("y") +
  geom_text(aes(x=, 1.4, label = paste(percentage, "%")), position = position_stack(vjust = 0.5), color = "black", size=3) +  # Show percentage inside pie chart
  labs(title = "b) Both sleep states") +
  theme_void() +
  scale_fill_brewer(palette ="Set1") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))  # Hide legend

#NREM PIE
# Filter only rows that target NREM sleep
NREM_data <- sleep_data %>%
  filter(Sleep == "NREM")

NREM_summary <- NREM_data %>%
  group_by(Type) %>%
  summarise(Count = n(), .groups = 'drop')  # Count number of methods that only target NREM sleep

NREM_summary$percentage <- round(NREM_summary$Count / sum(NREM_summary$Count) * 100, 1)

# Make pie chart for NREM sleep
NREM_pie <- ggplot(NREM_summary, aes(x = "", y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 2, alpha=0.6) +
  coord_polar("y") +
  geom_text(aes(x=, 1.4, label = ifelse(Type %in% c("Mechanical", "Chemogenetic"), "", paste(percentage, "%"))), position = position_stack(vjust = 0.5), color = "black", size=3) +  # Show percentage inside pie chart
  labs(title = "NREM sleep") +
  theme_void() +
  scale_fill_brewer(palette ="Set1") +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))  # Hide legend

#REM PIE
# Filter only rows that target REM sleep
rem_data <- sleep_data %>%
  filter(Sleep == "REM")

rem_summary <- rem_data %>%
  group_by(Type) %>%
  summarise(Count = n(), .groups = 'drop')  # Count number of areas that only target REM sleep

rem_summary$percentage <- round(rem_summary$Count / sum(rem_summary$Count) * 100, 1)

# Make pie chart for REM sleep
REM_pie <- ggplot(rem_summary, aes(x = "", y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 2, alpha=0.6) +
  coord_polar("y") +
  geom_text(aes(x=, 1.4, label = paste(percentage, "%")), position = position_stack(vjust = 0.5), color = "black", size=3) +  # Show brain area inside pie chart
  labs(title = "REM sleep") +
  theme_void() +
  scale_fill_brewer(name = "Method", palette ="Set1") +
  theme(plot.title = element_text(hjust = 0.5))

#Making grid
lay_pies <- rbind(c(1,1,1,1,1,1,1),
	     c(1,1,1,1,1,1,1),
             c(2,2,3,3,4,4,4))

#making figure grid
grid.arrange(plot_per, both_pie, NREM_pie, REM_pie, ncol=2, layout_matrix=lay_pies)


# GRID FOR RESULTS 4.2 - 4.7
layout <- rbind(c(1),
		c(2))


# RESULTS 4.2
# FIGURE 26: Line graph and pie chart for Mechanical methods
#Filter out all lines with type mechanical
filtered_mech <- SDM %>%
  filter(Type == "Mechanical")

total_all_by_year <- SDM %>%        
  group_by(Year) %>%
  summarise(N_all = n(), .groups = "drop")

pct_field_mech <- filtered_mech %>%
  group_by(Year, Subtype) %>%
  summarise(Count = n(), .groups = "drop") %>%
  left_join(total_all_by_year, by = "Year") %>%
  mutate(percentage_field = 100 * Count / N_all) %>%
  ungroup()

#count subtype
mech_summary <- filtered_mech %>%
  group_by(Subtype) %>%
  summarise(Count = n(), .groups = 'drop')  # Count number of subtypes that is used under mechanical

#make percentage to add to pie chart
mech_summary$percentage <- round(mech_summary$Count / sum(mech_summary$Count) * 100, 1)

#pie chart
mech_pie <- ggplot(mech_summary, aes(x = "", y = Count, fill = Subtype)) +
  geom_bar(stat = "identity", width = 2, alpha = 0.6) +
  coord_polar("y") +
  geom_text(aes(label = paste(percentage, "%")), position = position_stack(vjust = 0.5), color = "black", size=3) +  # Show brain area inside pie chart
  scale_fill_brewer(name = "Submethod", palette ="Set1") +
  labs(title = "Total Mechanical Method Distribution") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


# LINE GRAPH
mech_line <- ggplot(pct_field_mech, aes(x = Year, y = percentage_field, color = Subtype, group = Subtype)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_brewer(palette ="Set1") +
  labs(title = "Mechanical Method Field Distribution per Year",
       x = "Year",
       y = "Mechanical Experiments of Yearly Total (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Make grid
grid.arrange(mech_pie, mech_line, ncol=2, layout_matrix=layout)


# RESULTS 4.3
#Filter all lesion experiments
filtered_lesion <- SDM %>%
  filter(Type == "Lesion")

pct_field_lesion <- filtered_lesion %>%
  group_by(Year, Subtype) %>%
  summarise(Count = n(), .groups = "drop") %>%
  left_join(total_all_by_year, by = "Year") %>%
  mutate(percentage_field = 100 * Count / N_all) %>%
  ungroup()

#count subtype
lesion_summary <- filtered_lesion %>%
  group_by(Subtype) %>%
  summarise(Count = n(), .groups = 'drop')  %>% # Count number of subtypes that is used under lesions
  mutate(Subtype = factor(Subtype))

#make percentage to add to pie chart
lesion_summary$percentage <- round(lesion_summary$Count / sum(lesion_summary$Count) * 100, 1)

lesion_pie <- ggplot(lesion_summary, aes(x = "", y = Count, fill = Subtype)) +
  geom_bar(stat = "identity", width = 2, alpha = 0.6) +
  coord_polar("y") +
  geom_text(aes(x=, 1.3, label = paste(percentage, "%")), position = position_stack(vjust = 0.5), color = "black", size=3) +  # Show percentage in pie
  labs(title = "Total Brain Lesion Distribution") +
  scale_fill_brewer(name = "Submethod", palette ="Set1") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

#linegraph
line_lesion <- ggplot(pct_field_lesion, aes(x = Year, y = percentage_field, color = Subtype, group = Subtype)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_brewer(name = "Submethod", palette ="Set1") +
  labs(title = "Lesions Field Distribution per Year",
       x = "Year",
       y = "Lesion Experiments of Yearly Total (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

grid.arrange(lesion_pie, line_lesion, ncol=2, layout_matrix=layout)


# RESULTS 4.4
#Filter all pharma experiments
filtered_pharma <- SDM %>%
  filter(Type == "Pharmacologic")

pct_field_pharma <- filtered_pharma %>%
  group_by(Year, Subtype) %>%
  summarise(Count = n(), .groups = "drop") %>%
  left_join(total_all_by_year, by = "Year") %>%
  mutate(percentage_field = 100 * Count / N_all) %>%
  ungroup()

#count subtype
pharma_summary <- filtered_pharma %>%
  group_by(Subtype) %>%
  summarise(Count = n(), .groups = 'drop') %>% # Count number of subtypes that is used under pharmacologic
  mutate(Subtype = factor(Subtype))

#make percentage to add to pie chart
pharma_summary$percentage <- round(pharma_summary$Count / sum(pharma_summary$Count) * 100, 1)

#colour palette to extend to 12
set1_12 <- colorRampPalette(brewer.pal(9, "Set1"))(12)  # extend Set1 to 12
subs <- levels(factor(percentage_pharma$Subtype))  # percentage_pharma is your df for the line graph
cols_pharma <- setNames(colorRampPalette(brewer.pal(9, "Set1"))(length(subs)), subs)


# PIE CHART
pie_pharma <- ggplot(pharma_summary, aes(x = "", y = Count, fill = Subtype)) +
  geom_bar(stat = "identity", width = 2, alpha = 0.6) +
  coord_polar("y") +
  geom_text(aes(x=, 1.3, label = ifelse(Subtype %in% c("Orexin", "Corticosterone", "Cocaine", "Enzyme inhibitor", "Receptor agonist"), paste(percentage, "%"), "")),
  position = position_stack(vjust = 0.5), color = "black", size=3) +  # Show percentage in pie
  scale_fill_manual(name = "Submethod", values = cols_pharma) +
  labs(title = "Total Pharmacologic Method Distribution") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

percentage_pharma <- filtered_pharma %>%
  group_by(Year, Subtype) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ungroup()

pct_field_pharma

# LINE GRAPH
line_pharma <- ggplot(pct_field_pharma, aes(x = Year, y = percentage_field, color = Subtype, group = Subtype)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) + 
  scale_color_manual(values = cols_pharma) +
  labs(title = "Pharmacologic Method in Field Distribution Yearly",
       x = "Year",
       y = "Pharmacologic Experiments of Yearly Total (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

grid.arrange(pie_pharma, line_pharma, ncol=2, layout_matrix=layout)


# RESULTS 4.5
#Filter out all lines with type genetic
filtered_genetic <- SDM %>%
  filter(Type == "Gene Editing")

pct_field_genetic <- filtered_genetic %>%
  group_by(Year, Subtype) %>%
  summarise(Count = n(), .groups = "drop") %>%
  left_join(total_all_by_year, by = "Year") %>%
  mutate(percentage_field = 100 * Count / N_all) %>%
  ungroup()

#count subtype
gen_summary <- filtered_genetic %>%
  group_by(Subtype) %>%
  summarise(Count = n(), .groups = 'drop') %>%  # Count number of subtypes that is used under genetics
  mutate(Subtype = factor(Subtype))

#make percentage to add to pie chart
gen_summary$percentage <- round(gen_summary$Count / sum(gen_summary$Count) * 100, 1)

# PIE CHART
pie_gen <- ggplot(gen_summary, aes(x = "", y = Count, fill = Subtype)) +
  geom_bar(stat = "identity", alpha = 0.6, width = 2) +
  coord_polar("y") +
  geom_text(aes(x=, 1.3, label = ifelse(Subtype == "HR", "", paste(percentage, "%"))), position = position_stack(vjust = 0.5), color = "black", size=3) +  # Show brain area inside pie chart
  scale_fill_brewer(name = "Submethod", palette ="Set1") +
  labs(title = "Total Gene Editing Distribution") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

percentage_data <- filtered_genetic %>%
  group_by(Year, Subtype) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ungroup()

pct_field_genetic

# LINE GRAPH
gen_per <- ggplot(pct_field_genetic, aes(x = Year, y = percentage_field, color = Subtype, group = Subtype)) +
  geom_line(size = 1, alpha = 0.8) +  # Lag linjer
  geom_point(size = 2, alpha = 0.8) +  # Legg til punkter på linjene
  scale_color_brewer(palette ="Set1") +
  labs(title = "Gene Editing in Field Distribution Yearly",
       x = "Year",
       y = "Gene Editing Experiments of Yearly Total (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

grid.arrange(pie_gen, gen_per, ncol=2, layout_matrix=layout)


# RESULTS 4.6
#Filter out all lines with type optogenetic
filtered_opto <- SDM %>%
  filter(Type == "Optogenetic")

pct_field_opto <- filtered_opto %>%
  group_by(Year, Subtype) %>%
  summarise(Count = n(), .groups = "drop") %>%
  left_join(total_all_by_year, by = "Year") %>%
  mutate(percentage_field = 100 * Count / N_all) %>%
  ungroup()

#count subtype
opto_summary <- filtered_opto %>%
  group_by(Subtype) %>%
  summarise(Count = n(), .groups = 'drop')  # Count number of subtypes that is used under optogenetic

#make percentage to add to pie chart
opto_summary$percentage <- round(opto_summary$Count / sum(opto_summary$Count) * 100, 1)

#PIE CHART
pie_opto <- ggplot(opto_summary, aes(x = "", y = Count, fill = Subtype)) +
  geom_bar(stat = "identity", alpha = 0.6, width = 2) +
  coord_polar("y") +
  geom_text(aes(x=, 1.3, label = ifelse(Subtype %in% c("ChETA", "JAWS", "eNpHR"), "", paste(percentage, "%"))), position = position_stack(vjust = 0.5), color = "black", size=3) +  # Show brain area inside pie chart
  scale_fill_brewer(name = "Submethod", palette ="Set1") +
  labs(title = "Total Distribution of Optogenetic Methods") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate percentage per subtype per year
percentage_opto <- filtered_opto %>%
  group_by(Year, Subtype) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ungroup()

# LINE GRAPH
line_opto <- ggplot(pct_field_opto, aes(x = Year, y = percentage_field, color = Subtype, group = Subtype)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) + 
  labs(title = "Optogenetic Methods in Field Distribution Yearly",
       x = "Year",
       y = "Optogenetic Experiments of Yearly Total (%)") +
  scale_color_brewer(palette ="Set1") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

grid.arrange(pie_opto, line_opto, ncol=2, layout_matrix=layout)


# RESULTS 4.7
#Filter out all lines with type chemogenetic
filtered_chemo <- SDM %>%
  filter(Type == "Chemogenetic")

pct_field_chemo <- filtered_chemo %>%
  group_by(Year, Subtype) %>%
  summarise(Count = n(), .groups = "drop") %>%
  left_join(total_all_by_year, by = "Year") %>%
  mutate(percentage_field = 100 * Count / N_all) %>%
  ungroup()

#count subtype
chemo_summary <- filtered_chemo %>%
  group_by(Subtype) %>%
  summarise(Count = n(), .groups = 'drop')  # Count number of subtypes that is used under chemogenetics

#make percentage to add to pie chart
chemo_summary$percentage <- round(chemo_summary$Count / sum(chemo_summary$Count) * 100, 1)

#PIE CHART
pie_chemo <- ggplot(chemo_summary, aes(x = "", y = Count, fill = Subtype)) +
  geom_bar(stat = "identity", alpha = 0.6, width = 2) +
  coord_polar("y") +
  geom_text(aes(x=, 1.3, label = paste(percentage, "%")), position = position_stack(vjust = 0.5), color = "black", size=3) +  # Show brain area inside pie chart
  labs(title = "Total Distribution of Chemogentic Methods") +
  scale_fill_brewer(name = "Submethod", palette = "Set1") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


# LINE GRAPH
line_chemo <- ggplot(pct_field_chemo, aes(x = Year, y = percentage_field, color = Subtype, group = Subtype)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = "Chemogenetic Methods in Field Distribution Yearly",
       x = "Year",
       y = "Chemogenetic Experiments of Yearly Total (%)") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

grid.arrange(pie_chemo, line_chemo, ncol=2, layout_matrix=layout)


# RESULTS 4.8
# STATISTICAL ANALYSIS
#Select rows to do statistics on
selected_effectiveness <- SDM %>%
  select(Type, Subtype, Overarea, Area, Time, WE, NE, RE, Sleep)

selected_effectiveness

long_effectiveness <- melt(selected_effectiveness, id.vars= c("Type", "Subtype", "Overarea", "Area", "Time", "Sleep"), variable.name= "State", value.name= "Effectiveness")


long <- long_effectiveness %>%
	filter(!is.na(Effectiveness)) #Remove all rows with NA

#ONLY WE METHODS
#Remove all RE and NE in effectivness column
long_WE <- long %>%
	filter(State == "WE", Effectiveness <=300, !(Sleep != "Both" & Effectiveness < 0)) #remove extreme outliers AND drop negative numbers if they dont specifically target wake (both)
long_WE

# mean WE and SEM (Part of TABLE 9)
summ_WE <- long_WE %>%
  group_by(Type, State) %>%
  summarise(
    n    = n(),
    mean = mean(Effectiveness),
    sd   = sd(Effectiveness),
    SE   = sd / sqrt(n),
    .groups = "drop"
  )
summ_WE

# Welch's ANOVA
welch_WE <- oneway.test(Effectiveness ~ Type, data = long_WE, var.equal = FALSE)
welch_WE

# Games-Howell post hoc
gh_WE <- long_WE %>%
  rstatix::games_howell_test(Effectiveness ~ Type) %>%
  rstatix::add_significance() %>%
  arrange(p.adj)
gh_WE

# ONLY NE METHODS
long_NE <- long %>%
	filter(State == "NE", !(Sleep != "NREM" & Effectiveness > 0)) #keep only NE AND remove positive effectiveness values if not specifically targeting NREM

# mean NE and SEM (Part of TABLE 9)
summ_NE <- long_NE %>%
  group_by(Type, State) %>%
  summarise(
    n    = n(),
    mean = mean(Effectiveness),
    sd   = sd(Effectiveness),
    SE   = sd / sqrt(n),
    .groups = "drop"
  )
summ_NE

# Welch's ANOVA
welch_NE <- oneway.test(Effectiveness ~ Type, data = long_NE, var.equal = FALSE)
welch_NE

# Games-Howell post hoc
gh_NE <- long_NE %>%
  rstatix::games_howell_test(Effectiveness ~ Type) %>%
  rstatix::add_significance() %>%
  arrange(p.adj)
gh_NE

# ONLY ON RE METHODS
long_RE <- long %>%
	filter(State == "RE", !(Sleep != "REM" & Effectiveness > 0)) #keep only RE AND remove positive effectiveness values if not specifically targeting REM

# Mean RE and SEM (Part of TABLE 9)
summ_RE <- long_RE %>%
  group_by(Type, State) %>%
  summarise(
    n    = n(),
    mean = mean(Effectiveness),
    sd   = sd(Effectiveness),
    SE   = sd / sqrt(n),
    .groups = "drop"
  )
summ_RE

# Welch's ANOVA
welch_anova <- oneway.test(Effectiveness ~ Type, data = long_RE, var.equal = FALSE)
welch_anova

# Games-Howell post hoc
gh_RE <- long_RE %>%
  rstatix::games_howell_test(Effectiveness ~ Type) %>%
  rstatix::add_significance() %>%
  arrange(p.adj)
gh_RE

# FIGURE 32: EFFECTIVENESS SCATTERPLOT
# WAKEFULNESS
Eff_WE <- ggplot(long_WE, aes(x= Type, y=Effectiveness, fill = Type)) +
	geom_violin(alpha = 0.6) +
	geom_jitter(size=2, alpha=0.5) + #The dots
	labs(title= "Methods Increasing Wakefulness",
	x= "Method",
	y="Method Effectiveness %") +
	geom_hline(yintercept=0, linetype= "dashed", size=1, alpha=0.5) +
	ylim(-120, 310) +
        scale_fill_brewer(name = "Method",palette ="Set1") +
	theme_minimal() +
	theme(axis.title.x=element_blank(), #Remove x-axis label
	plot.title= element_text(hjust=0.5)) #Title to centre

#Signifiance brackets
sig_df_WE <- data.frame (
  xmin        = c("Gene Editing", "Gene Editing", "Gene Editing"),
  xmax        = c("Lesion",       "Mechanical",  "Pharmacologic"),
  y_position  = c(275, 304, 245),     
  annotations = c("*", "*", "*") 
)

Eff_WE1 <- Eff_WE +
  geom_signif(
    data = sig_df_WE,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual = TRUE,
    tip_length = 0.01,     # small bracket tips
    textsize = 5,          # size of the star text
    inherit.aes = FALSE
  )


# NREM
Eff_NE <- ggplot(long_NE, aes(x= Type, y=Effectiveness, fill = Type)) +
	geom_violin(alpha = 0.6) +
	geom_jitter(size=2, alpha=0.5) + #The dots
	labs(title= "Methods Suppressing NREM Sleep",
	x= "Method",
	y="Method Effectiveness %") +
	geom_hline(yintercept=0, linetype= "dashed", size=1, alpha=0.5) +
	ylim(-120, 310) +
        scale_fill_brewer(name = "Method", palette ="Set1") +
	theme_minimal() +
	theme(axis.title.x=element_blank(), #Remove x-axis label
	plot.title= element_text(hjust=0.5)) #Title to centre

#Significance brackets
sig_df_NE <- data.frame (
  xmin        = c("Gene Editing", "Optogenetic"),
  xmax        = c("Chemogenetic",  "Chemogenetic"),
  y_position  = c(285, 250),     # pick values below your upper ylim (250)
  annotations = c("***", "*")   # your desired star levels
)

Eff_NE1 <- Eff_NE +
  geom_signif(
    data = sig_df_NE,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual = TRUE,
    tip_length = 0.01,     # small bracket tips
    textsize = 5,          # size of the star text
    inherit.aes = FALSE
  )
Eff_NE1

# REM
Eff_RE <- ggplot(long_RE, aes(x= Type, y=Effectiveness, fill = Type)) +
	geom_violin(alpha = 0.6) +
	geom_jitter(size=2, alpha=0.5) + #The dots
	labs(title= "Methods Suppressing REM Sleep",
	x= "Method",
	y="Method Effectiveness %") +
	geom_hline(yintercept=0, linetype= "dashed", size=1, alpha=0.5) +
	ylim(-120, 310) +
        scale_fill_brewer(name = "Method", palette ="Set1") +
	theme_minimal() +
	theme(axis.title.x=element_blank(), #Remove x-axis label
	plot.title= element_text(hjust=0.5)) #Title to centre

#Significance brackets
sig_df_RE <- data.frame (
  xmin        = c("Gene Editing", "Gene Editing"),
  xmax        = c("Pharmacologic",  "Mechanical"),
  y_position  = c(250, 285),     # pick values below your upper ylim (250)
  annotations = c("****", "**")   # your desired star levels
)

Eff_RE1 <- Eff_RE +
  geom_signif(
    data = sig_df_RE,
    aes(xmin = xmin, xmax = xmax, annotations = annotations, y_position = y_position),
    manual = TRUE,
    tip_length = 0.01,     # small bracket tips
    textsize = 5,          # size of the star text
    inherit.aes = FALSE
  )
Eff_RE1

grid.arrange(Eff_WE1, Eff_NE1, Eff_RE1)


# FIGURE 33: EFFECT SIZES OF METHOD TARGETING EACH STATE
# Wake Shared Control (Chemo as control)
shared_WE <- load(long_WE,
	x= Type, y = Effectiveness,
	idx = c(
	"Chemogenetic", "Mechanical", "Lesion", "Pharmacologic", "Gene Editing", "Optogenetic"))
shared_WE_mean_diff <- mean_diff(shared_WE)
shared_WE_mean_diff

dabest_plot(shared_WE_mean_diff)


# NREM Shared Control (Chemogenetic as control)
shared_NE <- load(long_NE,
	x= Type, y = Effectiveness,
	idx = c(
	"Chemogenetic", "Mechanical", "Lesion", "Pharmacologic", "Gene Editing", "Optogenetic"))

shared_NE_mean_diff <- mean_diff(shared_NE)
print(shared_NE_mean_diff)

dabest_plot(shared_NE_mean_diff)

# REM Shared Control (Pharma as control)
shared_RE <- load(long_RE,
	x= Type, y = Effectiveness,
	idx = c(
	"Pharmacologic", "Mechanical", "Lesion", "Gene Editing", "Optogenetic", "Chemogenetic"))

shared_RE_mean_diff <- mean_diff(shared_RE)
print(shared_RE_mean_diff)

# PART OF FIGURE 33
dabest_plot(shared_RE_mean_diff)


# FIGURE 34: EFFECTIVENESS VS RECORDING TIME
#Take out columns Time, WE, NE, RE
long_abs <- long %>%
  mutate(Effectiveness = abs(Effectiveness))

long_abs <- long %>%
	mutate(State = recode(State,
                        WE = "Wake",
                        NE = "NREM",
                        RE = "REM"))

# The scatterplot with line of time count
ggplot(long_abs, aes(x = Time, y = Effectiveness, color = State)) +
  geom_point(alpha = 0.8) + 
  labs(title = "Percentage of Effectiveness and Duration of Recording",
       x = "Duration of Recording (Hrs)",
       y = "Effectiveness %") +
  scale_y_continuous(limits = c(0, 250)) + #y-axis values
  theme_minimal() +
  scale_color_brewer(name = "State Effectiveness", palette ="Set1") +
  theme(plot.title= element_text(hjust=0.5))

# FIGURE 35: HEATMAP WITH METHODS + SUBMETHODS AND EFFECTIVENESS
#filter rows with NA in area
filtered_area <- SDM %>%
  filter(!is.na(Area) & Area != "N/A", !(Type == "Mechanical"))

summary(filtered_area)

heatmap_data <- filtered_area %>%
  group_by(Overarea, Area, Type, Subtype) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  ungroup()

# Making wanted order of types
heatmap_data$Type <- factor(heatmap_data$Type, 
                             levels = c("Lesion", "Pharmacologic", "Gene Editing", "Optogenetic", "Chemogenetic"))

# Making wanted order of subtypes
heatmap_data$Subtype <- factor(heatmap_data$Subtype, 
                             levels = c("Permanent", "Temporary", "Cocaine", "Corticosterone", "Orexin", "Modafinil", "Receptor agonist", "Receptor antagonist", "Breeding", "HR", "Gene casette", "CRISPR", "ChR2", "ChETA", "eNpHR", "SFO", "ArchT", "iC++", "JAWS", "hM3Dq", "hM4Di"))

unique(heatmap_data$Type) #See that the levels are correct and all types are included
unique(heatmap_data$Subtype) #See that the levels are correct and all subtypes are included

# Make hierarchy and put subtype under type. I turn the order of types and subtypes here, since I didn't like how ggplot2 put in my wanted order
heatmap_data$Hierarchy <- factor(paste(heatmap_data$Type, "-", heatmap_data$Subtype),
                                  levels = rev(paste(rep(levels(heatmap_data$Type), each = length(unique(heatmap_data$Subtype))), #rev reverses the order
                                                  unique(heatmap_data$Subtype),
                                                  sep = " - ")),
                                  ordered = TRUE)  # Ensure correct order of types and subtypes

summary(heatmap_data)

#Make hierarchy using Overarea and Area
heatmap_data$AreaHierarchy <- factor(paste(heatmap_data$Overarea, "-", heatmap_data$Area),
                                   levels =(paste(rep(levels(heatmap_data$Overarea), each = length(unique(heatmap_data$Area))), 
                                                   unique(heatmap_data$Area),
                                                   sep = " - ")),
                                   ordered = TRUE)  # Ensure correct order of areas
tibble(heatmap_data)
# Make heatmap with effectivness against methods
ggplot(heatmap_data, aes(x = AreaHierarchy, y = Hierarchy, fill = Percentage)) +
  geom_tile(color = "gray") +  # Make coloured border
  labs(title = "Prevalence of Methods and Submethods Targeting Brain Areas",
       x = "Brain Region - <span style='color:blue'>Brain Area</span>",
       y = "Method - <span style='color:blue'>Submethod</span>") +
  scale_fill_viridis_c(
    option = "viridis",  # palette
    begin = 0.00,        # start at green-ish
    end   = 1.00,        # end at yellow
    direction = 1,
    name = "Percentage") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1), axis.text.x = element_text(angle=60, hjust =1), plot.title = element_text(hjust = 0.5)) + #Keep y-axis horizontal and x-axis vertical, centre title
  scale_x_discrete(labels = function(x) {
    gsub("^(.*?)( - )(.*)", "\\1 - <span style='color:blue'>\\3</span>", x) # Changing Second Part to Blue
  }) +
  scale_y_discrete(labels = function(x) {
    gsub("^(.*?)( - )(.*)", "\\1 - <span style='color:blue'>\\3</span>", x) # Changing Second Part to Blue
  }) +
  theme(axis.text.x = ggtext::element_markdown(), 
	axis.text.y= ggtext::element_markdown(),
	axis.title.x= ggtext::element_markdown(),
	axis.title.y= ggtext::element_markdown())

#FIGURE A.1
# Make heatmap with effectivness against methods
ggplot(heatmap_data, aes(x = AreaHierarchy, y = Type, fill = Percentage)) +
  geom_tile(color = "gray") +  # Make coloured border
  labs(title = "Prevalence of Methods Targeting Brain Areas",
       x = "Brain Region - <span style='color:blue'>Brain Area</span>",
       y = "Method") +
  scale_fill_viridis_c(
    option = "viridis",  # palette
    begin = 0.00,        # start at green-ish
    end   = 1.00,        # end at yellow
    direction = 1,
    name = "Percentage") +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1), axis.text.x = element_text(angle=60, hjust =1), plot.title = element_text(hjust = 0.5)) + #Keep y-axis horizontal and x-axis vertical, centre title
  scale_x_discrete(labels = function(x) {
    gsub("^(.*?)( - )(.*)", "\\1 - <span style='color:blue'>\\3</span>", x) # Changing Second Part to Blue
  }) +
  scale_y_discrete(labels = function(x) {
    gsub("^(.*?)( - )(.*)", "\\1 - <span style='color:blue'>\\3</span>", x) # Changing Second Part to Blue
  }) +
  theme(axis.text.x = ggtext::element_markdown(), 
	axis.text.y= ggtext::element_markdown(),
	axis.title.x= ggtext::element_markdown(),
	axis.title.y= ggtext::element_markdown())

# FIGURE 36: METHOD EFFECTIVENESS TARGETING AREAS HEATMAP
#MAKE HEATMAP USING EFFECTIVENESS
#WE heatmap
#Make absolute values of data and take the mean of the wake effectiveness percentage
filtered_MeanWE <- long_WE %>%
  filter(!is.na(Area) & Area != "N/A") %>%
  group_by(Type, Subtype, Overarea, Area) %>%
  summarise(MeanWE = mean(Effectiveness), .groups = 'drop')  # take the mean of brain areas that have more than one value

# Make hierarchy and put subtype under type. I turn the order of types and subtypes here, since I didn't like how ggplot2 put in my wanted order
filtered_MeanWE$Hierarchy <- factor(paste(filtered_MeanWE$Type, "-", filtered_MeanWE$Subtype),
                                  levels = rev(paste(rep(levels(filtered_MeanWE$Type), each = length(unique(filtered_MeanWE$Subtype))), #rev reverses the order
                                                  unique(filtered_MeanWE$Subtype),
                                                  sep = " - ")),
                                  ordered = TRUE)  # Ensure correct order of types and subtypes

#Areahierarchy
filtered_MeanWE$AreaHierarchy <- factor(paste(filtered_MeanWE$Overarea, "-", filtered_MeanWE$Area),
                                   levels =(paste(rep(levels(filtered_MeanWE$Overarea), each = length(unique(filtered_MeanWE$Area))), 
                                                   unique(filtered_MeanWE$Area),
                                                   sep = " - ")),
                                   ordered = TRUE)  # Ensure correct order of areas

WE_heatmap <- ggplot(filtered_MeanWE, aes(x = AreaHierarchy, y = Hierarchy, fill = MeanWE)) +
  geom_tile(color = "gray") +  # Make coloured border
  labs(title = "Wake-targeting Method Effectiveness in Targeting Brain Area",
       x = "Brain Region - <span style='color:blue'>Brain Area</span>",
       y = "Method - <span style='color:blue'>Submethod</span>",
       fill = "Mean WE %") +
  scale_fill_viridis_c(
    option = "viridis",  # palette
    begin = 0.00,        # start at green-ish
    end   = 1.00,        # end at yellow
    direction = 1,
    name = "Percentage",
    limits = c(-170, 170)) +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1), axis.text.x = element_text(angle=30, hjust =1), plot.title = element_text(hjust = 0.5)) + #Keep y-axis horizontal and x-axis vertical, centre title
  scale_x_discrete(labels = function(x) {
    gsub("^(.*?)( - )(.*)", "\\1 - <span style='color:blue'>\\3</span>", x) # Changing Second Part to Blue
  }) +
  scale_y_discrete(labels = function(x) {
    gsub("^(.*?)( - )(.*)", "\\1 - <span style='color:blue'>\\3</span>", x) # Changing Second Part to Blue
  }) +
  theme(axis.text.x = ggtext::element_markdown(), 
	axis.text.y= ggtext::element_markdown(),
	axis.title.x= ggtext::element_markdown(),
	axis.title.y= ggtext::element_markdown())

#NE HEATMAP
filtered_MeanNE <- long_NE %>%
  filter(!is.na(Area) & Area != "N/A") %>%
  group_by(Type, Subtype, Overarea, Area) %>%
  summarise(MeanNE = mean(Effectiveness), .groups = 'drop')  # take the mean of brain areas that have more than one value

# Make hierarchy and put subtype under type. I turn the order of types and subtypes here, since I didn't like how ggplot2 put in my wanted order
filtered_MeanNE$Hierarchy <- factor(paste(filtered_MeanNE$Type, "-", filtered_MeanNE$Subtype),
                                  levels = rev(paste(rep(levels(filtered_MeanNE$Type), each = length(unique(filtered_MeanNE$Subtype))), #rev reverses the order
                                                  unique(filtered_MeanNE$Subtype),
                                                  sep = " - ")),
                                  ordered = TRUE)  # Ensure correct order of types and subtypes

#Areahierarchy
filtered_MeanNE$AreaHierarchy <- factor(paste(filtered_MeanNE$Overarea, "-", filtered_MeanNE$Area),
                                   levels =(paste(rep(levels(filtered_MeanNE$Overarea), each = length(unique(filtered_MeanNE$Area))), 
                                                   unique(filtered_MeanNE$Area),
                                                   sep = " - ")),
                                   ordered = TRUE)  # Ensure correct order of areas

NE_heatmap <- ggplot(filtered_MeanNE, aes(x = AreaHierarchy, y = Hierarchy, fill = MeanNE)) +
  geom_tile(color = "gray") +  # Make coloured border
  labs(title = "NREM-targeting Method Effectiveness in Targeting Brain Area",
       x = "Brain Region - <span style='color:blue'>Brain Area</span>",
       y = "Method - <span style='color:blue'>Submethod</span>",
       fill = "Mean WE %") +
  scale_fill_viridis_c(
    option = "viridis",  # palette
    begin = 0.00,        # start at green-ish
    end   = 1.00,        # end at yellow
    direction = 1,
    name = "Percentage",
    limits = c(-170, 170)) +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1), axis.text.x = element_text(angle=30, hjust =1), plot.title = element_text(hjust = 0.5)) + #Keep y-axis horizontal and x-axis vertical, centre title
  scale_x_discrete(labels = function(x) {
    gsub("^(.*?)( - )(.*)", "\\1 - <span style='color:blue'>\\3</span>", x) # Changing Second Part to Blue
  }) +
  scale_y_discrete(labels = function(x) {
    gsub("^(.*?)( - )(.*)", "\\1 - <span style='color:blue'>\\3</span>", x) # Changing Second Part to Blue
  }) +
  theme(axis.text.x = ggtext::element_markdown(), 
	axis.text.y= ggtext::element_markdown(),
	axis.title.x= ggtext::element_markdown(),
	axis.title.y= ggtext::element_markdown())

#RE HEATMAP
filtered_MeanRE <- long_RE %>%
  filter(!is.na(Area) & Area != "N/A") %>%
  group_by(Type, Subtype, Overarea, Area) %>%
  summarise(MeanRE = mean(Effectiveness), .groups = 'drop')  # take the mean of brain areas that have more than one value

# Make hierarchy and put subtype under type. I turn the order of types and subtypes here, since I didn't like how ggplot2 put in my wanted order
filtered_MeanRE$Hierarchy <- factor(paste(filtered_MeanRE$Type, "-", filtered_MeanRE$Subtype),
                                  levels = rev(paste(rep(levels(filtered_MeanRE$Type), each = length(unique(filtered_MeanRE$Subtype))), #rev reverses the order
                                                  unique(filtered_MeanRE$Subtype),
                                                  sep = " - ")),
                                  ordered = TRUE)  # Ensure correct order of types and subtypes

filtered_MeanRE$AreaHierarchy <- factor(paste(filtered_MeanRE$Overarea, "-", filtered_MeanRE$Area),
                                   levels =(paste(rep(levels(filtered_MeanRE$Overarea), each = length(unique(filtered_MeanRE$Area))), 
                                                   unique(filtered_MeanRE$Area),
                                                   sep = " - ")),
                                   ordered = TRUE)  # Ensure correct order of areas

RE_heatmap <- ggplot(filtered_MeanRE, aes(x = AreaHierarchy, y = Hierarchy, fill = MeanRE)) +
  geom_tile(color = "gray") +  # Make coloured border
  labs(title = "REM-targeting Method Effectiveness in Targeting Brain Area",
       x = "Brain Region - <span style='color:blue'>Brain Area</span>",
       y = "Method - <span style='color:blue'>Submethod</span>",
       fill = "Mean WE %") +
  scale_fill_viridis_c(
    option = "viridis",  # palette
    begin = 0.00,        # start at green-ish
    end   = 1.00,        # end at yellow
    direction = 1,
    name = "Percentage",
    limits = c(-170, 170)) +
  theme_bw() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1), axis.text.x = element_text(angle=30, hjust =1), plot.title = element_text(hjust = 0.5)) + #Keep y-axis horizontal and x-axis vertical, centre title
  scale_x_discrete(labels = function(x) {
    gsub("^(.*?)( - )(.*)", "\\1 - <span style='color:blue'>\\3</span>", x) # Changing Second Part to Blue
  }) +
  scale_y_discrete(labels = function(x) {
    gsub("^(.*?)( - )(.*)", "\\1 - <span style='color:blue'>\\3</span>", x) # Changing Second Part to Blue
  }) +
  theme(axis.text.x = ggtext::element_markdown(), 
	axis.text.y= ggtext::element_markdown(),
	axis.title.x= ggtext::element_markdown(),
	axis.title.y= ggtext::element_markdown())

grid.arrange(WE_heatmap, NE_heatmap, RE_heatmap, ncol=1)
