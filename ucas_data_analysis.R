# UCAS Clearing Data Analysis -------------------------------------------------

# Load libraries and data -----------------------------------------------------

# Load libraries 

library(tidyverse)
library(gghighlight) # For facet chart
library(extrafont) # For using TASO font
library(scales) # For formatting axis as percentages

# Load data 

# Data as of 20/08

imd_df <- read.csv("UCAS CC 2024 5 days after Level 3 results day - Entry rates by IMD2019_NIMDM2017_SIMD2012_2016_2020_WIMD2019.csv")
polar4_df <- read.csv("UCAS CC 2024 5 days after Level 3 results day - Entry rates by POLAR4 quintile.csv")

# First release data 
#imd_df_results_release <- read.csv("UCAS CC 2024 Level 3 results day - Entry rates by IMD2019_NIMDM2017_SIMD2012_2016_2020_WIMD2019.csv")
#polar4_df_results_release <- read.csv("UCAS CC 2024 Level 3 results day - Entry rates by POLAR4 quintile.csv")

# Filtering data of interest --------------------------------------------------

# IMD ----

# Renaming columns 

imd_df <- imd_df %>%
  rename_all(~ str_replace_all(tolower(.), "\\.", "_")) %>%
  rename(imd_quintile = imd2019_nimdm2017_simd2012_2016_2020_wimd2019)

# Only keep rows for overall entry rates for each year 

imd_df <- imd_df %>%
  filter(age_group == "All",
         gender == "All",
         domicile == "All",
         imd_quintile != "All")

# Keeping columns of interest

imd_df <- imd_df %>%
  select(year, imd_quintile, accepted_applicants, entry_rate)

# Make year a factor 

imd_df <- imd_df %>%
  mutate(year = as.factor(year))

# POLAR ----

# Renaming columns 

polar4_df <- polar4_df %>%
  rename_all(~ str_replace_all(tolower(.), "\\.", "_")) 

# Only keep rows for overall entry rates for each year 

polar4_df <- polar4_df %>%
  filter(age_group == "All",
         gender == "All",
         domicile == "All",
         polar4_quintile != "All")

# Keeping columns of interest

polar4_df <- polar4_df %>%
  select(year, polar4_quintile, accepted_applicants, entry_rate)

# Make year a factor 

polar4_df <- polar4_df %>%
  mutate(year = as.factor(year))

# Charts ----------------------------------------------------------------------

# Setting theme ----

theme_taso <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 12, family = "URW DIN"), # Default text size for all text elements
      plot.title.position = "plot", # Aligns plot title to whole plot
      plot.title = element_text(face = "bold", size = 12),
      plot.caption.position = "plot", # Aligns caption to the left of the plot
      plot.caption = element_text(hjust = 0, size = 9, face = "italic", colour = "#5f5f5f"),
      #panel.grid.major = element_line(colour = "#CECABC", linewidth = 0.3), # Gridline colour
      #panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#edebe3", color = NA), # Background colour
      plot.margin = margin(0.25, 0.25, 0.25, 0.25, "in"), # Adding margin
      panel.border = element_blank(),
      panel.background = element_rect(fill = "#edebe3", color = NA),
      #axis.text = element_text(size = 8),
      axis.title = element_blank(),
      axis.line = element_line(colour = "#5F5F5F", linewidth = 0.5),
      axis.text.x = element_text(margin = margin(t = 7, unit = "pt")), # Increase top margin to move text down
      axis.ticks.length = unit(0.3, "cm"),
      axis.ticks.x = element_line(colour = "#5F5F5F", linewidth = 0.5)# Increase the length of ticks
      
    ) 
}

# Plot accepted applicants ----

fig_1_colors <- c("#3b66bc", "#e14491", "#00a8da", "#835ebd", "#07dbb3")

figure_1 <- ggplot(imd_df, aes(x = year, y = accepted_applicants, group = imd_quintile)) +
  geom_line(aes(color = factor(imd_quintile))) +
  geom_point(data = subset(imd_df, year == 2024),  # Add points only for year 2024
             aes(color = factor(imd_quintile)), size = 1) +
  theme_taso() +
  theme(legend.position = "none",
        plot.title = ggtext::element_markdown(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 6)) +  # Rotate x-axis text to 45 degrees
  gghighlight(use_direct_label = FALSE) +
  facet_wrap(~ imd_quintile, scales = "free") +
  labs(x = "Year", y = "",
       title = 'A record number of <span style="color:#3b66bc;">students from the most deprived areas</span> in the UK have secured  \na place at university or college this year',
       subtitle = "Number of accepted applicants by IMD Quintile",
       caption = "TASO analysis of UCAS confirmation and clearing data, 20 August 2024 release\n\nThe 'IMD quintiles' shown in this chart refer to the national indices of multiple deprivation from across the UK:\nIMD2019 (England), NIMDM2017 (Northern Ireland), SIMD2012/2016/2020 (Scotland), and WIMD2019 (Wales).\nThese indices measure relative levels of deprivation within each region.") +
  scale_y_continuous(labels = comma, limits = c(0, 80000)) +  # Add commas to y-axis labels
  scale_color_manual(values = fig_1_colors) +  # Apply custom colors
  geom_text(data = subset(imd_df, year == 2024), 
            aes(label = comma(accepted_applicants), color = factor(imd_quintile)),  # Match label color to line color
            vjust = -0.7, 
            hjust = 0.7,
            family = "URW DIN", size = 3, fontface = "bold") + # Add commas to data labels 
  coord_cartesian(clip = "off")

# Save as image
ggsave("figure_1.png", 
       plot = figure_1, 
       width = 180, 
       height = 120, 
       units = "mm", 
       dpi = 900, 
       type = "cairo", 
       device = "png")

# Plot entrance rates ----

fig_2_colors <- c("#3b66bc", "grey", "grey", "grey", "#07dbb3")

figure_2 <- ggplot(imd_df, aes(x = year, y = entry_rate, group = imd_quintile)) +
  geom_line(aes(color = factor(imd_quintile)), size = 1.1) +
  theme_taso() +
  theme(legend.position = "none",
        plot.title = ggtext::element_markdown(size = 12),
        panel.grid.major.y = element_line(colour = "#CECABC", linewidth = 0.3), # Gridline colour
        panel.grid.major.x = element_blank()
        ) +
  labs(x = "Year", y = "",
       title = 'Entry rates have gone up across the deprivation distribution, including the <span style="color:#3b66bc;">most deprived</span>  \nand <span style="color:#07dbb3;">least deprived</span> areas',
       subtitle = "Entry rate by IMD Quintile",
       caption = "TASO analysis of UCAS confirmation and clearing data, 20 August 2024 release\n\nThe 'IMD quintiles' shown in this chart refer to the national indices of multiple deprivation from across the UK:\nIMD2019 (England), NIMDM2017 (Northern Ireland), SIMD2012/2016/2020 (Scotland), and WIMD2019 (Wales).\nThese indices measure relative levels of deprivation within each region.") +
  scale_color_manual(values = fig_2_colors) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), 
                     limits = c(0, 0.6), 
                     breaks = seq(0, 0.6, by = 0.1),
                     expand = c(0,0)) +
  geom_text(data = subset(imd_df, year == 2024 & (imd_quintile == "Quintile 1" | imd_quintile == "Quintile 5")), 
            aes(label = scales::percent(entry_rate, accuracy = 1), color = factor(imd_quintile)),  # Format labels as percentages
            vjust = -0.7, 
            hjust = 0.7,
            family = "URW DIN", size = 3, fontface = "bold") +
  geom_point(data = subset(imd_df, imd_quintile == "Quintile 1" | imd_quintile == "Quintile 5"),  # Remove year filter
             aes(color = factor(imd_quintile)), 
             size = 2) + 
  coord_cartesian(clip = "off") 

# Save as image
ggsave("figure_2.png", 
       plot = figure_2, 
       width = 180, 
       height = 120, 
       units = "mm", 
       dpi = 900, 
       type = "cairo", 
       device = "png")

# Plot gap ----

# Calculate gaps for each year

entry_rate_gap <- imd_df %>%
  select(-accepted_applicants) %>%                   # Drop the 'accepted_applicants' column
  filter(imd_quintile %in% c("Quintile 1", "Quintile 5")) %>%  # Filter for Quintile 1 and 5
  pivot_wider(names_from = imd_quintile, values_from = entry_rate) %>%  # Pivot to spread Quintile 1 and 5 as columns
  mutate(entry_rate_gap = `Quintile 5` - `Quintile 1`)  # Calculate the gap between Quintile 1 and Quintile 5

# Plot 

figure_3 <- ggplot(entry_rate_gap, aes(x = year, y = entry_rate_gap, group = 1)) +  
  geom_line(color = "#f9466c", linewidth = 1.2) +  
  geom_point(size = 3, colour = "#f9466c") +  
  labs(title = 'The <span style="color:#f9466c;">entry rate gap</span> between the most deprived and least deprived students is  \nstubborn, and remains at 22 percentage points',
       subtitle = "Entry rate gap (the percentage point difference in entry rates between IMD Quintile 1 \nIMD Quintile 5). A lower gap indicates more equitable progression to higher education.",
       x = "Year",
       y = "Entry Rate Gap (Percentage Points)",
       caption = "TASO analysis of UCAS confirmation and clearing data, 20 August 2024 release\n\nThe 'IMD quintiles' shown in this chart refer to the national indices of multiple deprivation from across the UK:\nIMD2019 (England), NIMDM2017 (Northern Ireland), SIMD2012/2016/2020 (Scotland), and WIMD2019 (Wales).\nThese indices measure relative levels of deprivation within each region.") +
  theme_taso() +
  theme(legend.position = "none",
        plot.title = ggtext::element_markdown(size = 12),
        panel.grid.major.y = element_line(colour = "#CECABC", linewidth = 0.3), 
        panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(labels = function(x) paste0(x * 100, "pp"),  # Label as percentage points
                     limits = c(0, 0.3), 
                     breaks = seq(0, 0.3, by = 0.05),
                     expand = c(0,0)) +
  geom_text(data = subset(entry_rate_gap, year %in% c(2015, 2021, 2023, 2024)), 
            aes(label = paste0(round(entry_rate_gap * 100, 0), "pp")),  # Round values to 0 decimal places
            vjust = -0.9, 
            hjust = 0.5,
            family = "URW DIN", size = 3, fontface = "bold",
            colour = "#f9466c") +
  coord_cartesian(clip = "off") +
  annotate("text",
           x = "2016", y = 0.125,
           label = "The gap is 2pp lower\n than it was in 2015",
           size = 3,
           family = "URW DIN"
  ) + 
  geom_curve(aes(x = "2016", y = 0.148, xend = "2015", yend = 0.235),
             curvature = 0.2, arrow = arrow(length = unit(0.2, "cm")), color = "#5F5F5F") +
  annotate("text",
           x = "2022", y = 0.155,
           label = "The gap has improved\nsince a jump in 2021,\nand is now just below\npre-pandemic levels",
           size = 3,
           family = "URW DIN"
  ) + 
  geom_curve(aes(x = "2022", y = 0.198, xend = "2021", yend = 0.238),
             curvature = 0.2, arrow = arrow(length = unit(0.2, "cm")), color = "#5F5F5F")

# Save as image
ggsave("figure_3.png", 
       plot = figure_3, 
       width = 180, 
       height = 120, 
       units = "mm", 
       dpi = 900, 
       type = "cairo", 
       device = "png")
