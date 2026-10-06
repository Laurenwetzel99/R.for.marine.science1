#Practicing downloading files 
install.packages("here")
# Load the primary data science framework and Excel import library
library(tidyverse)
library(readxl)

# Practice Import A: Loading a standard comma-separated plain text file
benthic_cover <- read_csv("../data/workshop1/reef_cover_log.csv")
##this doesnt work you have to tell it where it is like below 
library(readr)
reef_cover_log <- read_csv("data/workshop1/reef_cover_log.csv")
View(reef_cover_log)
# Practice Import B: Parsing a tab-separated telemetry instrument array string
acoustic_stream <- read_tsv(here::here("data/acoustic_telemetry_stream.txt"))
##correct way 
acoustic_stream <- read_tsv("data/workshop1/acoustic_telemetry_stream.txt")
# Practice Import C: Targeting a specific sheet in a multi-tab Excel spreadsheet
fisheries_annual <- read_excel(here::here("data/fish_catch_data.xlsx"), sheet = "Commercial_2026")
#correct way 
library(readxl)
fish_catch_data <- read_excel("data/workshop1/fish_catch_data.xlsx")
View(fish_catch_data)
##Loading a new file 
# Read in mangrove_data
mangrove_data <- read_csv(file = here::here("data/mangrove_survey_raw.csv"))
##incorrect need to tell r where it is 
#below is correct-- but things are missing 
library(readr)
mangrove_survey_raw <- read_csv("data/workshop1/mangrove_survey_raw.csv")
View(mangrove_survey_raw)
# Use args within read_csv to skip headers and declare missing flags
mangrove_data <- read_csv(
  here::here("data/workshop1/mangrove_survey_raw.csv"),
  skip = 5,   # Skip the first 5 lines of field notes
  na = c(".", "NA", "9999", "ND", "blank"))  # Convert known text alts to true NA

##looking at tibble VS legacy base
# Force a modern tibble to degrade into a legacy base R data frame structure
benthic_cover_df <- as.data.frame(benthic_cover)
# Print the old-style dataframe structure to view
print(benthic_cover_df)
# And compare with tibble alternative
print(benthic_cover)

##Exploring data wrangling## 
# Install the data package (execute this command once in your console pane and then delete!)
# install.packages("palmerpenguins")

# Load the package data into active memory
library(palmerpenguins)
data("penguins")

# Examine the structure of the dataset - always do this when loading a new dataset!
glimpse(penguins) # tidyverse version (from dplyr package)
str(penguins) # base R version
# Generate an exploratory summary matrix
summary(penguins)
##Isolating attributes with select()
# Vertically slice specific morphometric variables by explicit name
morphology_metrics <- select(penguins, species, bill_length_mm, bill_depth_mm, body_mass_g)
glimpse(morphology_metrics)
# Retain a continuous block of attributes using the colon operator
spatial_block <- select(penguins, species:island)
# Discard logistics tracking attributes while preserving everything else using the minus sign
clean_scientific_fields <- select(penguins, -year)

##Sifting rows with filter()
# Isolate observations belonging to a single categorical target group
adelie_cohort <- filter(penguins, species == "Adelie")

# Sift out individuals using continuous numerical boundary thresholds
# Preserves only large penguins whose mass exceeds 4500 grams
heavy_penguins <- filter(penguins, body_mass_g > 4500)

# Combine multiple conditional parameters across separate attributes
# Preserves records matching Gentoo penguins sampled explicitly on Biscoe Island
biscoe_gentoo <- filter(penguins, species == "Gentoo" & island == "Biscoe")

# Sift records matching multiple targeting flags within an explicit set
sub_islands <- filter(penguins, island %in% c("Dream", "Torgersen"))


##Ordering sequences with arrange()
# Sort penguins by ascending body mass (Default setting: Smallest mass first)
lightest_first <- arrange(penguins, body_mass_g)

# Sort penguins in descending sequence using the desc() layout wrapper
heaviest_first <- arrange(penguins, desc(body_mass_g))

# Execute nested sorting criteria: Group by species, then sort by descending bill length
stratified_morphology <- arrange(penguins, species, desc(bill_length_mm))

###Introducing the Pipe (|>)
#instead of penguins_subset <- mutate(penguins, bill_ratio = bill_length_mm / bill_depth_mm) OR penguins_final <- filter(penguins_subset, species == "Adelie")
##you can do :below
penguins_final <- penguins |>
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) |>
  filter(species == "Adelie")

##Computing new attributes with mutate()
# Calculate a new morphological ratio in our environment
penguin_ratios <- penguins  |> 
  mutate(body_mass_kg = body_mass_g / 1000,   # Convert grams to kilograms
         bill_ratio = bill_length_mm / bill_depth_mm  # Bill ratio
  )
# View your newly engineered variables appended to the far-right columns
glimpse(penguin_ratios)

##Data aggregation and ecological summarization
# Grouping our active memory penguins by species
grouped_penguins <- group_by(penguins, species)

# Notice that the table looks identical, but metadata notes 'Groups: species [3]'
print(grouped_penguins)

# Collapsing the buckets into explicit summary metrics
species_mass_summary <- summarise(grouped_penguins,
                                  mean_mass_g = mean(body_mass_g)
)
print(species_mass_summary)
##Missing Value Trap-- If a column contains even a single missing observation flag (NA), 
#any mathematical aggregation function (such as mean(), sd(), or sum()) will automatically return NA to protect you from miscalculating metrics on incomplete data.
# Overcoming the missing value trap using na.rm = TRUE
biological_signal <- penguins %>%
  group_by(species, sex) %>%
  summarise(
    sample_size = n(),                                     # Count total individuals per category
    mean_mass_g = mean(body_mass_g, na.rm = TRUE),         # Calculate mean ignoring missing cells
    sd_mass_g   = sd(body_mass_g, na.rm = TRUE)            # Standard deviation calculation
  )

print(biological_signal)

#Summary Table: Pipe your data to group_by() and summarise() to calculate the mean body mass for each species and island.
#Visualisation: Pipe the same dataset into ggplot() to create a boxplot of body_mass_g by species, with the island variable mapped to fill or facet_wrap().
penguins |>
  group_by(species, island) |>
  summarise(
    mean_body_mass = mean(body_mass_g, na.rm = TRUE)
  )

# Boxplot: body mass by species, filled by island
# Visualisation
penguins |>
  ggplot(aes(x = species, y = body_mass_g, fill = island)) +
  geom_boxplot() +
  labs(
    title = "Body Mass by Species and Island",
    x = "Species",
    y = "Body Mass (g)",
    fill = "Island"
  ) +
  theme_minimal()

ggsave("outputs/figures/body_mass_boxplot.png", 
       plot = last_plot(), 
       width = 120, height = 120, 
       units = "mm", dpi = 300)



##Visualizing Mean Trends with Uncertainty Instead of creating a summary table object first,
#you can pipe your grouped data straight into a plot and use geom_errorbar() to represent the spread of your data
# Pipe directly from aggregation to plotting with error bars
mass_compare_plot <- penguins |>
  group_by(species, island) |>
  summarise(
    mean_mass = mean(body_mass_g, na.rm = TRUE),
    sd_mass = sd(body_mass_g, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = species, y = mean_mass, colour = island)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_mass - sd_mass, 
                    ymax = mean_mass + sd_mass), 
                width = 0.2) +
  labs(title = "Mean Body Mass by Species and Island",
       subtitle = "Error bars represent standard deviation",
       y = "Mean Body Mass (g)",
       x = "Species") +
  theme_minimal()

mass_compare_plot


##Saving work
# Create output directories if they do not exist:
if (!dir.exists("outputs/figures")) dir.create("outputs/figures") # folder for figs
if (!dir.exists("outputs/tables")) dir.create("outputs/tables") # folder for tables
if (!dir.exists("Rdata")) dir.create("Rdata") # folder for Rdata objects
# 1. Exporting our collapsed summary table as a universal flat text file
write_csv(biological_signal, "outputs/penguin_species_mass_summary.csv")

# 2. Saving our cleaned morphological cohort table as a native R binary file
saveRDS(clean_cohort, "outputs/clean_penguin_morphology_cohort.rds")
#clean_cohort doesnt exist 
#Ggplot saving
ggsave("outputs/figures/mass_compare_plot.png", 
       plot = mass_compare_plot, 
       width = 120, height = 120, 
       units = "mm", dpi = 300)






