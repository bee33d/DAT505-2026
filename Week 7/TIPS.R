#Tips script for Week 7

#PART 1: PREPARE DATA

#Load basic packages
library(tidyverse)
library(e1071)

#Load dataset
data = read.csv("ESS country data - FR.csv")

#Glimpse data: 
glimpse(data)

#Change essround

data = data %>%
  # 1. Filter and recode the rounds to their corresponding years
  filter(lrscale %in% c(0:10)) %>% #lrscale is left-right positioning
  mutate(essround = recode(as.character(essround),
                           "1"  = "2003",
                           "2"  = "2004-05",
                           "3"  = "2006-07",
                           "4"  = "2008-09",
                           "5"  = "2010-11",
                           "6"  = "2013",
                           "7"  = "2014-15",
                           "8"  = "2016-17",
                           "9"  = "2018-19",
                           "10" = "2021",
                           "11" = "2023-24"
  ))


# PART 2: CALCULATE AND VISUALIEZ COOKS DISTANCE

# LOAD LIBRARIES
library(tidyverse)
library(broom)   # This is the magic tool for model diagnostics
library(viridis) # For beautiful, readable color scales

# --- STEP 1: PREPARE THE DATA ---
# We must clean the ESS data because '77', '88', etc., are error codes, not numbers.
plot_data <- data %>%
  filter(essround == "2023-24") %>%
  mutate(
    # Convert to numeric
    lr = as.numeric(lrscale), #left-right position
    im = as.numeric(imwbcnt) #whether you think immigration is beneficial for country or not
  ) %>%
  # Remove ESS missing value codes (anything above 10) and drop NAs
  filter(lr <= 10, im <= 10) %>%
  drop_na(lr, im)

# --- STEP 2: RUN THE MODEL ---
# We create a linear model (lm) first so R can calculate the distances.
my_model <- lm(im ~ lr, data = plot_data)

# --- STEP 3: CALCULATE COOK'S DISTANCE ---
# The augment() function takes the original data and adds "diagnostic" columns.
# It will add a column called '.cooksd'.
augmented_data <- augment(my_model) %>%
  mutate(
    # We flag 'influential' points. 
    # A standard rule of thumb for large data is 4 divided by the number of rows (n).
    is_influential = .cooksd > (4 / n())
  )

# --- STEP 4: BEAUTIFY THE PLOT ---
ggplot(augmented_data, aes(x = lr, y = im)) +
  
  # 1. Draw the "Normal" points first
  # We make them very light and small so they don't distract.
  geom_jitter(data = filter(augmented_data, is_influential == FALSE),
              color = "gray80", alpha = 0.1, size = 0.5, width = 0.3, height = 0.3) +
  
  # 2. Draw the "Influential" points on top
  # We map the size and color to the actual Cook's Distance value.
  geom_jitter(data = filter(augmented_data, is_influential == TRUE),
              aes(color = .cooksd, size = .cooksd), 
              alpha = 0.7, width = 0.3, height = 0.3) +
  
  # 3. Add the trend line
  geom_smooth(method = "lm", color = "black", linewidth = 1, se = TRUE) +
  
  # 4. Refine Colors and Scales
  scale_color_viridis_c(option = "plasma", end = 0.9) + # Beautiful purple-to-yellow scale
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(breaks = 0:10) +
  
  # 5. Labels and Theme
  labs(
    title = "Who moves the line?",
    subtitle = "Points sized by Cook's Distance (Influence). Gray points have negligible impact.",
    x = "Left-Right Scale (0-10)",
    y = "Immigration: Bad to Good (0-10)",
    color = "Influence (Cook's D)",
    size = "Influence (Cook's D)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

#PART 3: DEAL WITH MISSING VALUES WITH MICE

# For visualizing missing values

library(tidyverse)
library(naniar)

plot_data <- data %>%
  filter(essround == "2023-24") %>%
  # Convert to numeric but KEEP the 77, 88, 99 for now
  mutate(
    lr = as.numeric(lrscale),
    im = as.numeric(imwbcnt)
  ) %>%
  # Convert ESS missing codes to NA across your variables
  replace_with_na(replace = list(lr = c(77, 88, 99), 
                                 im = c(77, 88, 99)))


gg_miss_var(plot_data) + 
  labs(title = "Missing Values by Variable") #visualisation


#Missing value analysis
library(mice)
# STEP 1: Run the imputation and SAVE IT to an object (usually called 'imp')
# This 'imp' object is the "Box of 5 possible datasets with 5 different imputations"
imp <- plot_data %>%
  select(im, lr, freehms) %>% #here put the variables that you'd like to inpute and the variables that you think predict the missingness
  mice(m = 5, print = FALSE)

# STEP 2a: Get your statistical results (The Summary)
model_results <- imp %>% 
  with(lm(im ~ lr + freehms)) %>% #here you write the model that will be used in priority to inpute
  pool() %>%
  summary()

# STEP 2b: Get your "reformed" data frame (The Data)
new_plot_data <- complete(imp, 1) 
#then merge the new columns with original dataset
