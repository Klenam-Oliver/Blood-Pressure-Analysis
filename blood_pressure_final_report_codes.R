
# Load necessary libraries
library(dplyr)
library(knitr)
library(car)
library(emmeans)

# Read in data
bpdata <- read.csv("/Users/presidentoliver/Desktop/Practicum in Data Analysis/Project 1 - Blood pressure study/bloodpressurestudy.csv")
#names(bpdata)

bpdata <- bpdata %>%
  mutate(subject = factor(subject),
         BPchange = after-before,                                # straight pre/post diff
         rel.BPchange = (after-before)/before,                   # change relative to initial
         BP.pct.drop.120 = 100*(before-after)/(before-120)) %>%  # % drop rel to 120 (interp?)
  rename(ID = subject,
         Drug = drug, 
         SysBP.pre = before, 
         SysBP.post = after)



# calculate both mean and standard deviation for BP_Change by Drug
formatted_table <- bpdata %>%
  group_by(Drug) %>%
  summarize(
    mean_BP_Change = mean(BPchange, na.rm = TRUE),
    sd_BP_Change = sd(BPchange, na.rm = TRUE)
  ) %>%
  kable(format = "html", digits = 2, caption = "Table 1")  # Adding a caption

# Print formatted table
formatted_table


### Model 5: One-way ANCOVA on change in BP, using pre-study BP as a covariate

# Visualization
ggplot(bpdata, aes(x=SysBP.pre, y=BPchange, shape=Drug, colour=Drug, fill=Drug)) +
  geom_smooth(method="lm") +
  geom_point() +
  labs(x = "Pre-study Systolic BP (mmHg)",
       y = "Change in Systolic BP (mmHg)")

# Modeling
ancova2 <- aov(BPchange ~ SysBP.pre + Drug, data = bpdata)



# Multiple comparison - simultaneous CIs 
emmeans(ancova2, specs = pairwise ~ Drug)

# Get the emmeans output
emmeans_output <- emmeans(ancova2, specs = pairwise ~ Drug)

# Format the contrast results using kable
contrast_table <- as.data.frame(emmeans_output$contrasts)

# Use kable to format the table for Word
kable(contrast_table, format = "markdown", caption = "Pairwise Comparisons of Drugs") %>%
  print() 

# Nonparametric test to compare distributions of pre-study BP
kruskal_result <- kruskal.test(SysBP.pre ~ Drug, data = bpdata)


# Extract the necessary values
kruskal_df <- data.frame(
  Statistic = kruskal_result$statistic,
  Degrees_of_Freedom = kruskal_result$parameter,
  P_value = kruskal_result$p.value
)
kable(kruskal_df, format = "html", digits = 3, caption = "Kruskal-Wallis Test for Pre-treatment BP") %>%
  print()