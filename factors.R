
library(tidyverse)

# Create a factor
f <- factor(c("apple", "banana", "apple", "cherry"))
f
#
  

# Example with explicit levels
f <- factor(c("apple", "banana", "apple", "cherry"), levels = c("apple", "banana", "cherry"))
f
#2. Setting Levels
##Purpose: Specify the order of factor levels explicitly to ensure proper ordering for plotting or analysis.

f <- factor(c("low", "medium", "high"))
f <- fct_relevel(f, "low", "medium", "high")
f

# Reverse the order of levels
f <- fct_relevel(f, "high", "medium", "low")
f
#3. Renaming Levels
##Purpose: Modify the names of factor levels for clarity or consistency.

f <- factor(c("A", "B", "C"))
f <- fct_recode(f, "Group A" = "A", "Group B" = "B", "Group C" = "C")
f

# Abbreviate levels
f <- fct_recode(f, "A" = "Group A", "B" = "Group B", "C" = "Group C")
f

#4. Dropping Unused Levels
##Purpose: Remove levels from a factor that are not present in the data.


f <- factor(c("apple", "banana", "cherry"), levels = c("apple", "banana", "cherry", "date"))
f <- fct_drop(f)
f

# Demonstrate effect of unused levels
data <- tibble(fruits = f)
data <- data %>% mutate(fruits = fct_drop(fruits))


#5. Releveling (Setting the Reference Category)
##Purpose: Change the reference level for statistical models or analysis.


f <- factor(c("control", "treatment", "control"), levels = c("control", "treatment"))
f <- fct_relevel(f, "treatment")
f


# Check effect in a linear model
df <- tibble(outcome = c(10, 15, 20), group = f)
lm(outcome ~ group, data = df)


#6. Collapsing Levels
##Purpose: Combine multiple levels into a single level.


f <- factor(c("red", "blue", "green"))
f <- fct_collapse(f, "cool_colors" = c("blue", "green"))
f


# Collapse to fewer groups
f <- fct_collapse(f, "all_colors" = c("red", "cool_colors"))
f


#7. Encoding Factors
##Purpose: Extract numerical encoding of factor levels.


f <- factor(c("low", "medium", "high"))
as.numeric(f)
  

# Use encoding in a model
df <- tibble(score = c(1, 2, 3), category = f)
model.matrix(~ category, data = df)
#8. Reordering Levels by a Function
#Purpose: Reorder factor levels based on a summary statistic.


  


f <- factor(c("A", "B", "A", "C"))
f <- fct_reorder(f, c(10, 20, 30), .fun = mean)
f

  


# Visualize with reordered levels
data <- tibble(score = c(10, 20, 15), group = f)
ggplot(data, aes(group, score)) + geom_col()
#9. Handling Ordered Factors
#Purpose: Work with ordered categorical variables where the order matters.


  


f <- factor(c("low", "medium", "high"), levels = c("low", "medium", "high"), ordered = TRUE)
f

  


# Compare ordered factors
min(f)
max(f)
#10. Converting Factors to Characters or Numeric
#Purpose: Convert factors back to character or numeric format when needed.


  


# Convert to character
as.character(f)

# Convert to numeric
as.numeric(f)

  


# Demonstrate the conversion
data <- tibble(factor_col = f)
data <- data %>% mutate(char_col = as.character(factor_col), num_col = as.numeric(factor_col))
#11. Counting Levels
#Purpose: Summarize the frequency of each factor level.


  


f <- factor(c("apple", "banana", "apple", "cherry"))
fct_count(f)

  


# Count with dplyr
data <- tibble(fruits = f)
data %>% count(fruits)
#12. Combining Factors
#Purpose: Merge two or more factors into one.


  


f1 <- factor(c("apple", "banana"))
f2 <- factor(c("cherry", "date"))
f <- fct_c(f1, f2)
f

  


# Combine factors in a data frame
data <- tibble(group1 = f1, group2 = f2)
data <- data %>% mutate(combined = fct_c(group1, group2))
#13. Visualizing Factors
#Purpose: Use factor levels effectively in plots.


  


data <- tibble(score = c(10, 20, 15), group = factor(c("A", "B", "C")))
ggplot(data, aes(group, score)) + geom_col()

  


# Highlight reordered levels
data <- data %>% mutate(group = fct_reorder(group, score))
ggplot(data, aes(group, score)) + geom_col()


#Summary Table of Common Functions
# Operation	Function	Description
# Create a factor	factor()	Create a factor from a character vector.
# Set levels	fct_relevel()	Specify or reorder levels.
# Rename levels	fct_recode()	Change level names.
# Drop unused levels	fct_drop()	Remove levels not present in the data.
# Collapse levels	fct_collapse()	Merge multiple levels into one.
# Encode factors	as.numeric()	Extract numeric encoding.
# Reorder by function	fct_reorder()	Reorder levels based on a summary statistic.
# Combine factors	fct_c()	Merge multiple factors into one.