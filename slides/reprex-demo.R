library(reprex)
library(ggplot2)

#### Example 1: copy and paste

# copy this
y <- 1:4
mean(y)

# run reprex, (or use the addin)
reprex()

# paste


#### Example 2: in the function, GitHub output (default)

reprex(rbinom(3, size = 10, prob = 0.5))

# paste


#### Example 3: in the function, StackOverflow output

reprex(rbinom(3, size = 10, prob = 0.5), venue = "so")

# paste


#### Example 4: multiline

reprex({
  x <- 1:4
  y <- 2:5
  x + y
})

# paste


#### Example 5: comments and saving to a file

demo_code <- c(
  "## a regular comment",
  "x <- 1:100",
  "#' Here is some embedded prose, as a roxygen comment.",
  "mean(x)"
)

md_content <-
  reprex(input = demo_code, outfile = "README-tmp_gh", show = FALSE)

# paste

# print to console
cat(demo_code, sep = "\n")

# open up md file


#### Example 6: markdown

reprex({
  #' # A Big Heading
  #'
  #' Look at my cute example. I love the
  #' [reprex](https://github.com/jennybc/reprex#readme) package!
  y <- 1:4
  mean(y)
})

#### Example 7: plots!

plot(1:10, 1:10)
reprex()

# paste

#### Example 8a: fail!
library(ggplot2)
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()



### Example 8b: fail again (if don't copy everything)

library(tibble)
df <- data_frame(num = 1:3, let = letters[1:3])
df


#### Example 9: with sessionInfo and notes

library(tidyverse)
library(broom)

# run a lm
lm_fit <- lm(Sepal.Length ~ . , data = iris)

# use broom to clean it up
lm_augmented <- augment(lm_fit)
head(lm_augmented)

# Explore the fit of the model
ggplot(data = lm_augmented,
       aes(x = .fitted,
           y = Sepal.Length)) + 
  geom_point() 


# run
reprex(si = TRUE)

