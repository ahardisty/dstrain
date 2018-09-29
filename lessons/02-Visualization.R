## 02-Visualization.R
# install.packages("ggplot2", "hexbin")
library(ggplot2)
# library(hexbin)

plot(iris$Sepal.Width, iris$Sepal.Length)

qplot(Sepal.Width, Sepal.Length, data = iris)
qplot(Sepal.Width, Sepal.Length, data = iris, color = Species, shape = Species)
qplot(Sepal.Width, Sepal.Length, data = iris, color = Species, shape = Species) + theme_bw()
qplot(Sepal.Width, Sepal.Length, data = iris, color = Species, shape = Species, geom = c("point", "smooth"), method = lm, se = FALSE) + theme_bw()


# Diving in: Scatterplots

install.packages("ggplot2")
library(ggplot2)
?mpg
View(mpg)

qplot(displ, hwy, data = mpg)

# Aesthetics

qplot(displ, hwy, data = mpg, color = class)
qplot(displ, hwy, data = mpg, size = class)
qplot(displ, hwy, data = mpg, shape = class)
qplot(displ, hwy, data = mpg, alpha = class)

qplot(displ, hwy, data = mpg, color = class)

# Faceting

qplot(displ, hwy, data = mpg) + facet_grid(. ~ cyl)
qplot(displ, hwy, data = mpg) + facet_grid(drv ~ .)
qplot(displ, hwy, data = mpg) + facet_grid(drv ~ cyl)
qplot(displ, hwy, data = mpg) + facet_wrap(~ class)

# Geoms

qplot(displ, hwy, data = mpg, geom = "smooth")
qplot(displ, hwy, data = mpg)
qplot(displ, hwy, data = mpg, geom = "point")
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))

# Your Turn
# QPLOT ---------------------------------------------
# qplot of mpg
# qplot of mpg with boxplot geom
# ---------------------------------------------

qplot(class, hwy, data = mpg, geom = "boxplot")
qplot(reorder(class, hwy), hwy, data = mpg, geom = "boxplot")

# Your Turn
# REORDER ---------------------------------------------
# visualize median hwy mileage, reorder by mileage
# ---------------------------------------------

# Diamonds

# Bar charts

# Your Turn
# QPLOT DIAMONDS DATA --------------------------
# MAKE THHREE QPLOTS ON DIAMONDS DATA
# --------------------------

qplot(cut, data = diamonds, geom = "bar", color = cut)
qplot(cut, data = diamonds, geom = "bar", fill = cut)

# Position Adjustments

# Your Turn
# ADJUSTMENTS --------------------------
# MAKE THHREE QPLOTS ON DIAMONDS DATA USING COLOR OR FILL
# --------------------------

qplot(color, data = diamonds, fill = cut, position = "stack")

qplot(color, data = diamonds, fill = cut, position = "stack")
qplot(color, data = diamonds, fill = cut, position = "dodge")
qplot(color, data = diamonds, fill = cut, position = "identity")
qplot(color, data = diamonds, fill = cut, position = "fill")

qplot(cty, hwy, data = mpg)
qplot(cty, hwy, data = mpg, position = "jitter")
qplot(cty, hwy, data = mpg, geom = "jitter")

qplot(cty, hwy, data = mpg, geom = "point", position = "jitter")
qplot(cty, hwy, data = mpg, geom = "jitter")


# Histograms

qplot(displ, data = mpg, binwidth = 1)

qplot(carat, data = diamonds, binwidth = 1)
qplot(carat, data = diamonds, binwidth = 0.1)
qplot(carat, data = diamonds, binwidth = 0.1)
qplot(carat, data = diamonds)

zoom <- coord_cartesian(xlim = c(55, 70))
qplot(depth, data = diamonds, binwidth = 0.2) + zoom

qplot(depth, data = diamonds, binwidth = 0.2, fill = cut) + zoom

qplot(depth, data = diamonds, binwidth = 0.2) +
  zoom + facet_wrap(~ cut)

qplot(depth, data = diamonds, geom = "freqpoly", color = cut,
  binwidth = 0.2) + zoom + facet_wrap(~ cut)

qplot(depth, data = diamonds, geom = "freqpoly",
  color = cut, binwidth = 0.2) + zoom

qplot(depth, data = diamonds, geom = "density",
  color = cut) + zoom

# Your Turn
# ---------------------------------------------
qplot(price, data = diamonds, binwidth = 500) +
  facet_wrap(~ cut)
qplot(price, data = diamonds, binwidth = 500,
  fill = cut)
qplot(price, data = diamonds, binwidth = 500,
  geom = "freqpoly", colour = cut)
qplot(price, data = diamonds, geom = "density",
  colour = cut)
# ---------------------------------------------

qplot(carat, price, data = diamonds, color = cut)

# Geoms for big data

qplot(carat, price, data = diamonds)
qplot(carat, price, data = diamonds, geom = "bin2d")
qplot(carat, price, data = diamonds, geom = "hex")
qplot(carat, price, data = diamonds, geom = "density2d")
qplot(carat, price, data = diamonds,
  geom = c("point", "density2d"))

qplot(carat, price, data = diamonds)
qplot(carat, price, data = diamonds, geom = "smooth")
qplot(carat, price, data = diamonds, geom = "smooth", color = cut)
qplot(carat, price, data = diamonds, geom = "smooth", group = cut)
qplot(carat, price, data = diamonds, geom = "smooth",
  color = cut, se = FALSE)
qplot(carat, price, data = diamonds, geom = "smooth",
  color = cut, se = FALSE, method = lm)

# Your Turn
# -------------------------------
qplot(carat, price, data = diamonds, color = "blue")
# -------------------------------

qplot(carat, price, data = diamonds, color = "blue")
qplot(carat, price, data = diamonds, color = I("blue"))

qplot(carat, price, data = diamonds)
qplot(carat, price, data = diamonds, size = I(0.5))
qplot(carat, price, data = diamonds, alpha = I(0.1))
qplot(carat, price, data = diamonds, size = I(0.5), alpha = I(0.1))

# plot the ds_skill_level -------------------------------------------------
# create a specific data frame
ds_skill_level <- results_tidy %>%
  filter(variable == "ds_skill_level") %>%
  mutate(value = ifelse(is.na(value), 1,value) %>% as.numeric()) %>%
  mutate(type = 'employee')

# try ?case_when (it's a new function)
dt %.% group_by(a) %.% mutate(b = ifelse(is.na(b), mean(b, na.rm = T), b))

# try three geoms
ggplot(ds_skill_level)+
  # geom_freqpoly(aes(value))+
  geom_density(aes(value))+
  # geom_histogram(aes(value), binwidth = 1) +
  # geom_bar(aes(value), stat = 'count')+
  scale_x_continuous(breaks = seq(1,6, by = 1)) +
  ggtitle(label = "distribution of r skills")

# try it with a barplot
ggplot(ds_skill_level) +
  geom_bar(aes(x = hometown, y = value, fill = interaction(favorite_us_city, favorite_non_us_city)), stat = 'identity') +
  scale_y_continuous(breaks = seq(1,6, by = .5)) +
  ggtitle(label = "distribution of ds skills using a boxplot - favorite us city") +
  scale_fill_discrete(name = "interaction of favorite\nus and non us city")


# try it with a boxplot
ggplot(ds_skill_level) +
  geom_boxplot(aes(x = type, y = value)) +
  scale_y_continuous(breaks = seq(1,6, by = .5)) +
  ggtitle(label = "distribution of ds skills using a boxplot")

# boxplot with colors
ggplot(ds_skill_level) +
  geom_boxplot(aes(x = type, y = value, fill = favorite_us_city)) +
  scale_y_continuous(breaks = seq(1,6, by = .5)) +
  ggtitle(label = "distribution of ds skills using a boxplot - favorite us city") +
  scale_fill_discrete(name = "favorite us city") +
  labs(x = "response type", y = "Data Science Skill Level")


ggplot(ds_skill_level) +
  geom_boxplot(aes(x = favorite_us_city, y = value, fill = type)) +
  scale_y_continuous(breaks = seq(1,6, by = .5)) +
  ggtitle(label = "distribution of ds skills using a boxplot - favorite us city") +
  scale_fill_discrete(name = "response type") +
  labs(x = "favorite us city", y = "Data Science Skill Level")

# plot food rankings -------------------------------------------------
# create a specific data frame
rm(results)
load("data/results_tidy.rda")
load("data/results_renamed.rda")


food_rank <- results %>%
  select(star_rank:gender) %>%
  gather(key = food_name, value = score
         , -c(country_of_origin:gender)) %>%
  filter(food_name != "star_rank")

# try three geoms
ggplot(food_rank)+

  # try it with a barplot
  ggplot(food_rank) +

  # try it with a boxplot
  ggplot(food_rank) +

  # boxplot with colors
  ggplot(food_rank) +




  # Saving graphs

  # Your Turn
  # ---------------
getwd()
# ---------------

getwd()
dir()

ggsave("my-plot.pdf")
ggsave("my-plot.png")
ggsave("my-plot.pdf", width = 6, height = 6)



# Saving graphs

# Your Turn
# ---------------
getwd()
# ---------------

getwd()
dir()

ggsave("my-plot.pdf")
ggsave("my-plot.png")
ggsave("my-plot.pdf", width = 6, height = 6)
