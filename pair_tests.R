# chart of independent t-tests

current <- dirname(rstudioapi::getActiveDocumentContext()$path)
target <- paste(current)
setwd(target)

source("data_processing_and_model.R")
source("ggplot_mysoc_theme//mysoc_theme.R")

#convert to independent t test
ind.wtd.t.test <- function(x, y, weight) {
  new_x <- x[y == 1]
  new_y <- x[y == 0]
  weightx <- weight[y == 1]
  weighty <- weight[y == 0]
  wtd.t.test(new_x, new_y, weight = weightx, weighty = weighty)
}

significance_level = 2.58

#add confidence bounds to results
bound.wtd.t.test <- function(...) {
  test_results <- ind.wtd.t.test(...)
  test_results$additional["Mean.x.lower"] <- test_results$additional["Mean.x"] - significance_level * test_results$additional["Std. Err"]
  test_results$additional["Mean.x.upper"] <- test_results$additional["Mean.x"] + significance_level * test_results$additional["Std. Err"]
  test_results$additional["Mean.y.lower"] <- test_results$additional["Mean.y"] - significance_level * test_results$additional["Std. Err"]
  test_results$additional["Mean.y.upper"] <- test_results$additional["Mean.y"] + significance_level * test_results$additional["Std. Err"]
  test_results
}

# convert results into a dataframe
test.to.df <- function(test_results, set_name, x_name, y_name) {
  series <- c(set_name, set_name)
  name <- c(x_name, y_name)
  lower <- c(test_results$additional["Mean.x.lower"], test_results$additional["Mean.y.lower"])
  upper <- c(test_results$additional["Mean.x.upper"], test_results$additional["Mean.y.upper"])
  estimate <- c(test_results$additional["Mean.x"], test_results$additional["Mean.y"])
  origin <- c("x", "y")
  df <- data.frame(series, name, lower, upper, estimate, origin)
  rownames(df) <- NULL
  df
}


# average taken as below median of 30k(ish)
income_test <- bound.wtd.t.test(dat$correct_mp, dat$above_average_income, weight = dat$wt_full_W1)
income_df <- test.to.df(income_test, "Personal income", "Above 30k", "Below 30k")

edu_test <- bound.wtd.t.test(dat$correct_mp, dat$graduate, weight = dat$wt_full_W1)
edu_df <- test.to.df(edu_test, "University", "Graduates", "Non-graduates")

#reverse to get colours right way around
bame_test <- bound.wtd.t.test(dat$correct_mp, !dat$bame, weight = dat$wt_full_W1)
bame_df <- test.to.df(bame_test, "Ethnicity", "White ethnicities", "BAME ethnicities")

home_test <- bound.wtd.t.test(dat$correct_mp, dat$homeowner, weight = dat$wt_full_W1)
home_df <- test.to.df(home_test, "Housing Tenure", "Homeowners", "Private/social renters")

#reverse to get colours right way around
age_test <- bound.wtd.t.test(dat$correct_mp, !dat$below_35, weight = dat$wt_full_W1)
age_df <- test.to.df(age_test, "Age", "Above 35", "Below 35")

df <- rbind(income_df, edu_df, bame_df, home_df, age_df)


# set up chart of different t-tests

df$offset <- 0.5
df$nudge_x <- 0
df$nudge_y <- 0.25
df$nudge_y[df$origin == "y"] <- -0.25

g <- ggplot(df, aes(
  y = series, x = estimate, xmin = lower,
  xmax = upper, label = name, color = origin, group = NULL
)) +
  ggstance::geom_pointrangeh(
    fill = "white", fatten = 0.8, size = 0.8
  ) +
  geom_text(aes(hjust = offset), nudge_y = df$nudge_y, nudge_x = df$nudge_x, size = standard_pt - 4) +
  xlab("") +
  ylab("Independent comparisons, not accouting for other factors (99% confidence intervals)") +
  ggtitle("Can you recognise your MP's name from a list?") +
  scale_x_continuous(label = percent, limits = 0:1) +
  mysoc_theme(legend.position = "none") +
  scale_colour_manual(values = c(mysoc_blue_dark, mysoc_red)) +
  labs(caption = "Data sources: BES 2014")

save_and_show(g, height=6, "outputs//ttest_plot.png")
