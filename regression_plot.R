# regression plot


current <- dirname(rstudioapi::getActiveDocumentContext()$path)
target <- paste(current)
setwd(target)

library(scales)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(DescTools)

source("data_processing_and_model.R")
source("ggplot_mysoc_theme//mysoc_theme.R")
source("alt_plot_coefs.R")

lm <- glm(correct_mp ~ is_uk + is_eu + is_cm + female + age
  + edu + homeowner + houseincome + income
  + pol_attention + vote_likelyhood + party_strength
  + bame + both_bame + mp_bame + same_minority_religion
  + region_east + region_northwest + region_scotland + region_london
  + mp_female + mp_age + mp_tenure + mp_gov + mp_opp
  + same_party + same_gender
  + correctly_spelled,
weights = dat$wt_full_W1, data = dat, family = binomial("logit")
)

# weighted logit will get a 'non-integer successes' error, adjust using quasibinomial
qlm <- update(lm, family = quasibinomial("logit"))

summary(qlm)

mod <- lm
nullmod <- glm(dat$correct_mp~1, family="binomial")
mcfadden = 1-logLik(mod)/logLik(nullmod)
mcfadden

coefs <- c(
  "UK citizen" = "is_uk",
  "EU citizen" = "is_eu",
  "Commonwealth citizen" = "is_cm",
  "Female respondent" = "female",
  "Respondent age (years)" = "age",
  "Education (6 point scale)" = "edu",
  "Homeowner" = "homeowner",
  "Household income (£10,000s)" = "houseincome",
  "Personal income (£10,000s)" = "income",
  "Political attention (9 point scale)" = "pol_attention",
  "Likelihood to vote (5 point scale)" = "vote_likelyhood",
  "Party identification (3 point scale)" = "party_strength",
  "Ethnic minority (BAME) respondent" = "bame",
  "Ethnic minority (BAME) MP" = "mp_bame",
  "Both ethnic minority" = "both_bame",
  "Ethnic majority - same minority religion" = "same_minority_religion",
  "East" = "region_east",
  "North West" = "region_northwest",
  "Scotland" = "region_scotland",
  "London" = "region_london",
  "Female MP" = "mp_female",
  "Age of MP (years)" = "mp_age",
  "Time as MP (years)" = "mp_tenure",
  "MP has government position" = "mp_gov",
  "MP has shadow position" = "mp_opp",
  "Same party" = "same_party",
  "Both female" = "same_gender"
)

groups <- list(
  Gender = c(
    "Female respondent",
    "Female MP",
    "Both female"
  ),
  Group = c(
    "Ethnic minority (BAME) respondent",
    "Ethnic minority (BAME) MP",
    "Both ethnic minority",
    "Ethnic majority - same minority religion"
  ),
  Citizen = c(
    "UK citizen",
    "EU citizen",
    "Commonwealth citizen"
  ),
  Role = c(
    "MP has government position",
    "MP has shadow position"
  ),
  Age = c(
    "Respondent age (years)",
    "Age of MP (years)",
    "Time as MP (years)"
  ),
  Political = c(
    "Political attention (9 point scale)",
    "Likelihood to vote (5 point scale)",
    "Party identification (3 point scale)",
    "Same party"
  ),
  Capital = c(
    "Household income (£10,000s)",
    "Personal income (£10,000s)",
    "Homeowner",
    "Education (6 point scale)"
  ),
  Area = c(
    "East",
    "North West",
    "Scotland",
    "London"
  )
)



addX <- function(x, ...)
  format(paste0(x, "X"), ...)

colors <- c(mysoc_palette_berry, mysoc_dark_grey, mysoc_blue)
g <- plot_summs(qlm, scale = TRUE, colors = colors, groups = groups, coefs = coefs, facet.label.pos = "left", exp=TRUE, ci_level=0.99) +
  ggtitle("Can you recognise your MP's name from a list?") +
  ylab("Logistic regression (predicted change in odds for one unit increase)") +
  xlab("Estimate and 99% confidence range") +
  scale_x_continuous(label = addX, breaks = 0:10) + 
  mysoc_theme() +
  labs(caption = "Data sources: BES 2014, TheyWorkForYou, EveryPolitican.org")
save_and_show(g, height = 10, "outputs//regression_plot.png")
save_and_show(g, height = 10, width = (10/9*16), "outputs//regression_plot_wide.png")

tab_model(qlm, show.r2= FALSE, file="outputs//regression_table.html", use.viewer=TRUE )
