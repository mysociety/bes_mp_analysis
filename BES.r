require(MASS)
require(pscl)
require(broom)
require(multiwayvcov)
require(lmtest)
require(weights)
require(jtools)
require(car)
rm(list = ls())

source("ggplot_mysoc_theme//mysoc_theme.R")


current <- dirname(rstudioapi::getActiveDocumentContext()$path)
target <- paste(current)
setwd(target)

dat <- read.csv("BES2015_W1_v7.9.csv", header = TRUE)

ep <- read.csv("ep\\bes_amended.csv", header = TRUE)

dat <- merge(dat, ep, c("mpName"))


dat$mp_female <- 0
dat$mp_female[dat$mp_gender == "female"] <- 1

# convert existing binary
dat$correct_mp <- 0
dat$is_uk <- 0
dat$is_eu <- 0
dat$is_cm <- 0

dat$correct_mp[dat$knowMP == "Correct name"] <- 1

dat$weighed_correct <- dat$correct_mp * dat$wt_full_W1

dat$female <- 0
dat$female[dat$gender=='Female'] <- 1

dat$is_uk[dat$ukCitizen == "Yes"] <- 1
dat$is_eu[dat$euCitizen == "Yes"] <- 1
dat$is_cm[dat$commonwealthCitizen == "Yes"] <- 1

dat$pol_attention[dat$polAttention == "Don't know"] <- ""
dat$pol_attention[dat$polAttention == "Pay a great deal of attention"] <- 7
dat$pol_attention[dat$polAttention == "Pay no attention"] <- 1
dat$pol_attention <- as.numeric(as.character(dat$polAttention))

# create country binary
dat$is_england <- 0
dat$is_scotland <- 0
dat$is_wales <- 0
dat$is_england[dat$country == "England"] <- 1
dat$is_scotland[dat$country == "Scotland"] <- 1
dat$is_wales[dat$country == "Wales"] <- 1

dat$edu <- NA
dat$edu[dat$edlevel == "No qualifications"] <- 0
dat$edu[dat$edlevel == "Below GCSE"] <- 1
dat$edu[dat$edlevel == "GCSE"] <- 2
dat$edu[dat$edlevel == "A-level"] <- 3
dat$edu[dat$edlevel == "Undergraduate"] <- 4
dat$edu[dat$edlevel == "Postgrad"] <- 5

dat$homeowner <- 0
dat$homeowner[dat$housing == "Own the leasehold/freehold outright"] <- 1
dat$homeowner[dat$housing == "Buying leasehold/freehold on a mortgage"] <- 1

dat$below_35 <- 0
dat$below_35[dat$age < 35] <- 1

dat$houseincome <- NA
dat$houseincome[dat$profile_gross_household == "under £5,000 per year"] <- 1
dat$houseincome[dat$profile_gross_household == "£5,000 to £9,999 per year"] <- 5000
dat$houseincome[dat$profile_gross_household == "£10,000 to £14,999 per year"] <- 10000
dat$houseincome[dat$profile_gross_household == "£15,000 to £19,999 per year"] <- 15000
dat$houseincome[dat$profile_gross_household == "£20,000 to £24,999 per year"] <- 20000
dat$houseincome[dat$profile_gross_household == "£25,000 to £29,999 per year"] <- 25000
dat$houseincome[dat$profile_gross_household == "£30,000 to £34,999 per year"] <- 30000
dat$houseincome[dat$profile_gross_household == "£35,000 to £39,999 per year"] <- 35000
dat$houseincome[dat$profile_gross_household == "£40,000 to £44,999 per year"] <- 40000
dat$houseincome[dat$profile_gross_household == "£45,000 to £49,999 per year"] <- 45000
dat$houseincome[dat$profile_gross_household == "£50,000 to £59,999 per year"] <- 50000
dat$houseincome[dat$profile_gross_household == "£60,000 to £69,999 per year"] <- 60000
dat$houseincome[dat$profile_gross_household == "£70,000 to £99,999 per year"] <- 70000
dat$houseincome[dat$profile_gross_household == "£100,000 to £149,999 per year"] <- 100000
dat$houseincome[dat$profile_gross_household == "£150,000 and over"] <- 1500000



dat$is_uk[dat$ukCitizen == "Yes"] <- 1
dat$is_eu[dat$euCitizen == "Yes"] <- 1
dat$is_cm[dat$commonwealthCitizen == "Yes"] <- 1


dat$income <- NA
dat$income[dat$profile_gross_personal == "under £5,000 per year"] <- 0
dat$income[dat$profile_gross_personal == "£5,000 to £9,999 per year"] <- 5
dat$income[dat$profile_gross_personal == "£10,000 to £14,999 per year"] <- 10
dat$income[dat$profile_gross_personal == "£15,000 to £19,999 per year"] <- 15
dat$income[dat$profile_gross_personal == "£20,000 to £24,999 per year"] <- 20
dat$income[dat$profile_gross_personal == "£25,000 to £29,999 per year"] <- 25
dat$income[dat$profile_gross_personal == "£30,000 to £34,999 per year"] <- 30
dat$income[dat$profile_gross_personal == "£35,000 to £39,999 per year"] <- 35
dat$income[dat$profile_gross_personal == "£40,000 to £44,999 per year"] <- 40
dat$income[dat$profile_gross_personal == "£45,000 to £49,999 per year"] <- 45
dat$income[dat$profile_gross_personal == "£50,000 to £59,999 per year"] <- 50
dat$income[dat$profile_gross_personal == "£60,000 to £69,999 per year"] <- 60
dat$income[dat$profile_gross_personal == "£70,000 to £99,999 per year"] <- 70
dat$income[dat$profile_gross_personal == "£100,000 to £149,999 per year"] <- 100
dat$income[dat$profile_gross_personal == "£150,000 and over"] <- 150

dat$vote_likelyhood <- NA
dat$vote_likelyhood[dat$turnoutUKGeneral == "Very unlikely that I would vote"] <- 1
dat$vote_likelyhood[dat$turnoutUKGeneral == "Fairly unlikely"] <- 2
dat$vote_likelyhood[dat$turnoutUKGeneral == "Neither likely nor unlikely"] <- 3
dat$vote_likelyhood[dat$turnoutUKGeneral == "Fairly likely"] <- 4
dat$vote_likelyhood[dat$turnoutUKGeneral == "Very likely that I would vote"] <- 5

dat$party_strength <- NA
dat$party_strength[dat$partyIdStrength == "Not very strong"] <- 1
dat$party_strength[dat$partyIdStrength == "Fairly strong"] <- 2
dat$party_strength[dat$partyIdStrength == "Very strong"] <- 3

dat$nonwhitebritish <- 1
dat$nonwhitebritish[dat$profile_ethnicity == "White British"] <- 0
dat$nonwhitebritish[dat$profile_ethnicity == ""] <- NA
dat$nonwhitebritish[dat$profile_ethnicity == "Prefer not to say"] <- NA

dat$bame <- 1
dat$bame[dat$profile_ethnicity == "White British"] <- 0
dat$bame[dat$profile_ethnicity == "Any other white background"] <- 0
dat$bame[dat$profile_ethnicity == ""] <- NA
dat$bame[dat$profile_ethnicity == "Prefer not to say"] <- NA

dat$region_east <- 0
dat$region_east[dat$gor == "East of England"] <- 1
dat$region_yorkshire <- 0
dat$region_yorkshire[dat$gor == "Yorkshire and the Humber"] <- 1
dat$region_westmidlands <- 0
dat$region_westmidlands[dat$gor == "West Midlands"] <- 1
dat$region_northwest <- 0
dat$region_northwest[dat$gor == "North West"] <- 1
dat$region_scotland <- 0
dat$region_scotland[dat$gor == "Scotland"] <- 1
dat$region_london <- 0
dat$region_london[dat$gor == "London"] <- 1

dat$person_party <- ""
dat$person_party[dat$partyId == "No - none"] <- ""
dat$person_party[dat$partyId == "Other"] <- ""
dat$person_party[dat$partyId == "Don't know"] <- ""
dat$person_party[dat$partyId == "Liberal Democrat"] <- "liberal-democrat"
dat$person_party[dat$partyId == "Labour"] <- "labour"
dat$person_party[dat$partyId == "Conservative"] <- "conservative"
dat$person_party[dat$partyId == "United Kingdom Independence Party (UKIP)"] <- "ukip"
dat$person_party[dat$partyId == "Green Party"] <- "green"
dat$person_party[dat$partyId == "British National Party (BNP)"] <- "bnp"
dat$person_party[dat$partyId == "Scottish National Party (SNP)"] <- "scottish-national-party"
dat$person_party[dat$partyId == "Plaid Cymru"] <- "plaid-cymru"

dat$mp_party[dat$mp_party == "labourco-operative"] <- "labour"

dat$same_party <- 0
dat$same_party[dat$mp_party == dat$person_party] <- 1
dat$same_gender <- 0
dat$same_gender[dat$female == dat$mp_female] <- 1

dat$same_bame <- 0
dat$same_bame[dat$bame == dat$mp_bame] <- 1

dat$mp_bame_voter_not <- 0
dat$mp_bame_voter_not[dat$bame == 0 & dat$mp_bame == 1] <- 1

dat$both_bame <- 0
dat$both_bame[dat$bame == 1 & dat$mp_bame == 1] <- 1

dat$new_mp <- 0
dat$new_mp[dat$mp_tenure <= 4] <- 1

# limit just to non white
# dat <- dat[(dat$nonwhite==0),]

lm <- glm(correct_mp ~ is_uk + is_eu + is_cm + female + age
  + edu + homeowner + houseincome + income
  + pol_attention + vote_likelyhood + party_strength
  + bame + both_bame + mp_bame_voter_not
  + region_east + region_northwest + region_scotland + region_london
  + mp_female + mp_age + mp_tenure + mp_gov + mp_opp
  + same_party + same_gender
  + correctly_spelled,
weights = dat$wt_full_W1, data = dat, family = binomial("logit")
)

# weighted logit will get a 'non-integer successes' error, adjust using quasibinomial
qlm <- update(lm, family = quasibinomial("logit"))

summary(qlm)
pR2(lm)

g <- plot_summs(qlm, scale = TRUE) + ggtitle("Regression outputs") + mysoc_discreet_scale() + mysoc_theme()
save_and_show(g, height=10, "plot.png")
stop()

#https://github.com/jacob-long/jtools/blob/021bb47f7773906cc88cbcfe5646674330b6eff3/R/plot_coefs.R
#modify this to:
#reduce size of circles
#colour by crossing the line?


destination <- "regression_output/"
name <- "mp_model"
ci <- confint(qlm)
n <- nrow(dat)
r2 <- pR2(lm)["McFadden"]
write.table(tidy(qlm), paste(destination, name, ".csv", sep = ""), sep = ",", row.names = FALSE)
write.table(tidy(ci), paste(destination, "ci_", name, ".csv", sep = ""), sep = ",", row.names = FALSE)
write.table(tidy(r2), paste(destination, "r_", name, ".csv", sep = ""), sep = ",", row.names = FALSE)
write.table(tidy(n), paste(destination, "n_", name, ".csv", sep = ""), sep = ",", row.names = FALSE)

stop()

#split out the model, plot genreation, and wdt testing into different files. 
wtd.t.test(dat$correct_mp, dat$bame, weight = dat$wt_full_W1)
wtd.t.test(dat$correct_mp, dat$homeowner, weight = dat$wt_full_W1)
wtd.t.test(dat$correct_mp, dat$below_35, weight = dat$wt_full_W1)
wtd.t.test(dat$correct_mp, dat$nonwhite, weight = dat$wt_full_W1)
wtd.t.test(dat$correct_mp, dat$same_party, weight = dat$wt_full_W1)
wtd.t.test(dat$correct_mp, dat$mp_gov, weight = dat$wt_full_W1)

#add some sort of economic measure?

reduced <- dat[!(dat$person_party == ""), ] #hmm what's this doing
wtd.t.test(dat$correct_mp, dat$same_party, weight = dat$wt_full_W1)
