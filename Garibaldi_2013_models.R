
# The purpose of this script is to recreate the AIC results from the Garibaldi et al 2013 supplementary tables S3 and S4, using the data they provided
# JR: (5-5-2019) this is now working to my satisfaction for pollen dep models, but fruit set models are still a little off.  
# Previous problems getting the same answers (at least in the pollen dep models) were due to 
# 1) using the wrong subset of crops, see notes below
# 2) differences in how NAs were omitted for running the pollen dep and fruit set models
# Fruit set models are not off by very much (in fact, null model is almost the same AIC)--I am guessing a minor data processing issue? but I could be wrong

# working directory on James's computer--don't forget to change this
setwd("C:/Users/James/Documents/Research/Pollinators/OBServ")

# load packages
library(lme4)
library(MuMIn)

# load data
G2013data <- read.csv(file="Garibaldi_et_al_2013_Database_S1.csv", header=T)
head(G2013data)
nrow(G2013data)

# Note: The methods seemed to suggest that the analysis used only the subset of crops with BOTH hb and wi making up at least 5% of visits (yes/no in column 2 from table S2), but based on the AIC results below it is clear that it uses the full dataset S1, so the following list is not needed.
# HBWI_5percent = c("Almond", "Buckwheat_A", "Buckwheat_B", "Cherry_A", "Coffee_h_B", "Coffee_h_C", "Coffee_l_C", "Cotton", "Cranberry", "Kiwi", "Longan", "Mango_A", "Onion", "Passion_f", "Pigeonpea", "Pumpkin", "Red_clover", "Springrape", "Strawberry", "Sunflower", "Tumip_rape", "Watermelon_A", "Watermelon_B", "Watermelon_C")
# G2013data_subset1 = subset(G2013data, system_ID %in% HBWI_5percent)

#######################
# POLLEN DEP MODELS
#######################

# Goal: match the AIC results listed in Table S3:
# Note: all variables were standardized using z-scores prior to analysis.  It appears that dataset S1 already had this done.
# Note: for AIC comparison, models are fit with ML rather than restricted ML, so REML should be set to false in lmer.  However, after model selection is done REML could be set back to true.
# Note: these results use AIC rather than AICc
# Note: setting na.action in lmer can be important for automated AIC comparison as it can change the input data between models.  The safest option would be na.fail.  Here it shouldn't be an issue since we remove all NAs before running the models.

# in the table, variables are listed like this:
# w = wild insects
# h = honey bees
# R = richness
# E = evenness

# NAs were removed as follows (this is how it was done in Lucas's code):
# subset data so only the relevant variables remain:
G2013data_subset2 = subset(G2013data, select = c(system_ID, E_var, Richness, visits_wild_insects, visits_honey_bees, pollen_dep))
# remove rows that have an NA for any relevant variable:
G2013data_subset2 = na.omit(G2013data_subset2)
nrow(G2013data_subset2)

# full model including up to 4-way interactions!
G2013_PD_model_full = lmer(data=G2013data_subset1, pollen_dep ~ visits_wild_insects*visits_honey_bees*Richness*E_var + (1|system_ID), REML=F, na.action="na.fail")

# full model including up to 4-way interactions, as specified in Lucas's code--note subtraction of some specific interaction terms
G2013_PD_model_full_Lucas = lmer(data=G2013data_subset1, pollen_dep ~ 
	visits_wild_insects*visits_honey_bees*Richness*E_var 
	- visits_honey_bees:Richness:E_var
	- visits_wild_insects:Richness:E_var
	- visits_wild_insects:visits_honey_bees:E_var
	- visits_wild_insects:visits_honey_bees:Richness  
	- visits_wild_insects:visits_honey_bees:Richness:E_var
	+ (1|system_ID),
	REML=F, na.action="na.fail")

# Note: not all possible models were listed in table S3 -- this would be more than 100 models...

# running all possible models with the MuMIn package:
# library(MuMIn)
dredge(G2013_PD_model_full_Lucas, rank="AIC")

# best model according to dredge, but not listed in table S3 for some reason? - has WI, HB, and Richness, but not evenness
G2013_PD_model_X15 = lmer(data=G2013data_subset1, pollen_dep ~ visits_wild_insects + visits_honey_bees + Richness + (1|system_ID), REML=F, na.action="na.fail")
AIC(G2013_PD_model_X15)

# a few spot-checked models from table S3:

# Garibaldi et al 2013 pollen dep null model
G2013_PD_model_A = lmer(data=G2013data_subset1, pollen_dep ~ (1|system_ID), REML=F, na.action="na.fail")
AIC(G2013_PD_model_A)

# Garibaldi et al 2013 pollen dep model B (richness and evenness only)
G2013_PD_model_B = lmer(data=G2013data_subset1, pollen_dep ~ Richness + E_var + (1|system_ID), REML=F, na.action="na.fail")
AIC(G2013_PD_model_B)

# Garibaldi et al 2013 pollen dep model F (wild bees and evenness only)
G2013_PD_model_F = lmer(data=G2013data_subset1, pollen_dep ~ visits_wild_insects + E_var + (1|system_ID), REML=F, na.action="na.fail")
AIC(G2013_PD_model_F)

# Garibaldi et al 2013 pollen dep model P (the best pollen dep model by AIC that was listed in table S3)
G2013_PD_model_P = lmer(data=G2013data_subset1, pollen_dep ~ visits_wild_insects + visits_honey_bees + Richness + E_var + (1|system_ID), REML=F, na.action="na.fail")
AIC(G2013_PD_model_P)

# Garibaldi et al 2013 pollen dep model Q, does not include richness or evenness)
G2013_PD_model_Q = lmer(data=G2013data_subset1, pollen_dep ~ visits_wild_insects + visits_honey_bees + (1|system_ID), REML=F, na.action="na.fail")
AIC(G2013_PD_model_Q)

# Garibaldi et al 2013 pollen dep model R, included HB*WI interaction)
G2013_PD_model_R = lmer(data=G2013data_subset1, pollen_dep ~ visits_wild_insects*visits_honey_bees + (1|system_ID), REML=F, na.action="na.fail")
AIC(G2013_PD_model_R)


#######################
# FRUIT SET MODELS
#######################

# Goal: match the AIC results listed in Table S4:
# Note: all variables were standardized using z-scores prior to analysis.  It appears that dataset S1 already had this done.
# Note: for AIC comparison, models are fit with ML rather than restricted ML, so REML should be set to false in lmer.  However, after model selection is done REML could be set back to true.
# Note: these results use AIC rather than AICc
# Note: setting na.action in lmer can be important for automated AIC comparison as it can change the input data between models.  The safest option would be na.fail.  Here it shouldn't be an issue since we remove all NAs before running the models.

# in the table, variables are listed like this:
# w = wild insects
# h = honey bees
# R = richness
# E = evenness

# Note that a very different set of rows is used for fruit set models than for pollen dep models, due to NAs (primarily because some studies measured one vs the other)
# subset data so only the relevant variables remain:
G2013data_subset3 = subset(G2013data, select = c(system_ID, E_var, Richness, visits_wild_insects, visits_honey_bees, fruit_set))
# remove rows that have an NA for any relevant variable:
G2013data_subset3 = na.omit(G2013data_subset3)
nrow(G2013data_subset3)


# Garibaldi et al 2013 fruit set model A (null)
G2013_FS_model_A = lmer(data=G2013data_subset3, fruit_set ~ (1|system_ID), REML=F, na.action="na.fail")
AIC(G2013_FS_model_A)		# this matches ok

# Garibaldi et al 2013 fruit set model F (WI only)
G2013_FS_model_F = lmer(data=G2013data_subset3, fruit_set ~ visits_wild_insects + (1|system_ID), REML=F, na.action="na.fail")
AIC(G2013_FS_model_F)		# this doesn't match close enough

# Garibaldi et al 2013 fruit set model P 
G2013_FS_model_P = lmer(data=G2013data_subset3, fruit_set ~ visits_wild_insects + visits_honey_bees + (1|system_ID), REML=F, na.action="na.fail")
AIC(G2013_FS_model_P)		# this doesn't match close enough

# Garibaldi et al 2013 fruit set model Q 
G2013_FS_model_Q = lmer(data=G2013data_subset3, fruit_set ~ visits_wild_insects + visits_honey_bees + visits_wild_insects*visits_honey_bees + (1|system_ID), REML=F, na.action="na.fail")
AIC(G2013_FS_model_Q)		# this doesn't match close enough

