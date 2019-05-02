
# The purpose of this script is to recreate the AIC results from the Garibaldi et al 2013 supplementary tables S3 and S4, using the data they provided
# JR: (5-1-2019) currently, it is not giving the same AIC values as they got in most cases.  One possibility is perhaps not exactly the same data was used for each model?  I need to read their methods more carefully.
# JR: (5-1-2019) I see now that the methods say "we included only crop systems for which both wild insects and honey bees were active flower visitors, i.e. at least 5% of total visitation by either group." (I assume that this corresponds to the crops listed with "yes" in table S2)

# working directory on James's computer--don't forget to change this
setwd("C:/Users/James/Documents/Research/Pollinators/OBServ")

# load packages
library(lme4)

# load data
G2013data <- read.csv(file="Garibaldi_et_al_2013_Database_S1.csv", header=T)
head(G2013data)
nrow(G2013data)

# crops with both hb and wi making up at least 5% of visits (from Garibaldi et al 2013 table S2)
HBWI_5percent <- c(
	"Almond",
	"Buckwheat_A",
	"Buckwheat_B",
	"Cherry_A",
	"Coffee_h_B",
	"Coffee_h_C",
	"Coffee_l_C",
	"Cotton",
	"Cranberry",
	"Kiwi",
	"Longan",
	"Mango_A",
	"Onion",
	"Passion_f",
	"Pigeonpea",
	"Pumpkin",
	"Red_clover",
	"Springrape",
	"Strawberry",
	"Sunflower",
	"Tumip_rape",
	"Watermelon_A",
	"Watermelon_B",
	"Watermelon_C"
)
HBWI_5percent

G2013data_subset <- subset(G2013data, system_ID %in% HBWI_5percent)
nrow(G2013data_subset)

#######################
# POLLEN DEP MODELS
#######################

# Goal: match the AIC results listed in Table S3:
# Note: all variables were standardized using z-scores prior to analysis.  It appears that dataset S1 already had this done.

# in the table, variables are listed like this:
# w = wild insects
# h = honey bees
# R = richness
# E = evenness

# Garibaldi et al 2013 pollen dep model P (the best pollen dep model by AIC)
G2013_PD_model_P = lmer(data=G2013data_subset, pollen_dep ~ visits_wild_insects + visits_honey_bees + Richness + E_var + (1|system_ID), REML=F)

# Garibaldi et al 2013 pollen dep model P (2nd best pollen dep model by AIC -- does not include richness or evenness)
G2013_PD_model_Q = lmer(data=G2013data_subset, pollen_dep ~ visits_wild_insects + visits_honey_bees + (1|system_ID), REML=F)

summary(G2013_PD_model_P)
AIC(G2013_PD_model_P)		# this matches the AIC in the table = 226 (JR: 5-1-2019) EDIT: no longer matches with 5% data filter

summary(G2013_PD_model_Q)
AIC(G2013_PD_model_Q)		# this does NOT match the AIC in the table. I got 240, but should have been 229 (JR: 5-1-2019) EDIT: no longer even close with 5% data filter


#######################
# FRUIT SET MODELS
#######################

# Goal: match the AIC results listed in Table S4:

# Garibaldi et al 2013 fruit set model P (the best fruit set model by AIC)
G2013_FS_model_P = lmer(data=G2013data_subset, fruit_set ~ visits_wild_insects + visits_honey_bees + (1|system_ID), REML=F)

# Garibaldi et al 2013 fruit set model Q (tied for 2nd best model-- includes WB x HB interaction)
G2013_FS_model_Q = lmer(data=G2013data_subset, fruit_set ~ visits_wild_insects + visits_honey_bees + visits_wild_insects*visits_honey_bees + (1|system_ID), REML=F)

# Garibaldi et al 2013 fruit set model F (tied for 2nd best model-- includes WB only)
G2013_FS_model_F = lmer(data=G2013data_subset, fruit_set ~ visits_wild_insects + (1|system_ID), REML=F)

summary(G2013_FS_model_P)
AIC(G2013_FS_model_P)		# this does NOT match the AIC in the table. 

summary(G2013_FS_model_Q)
AIC(G2013_FS_model_Q)		

summary(G2013_FS_model_F)
AIC(G2013_FS_model_F)		



#######################
# extra code from when I tried removing NA observations- not needed at the moment
G2013data_sub2 = subset(G2013data_subset, !is.na(visits_wild_insects) & !is.na(visits_honey_bees))
nrow(G2013data_sub2)



