import os
import sys

# Ensure R scripts resolve to the active conda env's Rscript (e.g. lme4),
# not a system R that may be earlier on PATH (e.g. /usr/local/bin/Rscript).
os.environ["PATH"] = os.pathsep.join(
    [os.path.dirname(sys.executable), os.environ.get("PATH", "")]
)

# Both the conda R and any system R share the same default personal library
# (~/Library/R/<arch>/<version>/library), so R prioritizes packages built
# against the system R there over the conda env's own copies, causing
# binary-incompatible segfaults (e.g. loading dplyr). Point it elsewhere.
os.environ["R_LIBS_USER"] = "/nonexistent"

configfile: "config.yaml"

# -------------------
# Files and paths
# -------------------
# raw / preprocessed data
QUALTRICS        = config["raw_data_file"]
RAW              = "raw-data/raw_data.csv"
CONJOINT_LONG    = "data/conjoint_long.csv"
COVARIATES       = "data/covariates.csv"
RESPONDENT_GROUPS = "data/respondent_groups.csv"

# overall conjoint results
CHOICE_OUTPUT = "data/overall_choice_emm.csv"
RATING_OUTPUT = "data/overall_rating_emm.csv"

# policy-type subgroup results
POLICY_CHOICE = "data/policy_choice_emm.csv"
POLICY_RATING = "data/policy_rating_emm.csv"

# net-zero framing results
FRAMING_CHOICE = "data/framing_choice_emm.csv"
FRAMING_RATING = "data/framing_rating_emm.csv"
NZ_CHOICE      = "data/nz_choice_emm.csv"
NZ_RATING      = "data/nz_rating_emm.csv"

# country subgroup results
COUNTRY_CHOICE = "data/country_choice_emm.csv"
COUNTRY_RATING = "data/country_rating_emm.csv"

# durability × cost (willingness-to-pay) interaction results
DURABILITY_COST_CHOICE = "data/durability_cost_choice_emm.csv"
DURABILITY_COST_RATING = "data/durability_cost_rating_emm.csv"

# fuel × cost (willingness-to-pay) interaction results
FUEL_COST_CHOICE = "data/fuel_cost_choice_emm.csv"
FUEL_COST_RATING = "data/fuel_cost_rating_emm.csv"

# climate concern subgroup results
CONCERN_CHOICE = "data/concern_choice_emm.csv"
CONCERN_RATING = "data/concern_rating_emm.csv"

# flyer-type (flying-frequency) subgroup results
FLYER_TYPE_CHOICE = "data/flyer_type_choice_emm.csv"
FLYER_TYPE_RATING = "data/flyer_type_rating_emm.csv"

# net-zero framing effect by country
FRAMING_EFFECT_CHOICE = "data/framing_effect_choice.csv"

# LPA on conjoint choice data (preference profiles)
LPA_CONJOINT_IC      = "data/lpa_conjoint_ic.csv"

# output plots
CHOICE_PLOT              = "output/supp_figs/general_choice_conjoint.png"
RATING_PLOT              = "output/supp_figs/general_rating_conjoint.png"
COUNTRY_CHOICE_PLOT      = "output/country_choice_conjoint.png"
COUNTRY_RATING_PLOT      = "output/supp_figs/country_rating_conjoint.png"
FRAMING_CHOICE_PLOT      = "output/supp_figs/framing_choice_plot.png"
FRAMING_RATING_PLOT      = "output/supp_figs/framing_rating_plot.png"
NZ_SUMMARY               = "output/supp_figs/nz_summary.txt"
POLICY_CHOICE_PLOT       = "output/policy_choice_plot.png"
POLICY_RATING_PLOT       = "output/supp_figs/policy_rating_plot.png"
DURABILITY_COST_CHOICE_PLOT = "output/interact_durability_wtp.png"
DURABILITY_COST_RATING_PLOT = "output/supp_figs/interact_durability_wtp_rating.png"
FUEL_COST_CHOICE_PLOT    = "output/interact_fuel_wtp.png"
FUEL_COST_RATING_PLOT    = "output/supp_figs/interact_fuel_wtp_rating.png"
CONCERN_CHOICE_PLOT      = "output/supp_figs/concern_choice_plot.png"
CONCERN_RATING_PLOT      = "output/supp_figs/concern_rating_plot.png"
FLYER_TYPE_CHOICE_PLOT   = "output/supp_figs/flyer_type_choice_plot.png"
FLYER_TYPE_RATING_PLOT   = "output/supp_figs/flyer_type_rating_plot.png"
FRAMING_EFFECT_CHOICE_PLOT = "output/supp_figs/country_framing_choice.png"
LPA_CONJOINT_IC_PLOT = "output/supp_figs/lpa_conjoint_ic.png"

# -------------------
# Rule modules
# -------------------
include: "rules/preprocess.smk"
include: "rules/analysis.smk"
include: "rules/visualise.smk"

# -------------------
# Final target
# -------------------
rule all:
    input:
        # preprocessed data
        RAW,
        CONJOINT_LONG,
        COVARIATES,
        RESPONDENT_GROUPS,
        # analysis outputs
        CHOICE_OUTPUT,
        RATING_OUTPUT,
        POLICY_CHOICE,
        POLICY_RATING,
        FRAMING_CHOICE,
        FRAMING_RATING,
        NZ_CHOICE,
        NZ_RATING,
        COUNTRY_CHOICE,
        COUNTRY_RATING,
        DURABILITY_COST_CHOICE,
        DURABILITY_COST_RATING,
        FUEL_COST_CHOICE,
        FUEL_COST_RATING,
        CONCERN_CHOICE,
        CONCERN_RATING,
        FLYER_TYPE_CHOICE,
        FLYER_TYPE_RATING,
        FRAMING_EFFECT_CHOICE,
        LPA_CONJOINT_IC,
        # plots
        CHOICE_PLOT,
        RATING_PLOT,
        COUNTRY_CHOICE_PLOT,
        COUNTRY_RATING_PLOT,
        FRAMING_CHOICE_PLOT,
        FRAMING_RATING_PLOT,
        NZ_SUMMARY,
        POLICY_CHOICE_PLOT,
        POLICY_RATING_PLOT,
        DURABILITY_COST_CHOICE_PLOT,
        DURABILITY_COST_RATING_PLOT,
        FUEL_COST_CHOICE_PLOT,
        FUEL_COST_RATING_PLOT,
        CONCERN_CHOICE_PLOT,
        CONCERN_RATING_PLOT,
        FLYER_TYPE_CHOICE_PLOT,
        FLYER_TYPE_RATING_PLOT,
        FRAMING_EFFECT_CHOICE_PLOT,
        LPA_CONJOINT_IC_PLOT,
