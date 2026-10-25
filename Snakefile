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

# Main figures are also saved as vector PDF when save_pdf is on (config.yaml or
# --config save_pdf=true); supplementary figures are always PNG only. The PDF
# is declared as an extra output of each main-figure rule, so switching
# save_pdf on makes Snakemake rebuild exactly those figures.
SAVE_PDF = str(config.get("save_pdf", False)).lower() in ("true", "1", "yes")

def pdf_output(png, name="pdf"):
    """{name: <png path as .pdf>} for a main figure when SAVE_PDF, else {}."""
    return {name: png[:-len(".png")] + ".pdf"} if SAVE_PDF else {}

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

# willingness to pay (conditional logit, linear price) by country
WTP_COUNTRY = "data/wtp_country.csv"
WTP_SUMMARY = "output/text-files/wtp_summary.txt"

# robustness checks (sample variants, income weighting, internal validation)
ROBUSTNESS_VARIANTS   = "data/robustness_country_choice.csv"
ROBUSTNESS_VALIDATION = "data/robustness_validation.csv"
ROBUSTNESS_SUMMARY    = "output/text-files/robustness_summary.txt"
ROBUSTNESS_SAMPLE_PLOT = "output/supp-figs/robustness_sample.png"
ROBUSTNESS_INCOME_PLOT = "output/supp-figs/robustness_income.png"
VALIDATION_STATED_PLOT = "output/supp-figs/validation_stated.png"
VALIDATION_RANKING_PLOT = "output/supp-figs/validation_ranking.png"

# climate concern subgroup results
CONCERN_CHOICE = "data/concern_choice_emm.csv"
CONCERN_RATING = "data/concern_rating_emm.csv"

# flyer-type (flying-frequency) subgroup results
FLYER_TYPE_CHOICE = "data/flyer_type_choice_emm.csv"
FLYER_TYPE_RATING = "data/flyer_type_rating_emm.csv"

# net-zero framing effect by country
FRAMING_EFFECT_CHOICE = "data/framing_effect_choice.csv"

# LPA on conjoint choice data (preference profiles)
LPA_CONJOINT_IC       = "data/lpa_conjoint_ic.csv"
LPA_CONJOINT_PROFILES = "data/lpa_conjoint_profiles.csv"
LPA_CONJOINT_MEANS    = "data/lpa_conjoint_means.csv"

# Policy preference by LPA profile
POLICY_BY_PROFILE_CHOICE = "data/policy_by_profile_choice.csv"

# Multinomial predictors of LPA profile membership
PROFILE_PREDICTORS = "data/profile_predictors.csv"
PROFILE_PREDICTORS_AME = "data/profile_predictors_ame.csv"

# Compensating price differential: GBF vs SAF_ETS breakeven
POLICY_BREAKEVEN             = "data/policy_breakeven.csv"
POLICY_BREAKEVEN_GRID        = "data/policy_breakeven_grid.csv"
POLICY_BREAKEVEN_INTERACTION = "output/text-files/policy_breakeven_interaction.txt"

# output plots
GENERAL_CONJOINT_PLOT    = "output/supp-figs/general_conjoint.png"
COUNTRY_CHOICE_PLOT      = "output/country_choice_conjoint.png"
COUNTRY_RATING_PLOT      = "output/supp-figs/country_rating_conjoint.png"
FRAMING_RESULTS_PLOT     = "output/supp-figs/framing_results.png"
NZ_SUMMARY               = "output/text-files/nz_summary.txt"
POLICY_CHOICE_PLOT       = "output/policy_choice_plot.png"
INTERACT_COMBINED_PLOT   = "output/supp-figs/interact_combined_plot.png"
WTP_PLOT                 = "output/wtp_plot.png"
CONCERN_CHOICE_PLOT      = "output/supp-figs/concern_choice_plot.png"
CONCERN_RATING_PLOT      = "output/supp-figs/concern_rating_plot.png"
FLYER_TYPE_CHOICE_PLOT   = "output/supp-figs/flyer_type_choice_plot.png"
FLYER_TYPE_RATING_PLOT   = "output/supp-figs/flyer_type_rating_plot.png"
FRAMING_EFFECT_CHOICE_PLOT = "output/supp-figs/country_framing_choice.png"
LPA_CONJOINT_IC_PLOT      = "output/supp-figs/lpa_conjoint_ic.png"
LPA_CONJOINT_PROFILE_PLOT = "output/supp-figs/lpa_conjoint_profiles.png"
PROFILE_PREDICTORS_PLOT   = "output/supp-figs/lpa_profile_predictors.png"
PROFILES_COMBINED_PLOT       = "output/profiles_combined_plot.png"
# Country-level cost-preference breakeven
AIRFARE_RAW                      = config["cost_files"][config["cost_source"]]
AIRFARE_COSTS                    = "data/airfare_costs.csv"
POLICY_BREAKEVEN_COUNTRY_GRID    = "data/policy_breakeven_country_grid.csv"
POLICY_BREAKEVEN_COUNTRY_SUMMARY = "data/policy_breakeven_country_summary.csv"
POLICY_BREAKEVEN_COUNTRY_TXT     = "output/text-files/policy_breakeven_country.txt"
POLICY_BREAKEVEN_COMBINED        = "output/policy_breakeven_combined.png"

# supplementary tables (LaTeX)
SAMPLE_TARGETS = "raw-data/sample_targets.csv"
SAMPLE_TABLE   = "output/supp-tables/sample_summary.tex"

# Main-paper figures (PNG, plus PDF when save_pdf is on); everything under
# output/supp-figs/ is supplementary and PNG only.
MAIN_FIGURES = [
    COUNTRY_CHOICE_PLOT,
    WTP_PLOT,
    POLICY_CHOICE_PLOT,
    PROFILES_COMBINED_PLOT,
    POLICY_BREAKEVEN_COMBINED,
]

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
        LPA_CONJOINT_PROFILES,
        LPA_CONJOINT_MEANS,
        POLICY_BY_PROFILE_CHOICE,
        PROFILE_PREDICTORS,
        PROFILE_PREDICTORS_AME,
        POLICY_BREAKEVEN,
        POLICY_BREAKEVEN_GRID,
        POLICY_BREAKEVEN_INTERACTION,
        AIRFARE_COSTS,
        POLICY_BREAKEVEN_COUNTRY_GRID,
        POLICY_BREAKEVEN_COUNTRY_SUMMARY,
        POLICY_BREAKEVEN_COUNTRY_TXT,
        # plots
        GENERAL_CONJOINT_PLOT,
        COUNTRY_CHOICE_PLOT,
        COUNTRY_RATING_PLOT,
        FRAMING_RESULTS_PLOT,
        NZ_SUMMARY,
        POLICY_CHOICE_PLOT,
        INTERACT_COMBINED_PLOT,
        WTP_COUNTRY,
        WTP_SUMMARY,
        WTP_PLOT,
        ROBUSTNESS_VARIANTS,
        ROBUSTNESS_VALIDATION,
        ROBUSTNESS_SUMMARY,
        ROBUSTNESS_SAMPLE_PLOT,
        ROBUSTNESS_INCOME_PLOT,
        VALIDATION_STATED_PLOT,
        VALIDATION_RANKING_PLOT,
        CONCERN_CHOICE_PLOT,
        CONCERN_RATING_PLOT,
        FLYER_TYPE_CHOICE_PLOT,
        FLYER_TYPE_RATING_PLOT,
        FRAMING_EFFECT_CHOICE_PLOT,
        LPA_CONJOINT_IC_PLOT,
        LPA_CONJOINT_PROFILE_PLOT,
        PROFILE_PREDICTORS_PLOT,
        PROFILES_COMBINED_PLOT,
        POLICY_BREAKEVEN_COMBINED,
        # supplementary tables
        SAMPLE_TABLE,
        # vector PDFs of the main figures (only when save_pdf is on)
        *[pdf for png in MAIN_FIGURES for pdf in pdf_output(png).values()],
