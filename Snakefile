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
# data
QUALTRICS = config["raw_data_file"]
RAW = "raw-data/raw_data.csv"
CONJOINT_LONG = "data/conjoint_long.csv"
COVARIATES = "data/covariates.csv"
RESPONDENT_GROUPS = "data/respondent_groups.csv"
LPA_SUMMARY = "output/supp_figs/lpa_profile_summary.txt"

# results data
POLICY_CHOICE = "data/policy_choice_emm.csv"
POLICY_RATING = "data/policy_rating_emm.csv"
POLICY_CHOICE_PLOT = "output/policy_choice_plot.png"
POLICY_RATING_PLOT = "output/supp_figs/policy_rating_plot.png"
CHOICE_OUTPUT = "data/overall_choice_emm.csv"
RATING_OUTPUT = "data/overall_rating_emm.csv"
FRAMING_CHOICE = "data/framing_choice_emm.csv"
FRAMING_RATING = "data/framing_rating_emm.csv"
NZ_CHOICE = "data/nz_choice_emm.csv"
NZ_RATING = "data/nz_rating_emm.csv"

# country subgroup results
COUNTRY_CHOICE = "data/country_choice_emm.csv"
COUNTRY_RATING = "data/country_rating_emm.csv"

# durability x cost (willingness-to-pay) interaction results
DURABILITY_COST_CHOICE = "data/durability_cost_choice_emm.csv"
DURABILITY_COST_RATING = "data/durability_cost_rating_emm.csv"
DURABILITY_COST_CHOICE_PLOT = "output/durability_cost_choice_plot.png"
DURABILITY_COST_RATING_PLOT = "output/supp_figs/durability_cost_rating_plot.png"

# flyer-type (flying-frequency) subgroup results
FLYER_TYPE_CHOICE = "data/flyer_type_choice_emm.csv"
FLYER_TYPE_RATING = "data/flyer_type_rating_emm.csv"
FLYER_TYPE_CHOICE_PLOT = "output/flyer_type_choice_plot.png"
FLYER_TYPE_RATING_PLOT = "output/supp_figs/flyer_type_rating_plot.png"

# net-zero framing effect by country (probability shift per attribute level)
FRAMING_EFFECT_CHOICE = "data/framing_effect_choice.csv"
FRAMING_EFFECT_CHOICE_PLOT = "output/country_framing_choice.png"

# LPA model-selection diagnostics (AIC/BIC/SABIC/ICL/AWE across G, raw vs
# within-person-centered items)
LPA_FIT_INDICES = "data/lpa_fit_indices.csv"
LPA_FIT_INDICES_PLOT = "output/supp_figs/lpa_fit_indices_plot.png"

# output plots and text files
CHOICE_PLOT = "output/supp_figs/general_choice_conjoint.png"
RATING_PLOT = "output/supp_figs/general_rating_conjoint.png"
COUNTRY_CHOICE_PLOT = "output/country_choice_conjoint.png"
COUNTRY_RATING_PLOT = "output/supp_figs/country_rating_conjoint.png"
FRAMING_CHOICE_PLOT = "output/supp_figs/framing_choice_plot.png"
FRAMING_RATING_PLOT = "output/supp_figs/framing_rating_plot.png"
NZ_SUMMARY = "output/supp_figs/nz_summary.txt"

# -------------------
# Final target
# -------------------
rule all:
    input:
        RAW,
        CONJOINT_LONG,
        COVARIATES,
        RESPONDENT_GROUPS,
        LPA_SUMMARY,
        POLICY_CHOICE,
        POLICY_RATING,
        POLICY_CHOICE_PLOT,
        POLICY_RATING_PLOT,
        CHOICE_OUTPUT,
        RATING_OUTPUT,
        FRAMING_CHOICE,
        FRAMING_RATING,
        NZ_CHOICE,
        NZ_RATING,
        COUNTRY_CHOICE,
        COUNTRY_RATING,
        DURABILITY_COST_CHOICE,
        DURABILITY_COST_RATING,
        DURABILITY_COST_CHOICE_PLOT,
        DURABILITY_COST_RATING_PLOT,
        FLYER_TYPE_CHOICE,
        FLYER_TYPE_RATING,
        FLYER_TYPE_CHOICE_PLOT,
        FLYER_TYPE_RATING_PLOT,
        FRAMING_EFFECT_CHOICE,
        FRAMING_EFFECT_CHOICE_PLOT,
        LPA_FIT_INDICES,
        LPA_FIT_INDICES_PLOT,
        CHOICE_PLOT,
        RATING_PLOT,
        COUNTRY_CHOICE_PLOT,
        COUNTRY_RATING_PLOT,
        FRAMING_CHOICE_PLOT,
        FRAMING_RATING_PLOT,
        NZ_SUMMARY

# -------------------
# Rule 0: Get valid responses
# -------------------
rule valid_responses:
    input:
        QUALTRICS
    output:
        RAW
    script:
        "scripts/preprocessing/valid_responses.R"

# -------------------
# Rule 1: Reshape conjoint data
# -------------------
rule reshape_conjoint:
    input:
        RAW
    output:
        CONJOINT_LONG
    script:
        "scripts/preprocessing/reshape_conjoint.R"

# -------------------
# Rule 2: Get covariate data
# -------------------
rule covariates:
    input:
        RAW
    output:
        COVARIATES
    script:
        "scripts/preprocessing/respondent_data.R"

# -------------------
# Rule 2b: Derive respondent groups (flyer type, concern score, LPA profile)
# -------------------
rule respondent_groups:
    input:
        COVARIATES
    output:
        groups      = RESPONDENT_GROUPS,
        lpa_summary = LPA_SUMMARY
    script:
        "scripts/preprocessing/respondent_groups.R"

# -------------------
# Rule 2c: LPA model-selection diagnostics (informs the profile count used in
# respondent_groups)
# -------------------
rule lpa_fit_indices:
    input:
        COVARIATES
    output:
        LPA_FIT_INDICES
    script:
        "scripts/analysis/lpa_fit_indices.R"

# -------------------
# Rule 3: Run conjoint analysis
# -------------------
rule conjoint_analysis:
    input:
        CONJOINT_LONG
    output:
        choice = CHOICE_OUTPUT,
        rating = RATING_OUTPUT
    script:
        "scripts/analysis/conjoint_analysis.R"

# -------------------
# Rule 6: Policy type analysis
# -------------------
rule policy_type_analysis:
    input:
        CONJOINT_LONG
    output:
        choice = POLICY_CHOICE,
        rating = POLICY_RATING
    script:
        "scripts/analysis/policy_types.R"

# -------------------
# Rule 4: Run net-zero framing analysis
# -------------------
rule nz_framing_analysis:
    input:
        CONJOINT_LONG
    output:
        framing_choice = FRAMING_CHOICE,
        framing_rating = FRAMING_RATING,
        nz_choice = NZ_CHOICE,
        nz_rating = NZ_RATING
    script:
        "scripts/analysis/nz_framing.R"

# -------------------
# Rule 5: Country subgroup analysis
# -------------------
rule country_analysis:
    input:
        conjoint   = CONJOINT_LONG,
        covariates = COVARIATES
    output:
        choice = COUNTRY_CHOICE,
        rating = COUNTRY_RATING
    script:
        "scripts/analysis/country.R"

# -------------------
# Rule 7: Durability x cost interaction analysis (willingness-to-pay)
# -------------------
rule durability_cost_analysis:
    input:
        conjoint   = CONJOINT_LONG,
        covariates = COVARIATES
    output:
        choice = DURABILITY_COST_CHOICE,
        rating = DURABILITY_COST_RATING
    script:
        "scripts/analysis/durability_cost.R"

# -------------------
# Rule 8: Flyer-type (flying-frequency) subgroup analysis
# -------------------
rule flyer_type_analysis:
    input:
        conjoint = CONJOINT_LONG,
        groups   = RESPONDENT_GROUPS
    output:
        choice = FLYER_TYPE_CHOICE,
        rating = FLYER_TYPE_RATING
    script:
        "scripts/analysis/flyer_type.R"

# -------------------
# Rule 9: Net-zero framing effect by country
# -------------------
rule framing_effect_analysis:
    input:
        conjoint   = CONJOINT_LONG,
        covariates = COVARIATES
    output:
        choice = FRAMING_EFFECT_CHOICE
    script:
        "scripts/analysis/framing_effect.R"

# -------------------
# Rule 12: Plot policy type results
# -------------------
rule plot_policy_types:
    input:
        choice = POLICY_CHOICE,
        rating = POLICY_RATING
    output:
        choice_plot = POLICY_CHOICE_PLOT,
        rating_plot = POLICY_RATING_PLOT
    script:
        "visualise/policy_plot.R"

# -------------------
# Rule 10: Plot basic conjoint results
# -------------------
rule plot_conjoint_general:
    input:
        choice         = CHOICE_OUTPUT,
        rating         = RATING_OUTPUT,
        country_choice = COUNTRY_CHOICE,
        country_rating = COUNTRY_RATING
    output:
        choice_plot         = CHOICE_PLOT,
        rating_plot         = RATING_PLOT,
        country_choice_plot = COUNTRY_CHOICE_PLOT,
        country_rating_plot = COUNTRY_RATING_PLOT
    script:
        "visualise/conjoint_general_plot.R"

# -------------------
# Rule 11: Plot framing results and NZ summary
# -------------------
rule plot_nz_framing:
    input:
        framing_choice = FRAMING_CHOICE,
        framing_rating = FRAMING_RATING,
        nz_choice = NZ_CHOICE,
        nz_rating = NZ_RATING
    output:
        framing_choice_plot = FRAMING_CHOICE_PLOT,
        framing_rating_plot = FRAMING_RATING_PLOT,
        nz_summary = NZ_SUMMARY
    script:
        "visualise/nz_framing_plot.R"

# -------------------
# Rule 13: Plot durability x cost interaction (willingness-to-pay)
# -------------------
rule plot_durability_cost:
    input:
        choice = DURABILITY_COST_CHOICE,
        rating = DURABILITY_COST_RATING
    output:
        choice_plot = DURABILITY_COST_CHOICE_PLOT,
        rating_plot = DURABILITY_COST_RATING_PLOT
    script:
        "visualise/durability_cost_plot.R"

# -------------------
# Rule 14: Plot flyer-type subgroup results
# -------------------
rule plot_flyer_type:
    input:
        choice = FLYER_TYPE_CHOICE,
        rating = FLYER_TYPE_RATING
    output:
        choice_plot = FLYER_TYPE_CHOICE_PLOT,
        rating_plot = FLYER_TYPE_RATING_PLOT
    script:
        "visualise/flyer_type_plot.R"

# -------------------
# Rule 15: Plot net-zero framing effect by country
# -------------------
rule plot_framing_effect:
    input:
        choice = FRAMING_EFFECT_CHOICE
    output:
        choice_plot = FRAMING_EFFECT_CHOICE_PLOT
    script:
        "visualise/framing_effect_plot.R"

# -------------------
# Rule 16: Plot LPA model-selection diagnostics
# -------------------
rule plot_lpa_fit_indices:
    input:
        LPA_FIT_INDICES
    output:
        LPA_FIT_INDICES_PLOT
    script:
        "visualise/lpa_fit_indices_plot.R"
