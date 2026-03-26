# -------------------
# Files and paths
# -------------------
# data
QUALTRICS = "raw-data/GBF_250326.csv"
RAW = "raw-data/raw_data.csv"
CONJOINT_LONG = "data/conjoint_long.csv"
COVARIATES = "data/covariates.csv"

# results data
CHOICE_OUTPUT = "data/overall_choice_emm.csv"
RATING_OUTPUT = "data/overall_rating_emm.csv"
FRAMING_CHOICE = "data/framing_choice_emm.csv"
FRAMING_RATING = "data/framing_rating_emm.csv"
NZ_CHOICE = "data/nz_choice_emm.csv"
NZ_RATING = "data/nz_rating_emm.csv"

# output plots and text files
CHOICE_PLOT = "output/general_choice_conjoint.png"
RATING_PLOT = "output/general_rating_conjoint.png"
FRAMING_CHOICE_PLOT = "output/framing_choice_plot.png"
FRAMING_RATING_PLOT = "output/framing_rating_plot.png"
NZ_SUMMARY = "output/nz_summary.txt"

# -------------------
# Final target
# -------------------
rule all:
    input:
        RAW,
        CONJOINT_LONG,
        COVARIATES,
        CHOICE_OUTPUT,
        RATING_OUTPUT,
        FRAMING_CHOICE,
        FRAMING_RATING,
        NZ_CHOICE,
        NZ_RATING,
        CHOICE_PLOT,
        RATING_PLOT,
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
        "scripts/preprocessing/00_valid_responses.R"

# -------------------
# Rule 1: Reshape conjoint data
# -------------------
rule reshape_conjoint:
    input:
        RAW
    output:
        CONJOINT_LONG
    script:
        "scripts/preprocessing/01_reshape_conjoint.R"

# -------------------
# Rule 2: Get covariate data
# -------------------
rule covariates:
    input:
        RAW
    output:
        COVARIATES
    script:
        "scripts/preprocessing/02_respondent_data.R"

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
        "scripts/analysis/03_conjoint_analysis.R"

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
        "scripts/analysis/04_nz_framing.R"

# -------------------
# Rule 10: Plot basic conjoint results
# -------------------
rule plot_conjoint_general:
    input:
        choice = CHOICE_OUTPUT,
        rating = RATING_OUTPUT
    output:
        choice_plot = CHOICE_PLOT,
        rating_plot = RATING_PLOT
    script:
        "visualise/10_conjoint_general.R"

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
        "visualise/11_nz_framing.R"