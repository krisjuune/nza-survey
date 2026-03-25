# -------------------
# Files and paths
# -------------------
RAW = "raw-data/GBF_250326.csv"
CONJOINT_LONG = "data/conjoint_long.csv"
COVARIATES = "data/covariates.csv"

CHOICE_OUTPUT = "data/overall_choice_emm.csv"
RATING_OUTPUT = "data/overall_rating_emm.csv"

CHOICE_PLOT = "output/general_choice_conjoint.png"
RATING_PLOT = "output/general_rating_conjoint.png"

# -------------------
# Final target
# -------------------
rule all:
    input:
        CONJOINT_LONG,
        COVARIATES,
        CHOICE_OUTPUT,
        RATING_OUTPUT,
        CHOICE_PLOT,
        RATING_PLOT

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
# Rule 10: Plot basic analysis
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
