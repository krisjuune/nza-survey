# -------------------
# Files and paths
# -------------------
RAW = "raw-data/GBF_150326.csv"
CONJOINT_LONG = "data/conjoint_long.csv"

CHOICE_OUTPUT = "data/choice_results.csv"
RATING_OUTPUT = "data/rating_results.csv"

CHOICE_PLOT = "output/general_choice_conjoint.png"
RATING_PLOT = "output/general_rating_conjoint.png"

# -------------------
# Final target
# -------------------
rule all:
    input:
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
# Rule 2: Run conjoint analysis
# -------------------
rule conjoint_analysis:
    input:
        CONJOINT_LONG
    output:
        CHOICE_OUTPUT
    script:
        "scripts/analysis/02_conjoint_analysis.R"

# -------------------
# Rule 3: Conjoint rating analysis
# -------------------
rule conjoint_rating:
    input:
        CONJOINT_LONG
    output:
        RATING_OUTPUT
    script:
        "scripts/analysis/03_conjoint_rating.R"

# -------------------
# Rule 4: Plot basic analysis
# -------------------
rule plot_conjoint_general:
    input:
        choice = CHOICE_OUTPUT,
        rating = RATING_OUTPUT
    output:
        choice_plot = CHOICE_PLOT,
        rating_plot = RATING_PLOT
    script:
        "visualise/04_conjoint_general.R"