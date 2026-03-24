# -------------------
# Files and paths
# -------------------
RAW = "raw-data/GBF_150326.csv"
CONJOINT_LONG = "data/conjoint_long.csv"
CHOICE_OUTPUT = "data/choice_results.csv"
RATING_OUTPUT = "data/rating_results.csv"

# -------------------
# Final target
# -------------------
rule all:
    input:
        CHOICE_OUTPUT,
        RATING_OUTPUT

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
