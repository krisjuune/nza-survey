rule valid_responses:
    input:
        QUALTRICS
    output:
        RAW
    script:
        "scripts/preprocessing/valid_responses.R"


rule reshape_conjoint:
    input:
        RAW
    output:
        CONJOINT_LONG
    script:
        "scripts/preprocessing/reshape_conjoint.R"


rule covariates:
    input:
        RAW
    output:
        COVARIATES
    script:
        "scripts/preprocessing/respondent_data.R"


rule respondent_groups:
    input:
        COVARIATES
    output:
        groups = RESPONDENT_GROUPS
    script:
        "scripts/preprocessing/respondent_groups.R"
