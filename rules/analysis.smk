rule conjoint_analysis:
    input:
        CONJOINT_LONG
    output:
        choice = CHOICE_OUTPUT,
        rating = RATING_OUTPUT
    script:
        "../scripts/analysis/conjoint_analysis.R"


rule policy_type_analysis:
    input:
        CONJOINT_LONG
    output:
        choice = POLICY_CHOICE,
        rating = POLICY_RATING
    script:
        "../scripts/analysis/policy_types.R"


rule nz_framing_analysis:
    input:
        CONJOINT_LONG
    output:
        framing_choice = FRAMING_CHOICE,
        framing_rating = FRAMING_RATING,
        nz_choice      = NZ_CHOICE,
        nz_rating      = NZ_RATING
    script:
        "../scripts/analysis/nz_framing.R"


rule country_analysis:
    input:
        conjoint   = CONJOINT_LONG,
        covariates = COVARIATES
    output:
        choice = COUNTRY_CHOICE,
        rating = COUNTRY_RATING
    script:
        "../scripts/analysis/country.R"


rule durability_cost_analysis:
    input:
        conjoint   = CONJOINT_LONG,
        covariates = COVARIATES
    output:
        choice = DURABILITY_COST_CHOICE,
        rating = DURABILITY_COST_RATING
    script:
        "../scripts/analysis/durability_cost.R"


rule fuel_cost_analysis:
    input:
        conjoint   = CONJOINT_LONG,
        covariates = COVARIATES
    output:
        choice = FUEL_COST_CHOICE,
        rating = FUEL_COST_RATING
    script:
        "../scripts/analysis/fuel_cost.R"


rule concern_group_analysis:
    input:
        conjoint   = CONJOINT_LONG,
        groups     = RESPONDENT_GROUPS,
        covariates = COVARIATES
    output:
        choice = CONCERN_CHOICE,
        rating = CONCERN_RATING
    script:
        "../scripts/analysis/concern_group.R"


rule flyer_type_analysis:
    input:
        conjoint = CONJOINT_LONG,
        groups   = RESPONDENT_GROUPS
    output:
        choice = FLYER_TYPE_CHOICE,
        rating = FLYER_TYPE_RATING
    script:
        "../scripts/analysis/flyer_type.R"


rule lpa_conjoint:
    input:
        conjoint = CONJOINT_LONG
    output:
        ic       = LPA_CONJOINT_IC,
        profiles = LPA_CONJOINT_PROFILES,
        means    = LPA_CONJOINT_MEANS
    script:
        "../scripts/analysis/lpa_conjoint.R"


rule profile_predictors_analysis:
    input:
        profiles   = LPA_CONJOINT_PROFILES,
        groups     = RESPONDENT_GROUPS,
        covariates = COVARIATES
    output:
        results = PROFILE_PREDICTORS,
        ame     = PROFILE_PREDICTORS_AME
    script:
        "../scripts/analysis/profile_predictors.R"


rule policy_by_profile_analysis:
    input:
        conjoint = CONJOINT_LONG,
        profiles = LPA_CONJOINT_PROFILES
    output:
        choice = POLICY_BY_PROFILE_CHOICE
    script:
        "../scripts/analysis/policy_by_profile.R"


rule policy_breakeven_analysis:
    input:
        conjoint = CONJOINT_LONG
    output:
        results     = POLICY_BREAKEVEN,
        grid        = POLICY_BREAKEVEN_GRID,
        interaction = POLICY_BREAKEVEN_INTERACTION
    script:
        "../scripts/analysis/policy_breakeven.R"


rule policy_breakeven_country_analysis:
    input:
        conjoint   = CONJOINT_LONG,
        covariates = COVARIATES,
        costs      = AIRFARE_COSTS
    output:
        grid    = POLICY_BREAKEVEN_COUNTRY_GRID,
        summary = POLICY_BREAKEVEN_COUNTRY_SUMMARY,
        txt     = POLICY_BREAKEVEN_COUNTRY_TXT
    script:
        "../scripts/analysis/policy_breakeven_country.R"


rule framing_effect_analysis:
    input:
        conjoint   = CONJOINT_LONG,
        covariates = COVARIATES
    output:
        choice = FRAMING_EFFECT_CHOICE
    script:
        "../scripts/analysis/framing_effect.R"


rule wtp_analysis:
    input:
        conjoint   = CONJOINT_LONG,
        covariates = COVARIATES
    output:
        wtp = WTP_COUNTRY,
        txt = WTP_SUMMARY
    script:
        "../scripts/analysis/wtp.R"


rule robustness_analysis:
    input:
        conjoint       = CONJOINT_LONG,
        covariates     = COVARIATES,
        country_choice = COUNTRY_CHOICE,
        country_rating = COUNTRY_RATING,
        overall_choice = CHOICE_OUTPUT,
        overall_rating = RATING_OUTPUT,
        policy_choice  = POLICY_CHOICE,
        policy_rating  = POLICY_RATING
    output:
        variants   = ROBUSTNESS_VARIANTS,
        validation = ROBUSTNESS_VALIDATION,
        txt        = ROBUSTNESS_SUMMARY
    script:
        "../scripts/analysis/robustness.R"


rule sample_table:
    input:
        raw     = RAW,
        targets = SAMPLE_TARGETS
    output:
        table = SAMPLE_TABLE
    script:
        "../scripts/analysis/sample_table.R"
