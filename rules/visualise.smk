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
        "../visualise/conjoint_general_plot.R"


rule plot_nz_framing:
    input:
        framing_choice = FRAMING_CHOICE,
        framing_rating = FRAMING_RATING,
        nz_choice      = NZ_CHOICE,
        nz_rating      = NZ_RATING
    output:
        framing_choice_plot = FRAMING_CHOICE_PLOT,
        framing_rating_plot = FRAMING_RATING_PLOT,
        nz_summary          = NZ_SUMMARY
    script:
        "../visualise/nz_framing_plot.R"


rule plot_policy_types:
    input:
        choice = POLICY_CHOICE,
        rating = POLICY_RATING
    output:
        choice_plot = POLICY_CHOICE_PLOT,
        rating_plot = POLICY_RATING_PLOT
    script:
        "../visualise/policy_plot.R"


rule plot_interact_combined:
    input:
        durability_choice = DURABILITY_COST_CHOICE,
        fuel_choice       = FUEL_COST_CHOICE
    output:
        plot = INTERACT_COMBINED_PLOT
    script:
        "../visualise/interact_combined_plot.R"


rule plot_concern_group:
    input:
        choice = CONCERN_CHOICE,
        rating = CONCERN_RATING
    output:
        choice_plot = CONCERN_CHOICE_PLOT,
        rating_plot = CONCERN_RATING_PLOT
    script:
        "../visualise/concern_group_plot.R"


rule plot_flyer_type:
    input:
        choice = FLYER_TYPE_CHOICE,
        rating = FLYER_TYPE_RATING
    output:
        choice_plot = FLYER_TYPE_CHOICE_PLOT,
        rating_plot = FLYER_TYPE_RATING_PLOT
    script:
        "../visualise/flyer_type_plot.R"


rule plot_lpa_conjoint_ic:
    input:
        ic = LPA_CONJOINT_IC
    output:
        ic_plot = LPA_CONJOINT_IC_PLOT
    script:
        "../visualise/lpa_conjoint_ic_plot.R"


rule plot_lpa_conjoint_profiles:
    input:
        means = LPA_CONJOINT_MEANS
    output:
        profile_plot = LPA_CONJOINT_PROFILE_PLOT
    script:
        "../visualise/lpa_conjoint_profile_plot.R"


rule plot_profiles_combined:
    input:
        choice  = POLICY_BY_PROFILE_CHOICE,
        results = PROFILE_PREDICTORS
    output:
        plot = PROFILES_COMBINED_PLOT
    script:
        "../visualise/profiles_combined_plot.R"


rule plot_policy_breakeven_combined:
    input:
        grid           = POLICY_BREAKEVEN_COUNTRY_GRID,
        cost_scenarios = AIRFARE_COST_SCENARIOS
    output:
        combined = POLICY_BREAKEVEN_COMBINED
    script:
        "../visualise/policy_breakeven_combined_plot.R"


rule plot_profile_predictors:
    input:
        results = PROFILE_PREDICTORS
    output:
        plot = PROFILE_PREDICTORS_PLOT
    script:
        "../visualise/profile_predictors_plot.R"


rule plot_policy_by_profile:
    input:
        choice = POLICY_BY_PROFILE_CHOICE
    output:
        plot = POLICY_BY_PROFILE_PLOT
    script:
        "../visualise/policy_by_profile_plot.R"


rule plot_framing_effect:
    input:
        choice = FRAMING_EFFECT_CHOICE
    output:
        choice_plot = FRAMING_EFFECT_CHOICE_PLOT
    script:
        "../visualise/framing_effect_plot.R"
