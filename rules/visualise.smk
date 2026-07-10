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


rule plot_durability_cost:
    input:
        choice = DURABILITY_COST_CHOICE,
        rating = DURABILITY_COST_RATING
    output:
        choice_plot = DURABILITY_COST_CHOICE_PLOT,
        rating_plot = DURABILITY_COST_RATING_PLOT
    script:
        "../visualise/durability_cost_plot.R"


rule plot_fuel_cost:
    input:
        choice = FUEL_COST_CHOICE,
        rating = FUEL_COST_RATING
    output:
        choice_plot = FUEL_COST_CHOICE_PLOT,
        rating_plot = FUEL_COST_RATING_PLOT
    script:
        "../visualise/fuel_cost_plot.R"


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


rule plot_framing_effect:
    input:
        choice = FRAMING_EFFECT_CHOICE
    output:
        choice_plot = FRAMING_EFFECT_CHOICE_PLOT
    script:
        "../visualise/framing_effect_plot.R"
