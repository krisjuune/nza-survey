# nza-survey

Analysis pipeline for a discrete-choice experiment on aviation net-zero
policy preferences, fielded across six countries (Australia, Brazil, Germany,
Kenya, UAE, Vietnam). Respondents chose between pairs of policy bundles
varying in fuel type, offsetting activity, durability of offsets, responsible
actor, and ticket cost increase, and rated each bundle on a 5-point Likert
scale.

The pipeline runs with [Snakemake](https://snakemake.github.io/):
raw data file -> reshaped conjoint/respondent data -> mixed-effects
models (`lme4`) -> marginal means (`emmeans`) -> figures (`ggplot2`).

## Setup

Create and activate the conda environment:

```sh
conda env create -f environment.yaml
conda activate nza-survey
```

Set the path to the raw data file in `config.yaml`
(`raw_data_file`). `use_test_data: true` runs the pipeline on Qualtrics
preview/test responses instead of real survey data.

## Running the pipeline

```sh
snakemake --cores 1
```

Run a single target (and its dependencies) instead of the whole pipeline,
e.g.:

```sh
snakemake output/country_choice_conjoint.png --cores 1
```

Each R script can also be run standalone (outside Snakemake) for
development. Every script falls back to default `here()`, relative paths
under `data/`/`output/` when no `snakemake` object is injected.

## Repository structure

```
Snakefile                  pipeline definition (rules, file paths, rule all)
config.yaml                raw data path and test/real data switch
environment.yaml           conda environment spec

scripts/
  preprocessing/           Qualtrics export -> conjoint_long.csv / covariates.csv /
                            respondent_groups.csv
  analysis/                mixed-effects models + emmeans, one script per analysis
  fielding/                ad hoc data-quality checks, not part of the pipeline
  survey-implementation/   Qualtrics JS/HTML/CSS for the conjoint task itself

visualise/                 ggplot2 figure scripts, one per analysis
  theme_clean.R             shared theme, color palette, and plot helpers

data/                       intermediate CSVs (reshaped data, emmeans tables)
output/                     final figures (PNG) and text summaries
raw-data/                   raw Qualtrics export (not tracked)
```

## Pipeline stages

1. **Preprocessing** (`scripts/preprocessing/`): validate raw responses,
   reshape the conjoint task into long format (`conjoint_long.csv`), extract
   respondent-level covariates (`covariates.csv`), and derive subgroup
   variables - flyer type, aviation climate concern tercile, latent-profile
   pathway class (`respondent_groups.csv`).
2. **Analysis** (`scripts/analysis/`): `glmer`/`lmer` models with `(1 | id)`
   random intercepts for repeated measures, summarized via `emmeans`
   marginal means (choice probability and rating scale). Includes the main
   conjoint effects, country and flyer-type subgroups, net-zero framing
   effects, the durability x cost interaction, policy-type bundles, and LPA
   model-selection diagnostics.
3. **Visualisation** (`visualise/`): one figure script per analysis, sharing
   a common theme and color palette (`theme_clean.R`) - muted per-attribute
   colors, dashed neutral/reference lines, and consistent country/attribute
   ordering across figures.

## Environment notes

The `Snakefile` pins `PATH` and `R_LIBS_USER` at the top so R scripts always
resolve to the conda environment's R and packages, even when a system R
installation is also present on the machine (a mismatch otherwise causes
missing-package errors or segfaults from loading binary-incompatible
packages out of a shared personal library).

## Setting up the experiment in Qualtrics

`scripts/survey-implementation/` includes the JS/HTML/CSS pasted into Qualtrics
question editors to implement the conjoint choice task and the net-zero
framing manipulation inside a Loop & Merge block (6 loop iterations = 6
choice tasks). Survey flow/branching itself (e.g. randomizing respondents
into the framing condition) is configured in Qualtrics and is available in
the OSF preregistration.

- **`choice_experiment.js`** - JavaScript question, runs once on load (before
  the loop). Switches on `Q_Language` (DE/PT-BR/AR/VI, default EN) to set
  language-specific attribute names/descriptions and value labels, then
  randomly draws and shuffles the 5 attribute levels for each alternative (A
  and B) of all 6 tasks, storing both the raw codes (`taskN_fuel1_code`, ...)
  and display labels (`taskN_fuel1`, ...) as embedded data for the loop and
  table to consume.
- **`choice_exp_table.html`** - question text/HTML for the choice-task
  question inside the Loop & Merge block. Renders the two-alternative,
  five-attribute comparison table, pulling attribute names from the embedded
  data set by `choice_experiment.js` (`${e://Field/__js_*}`) and the current
  loop row's values via Loop & Merge fields (`${lm://Field/N}`).
- **`choice_table_lang_switches.js`** - JavaScript for the same choice-task
  question, runs on load of each loop iteration. Sets the "Task N of 6"
  counter text (with a separate Arabic counter string) and switches the
  table to right-to-left layout/alignment when the respondent's language is
  Arabic.
- **`framing_helper.js`** - JavaScript question, runs once and independent of
  framing condition. Pre-computes, for all 6 tasks, whether each alternative
  is "net-zero aligned" (fuel is `plants`/`electric`, or activity is
  `trees`/`direct_air` together with `permanent` durability) and stores the
  result as embedded data, so it's available in the data export for every
  respondent regardless of which framing arm they were assigned to.
- **`framing_experiment.js`** - JavaScript for the choice-task question,
  runs on ready, only meaningful for respondents in the net-zero framing
  condition. Re-applies the same net-zero classification to the current
  task's alternatives and injects translated explanatory text above the
  table (e.g. "Package A: aviation does/does not continue to contribute to
  global warming").
- **`style_customisation.css`** - survey-wide CSS: hides Qualtrics's built-in
  language selector and hides the page footer on the first page only.
