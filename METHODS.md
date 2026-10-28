# Supplementary Methods

*Beyond Weight Loss After Sleeve Gastrectomy: An Exploratory Cohort Study of Preoperative Binge Eating Status and Early Postoperative Outcomes*

## Contents

- [SM1. Analysis Overview and Software Environment](#sm1-analysis-overview-and-software-environment)
- [SM2. Follow-up Availability and Missing Data Diagnostics](#sm2-follow-up-availability-and-missing-data-diagnostics)
- [SM3. Preoperative and Six-Month Comparisons](#sm3-preoperative-and-six-month-comparisons)
- [SM4. Exploratory Machine Learning Analysis](#sm4-exploratory-machine-learning-analysis)
- [SM5. Postoperative Longitudinal Models and MAR/MNAR Sensitivity Analyses](#sm5-postoperative-longitudinal-models-and-marmnar-sensitivity-analyses)
- [SM6. Exploratory Analyses of Associations Between Adiposity and Immune Cell Counts](#sm6-exploratory-analyses-of-associations-between-adiposity-and-immune-cell-counts)
- [References](#references)

*Click any section above to jump directly to it.*

## SM1. Analysis Overview and Software Environment

Python 3.11.7 was used for follow-up availability and missing data diagnostics, preoperative and 6-month between-group comparisons, and exploratory machine learning analyses (SM2–SM4). R 4.5.1 was used for postoperative longitudinal models and missing data sensitivity analyses (SM5) and exploratory analyses of associations between adiposity and immune cell counts (SM6).

Python and R dependencies were managed with uv and renv, respectively. Principal packages included pandas, SciPy, statsmodels, scikit-learn, LightGBM, CatBoost, XGBoost, and SHapley Additive exPlanations (SHAP) in Python, and mmrm, emmeans, and mice in R. Annotated R and Python scripts are available on GitHub (https://github.com/MinglongCheng/binge-eating-bariatric-analysis).

## SM2. Follow-up Availability and Missing Data Diagnostics

**Visit-level laboratory attendance.**

Laboratory attendance at 1, 3, and 6 months after surgery (M1, M3, and M6, respectively) was distinguished from the availability of weight or other clinical data. Where estimable, multivariable logistic regressions modeled nonattendance using preoperative binge eating (BE) status, age, sex, and baseline body mass index (BMI); age and BMI were standardized. Full-model and covariate associations were assessed using likelihood-ratio and Wald tests, respectively. Benjamini–Hochberg (BH) false discovery rate (FDR) adjustment (1) was applied across visits separately for the full-model test and each covariate.

**Indicator-level availability among attenders.**

The same models assessed missingness of the 30 anthropometric and laboratory indicators among attenders at each visit. BH adjustment was applied across indicators within each visit, separately for the full-model test and each covariate. Two-sided Fisher exact tests provided unadjusted BE-group comparisons. Unavailable regression P values were set to 1 solely for BH adjustment.

**Empirical missingness classification.**

MAR and MCAR denote missing at random and missing completely at random, respectively. BH-adjusted P values (q) <0.10 for the full model or any covariate yielded MAR-drivers; otherwise, indicator-level Fisher P<0.05 yielded MAR-G (group-associated), with MCAR-compatible assigned when neither criterion was met. Overall indicator labels across visits prioritized MAR-drivers, followed by MAR-G and MCAR-compatible. These empirical labels do not establish the true missingness mechanism. Attendance and indicator availability were summarized overall and by group.

## SM3. Preoperative and Six-Month Comparisons

**Unadjusted between-group comparisons.**

Cross-sectional analyses used endpoint-specific complete cases, with the available sample size reported for each variable. Continuous-variable tests followed a fixed, variable-specific mapping to Student’s t test, Welch’s t test, or Mann–Whitney U tests. Distributional diagnostics were reviewed separately using Shapiro–Wilk tests for group sizes ≤50, Lilliefors-corrected Kolmogorov–Smirnov tests for larger groups, Levene tests, and graphical assessment when required. Categorical comparisons used Pearson’s chi-square test without continuity correction, replaced by two-sided Fisher exact tests when any expected count was <5.

Student’s and Welch’s t tests were accompanied by mean differences and Hedges’ g; Mann–Whitney U tests by Hodges–Lehmann estimates and Cliff’s δ. Mean-difference and Hedges’ g confidence intervals (CIs) used t-based and normal approximations, respectively; Hodges–Lehmann and Cliff’s δ CIs used 2,000 within-group percentile bootstrap resamples (seed=42). For proportions, prevalence ratios and risk differences used Katz log and Newcombe–Wilson CIs, respectively. All CIs were 95% intervals. Differences were oriented as BE minus non-BE, and ratios as BE/non-BE. BH-FDR adjustment (1) covered all continuous preoperative laboratory comparisons separately within the Nutrition, Metabolic, and Immune domains (8, 14, and 6 biomarkers, respectively). Nonlaboratory comparisons were not multiplicity-adjusted.

**Clinical threshold analyses.**

For clinically interpretable prevalence contrasts, selected continuous biomarkers were dichotomized as normal versus abnormal using prespecified reference cutoffs, and endpoints were included when the overall abnormal fraction was between 15% and 85% to avoid sparse tables. The cutoffs were as follows: total protein <68.3 g/L (2), folate <6 ng/mL (3), and 25-hydroxyvitamin D [25(OH)D] ≤30 ng/mL (4); alanine aminotransferase (ALT) >60 U/L in males and >42 U/L in females, aspartate aminotransferase (AST) >43 U/L, and γ-glutamyl transferase (GGT) >58 U/L in males and >43 U/L in females (2); fasting plasma glucose (FPG) ≥7.0 mmol/L and glycated hemoglobin (HbA1c) ≥6.5% (5); total cholesterol ≥5.2 mmol/L, low-density lipoprotein cholesterol (LDL-C) ≥3.4 mmol/L, and high-density lipoprotein cholesterol (HDL-C) <1.0 mmol/L (6); and uric acid ≥420 µmol/L in males and ≥360 µmol/L in females (7). Abnormality indicators were derived programmatically from the continuous values and sex where applicable. Between-group comparisons used Pearson's chi-square test or two-sided Fisher exact tests and reported prevalence ratios and risk differences with 95% confidence intervals.

**Sex-adjusted analyses.**

To address potential confounding by sex, eight prespecified biomarkers with established sex-related variation were evaluated in sex-adjusted models: serum iron, hemoglobin, red blood cell (RBC) count, ALT, GGT, HDL-C, creatinine, and uric acid (8, 9). Hemoglobin and HDL-C were analyzed on the original scale, whereas serum iron, RBC, ALT, GGT, creatinine, and uric acid were log-transformed; effects were reported as adjusted mean differences and geometric mean ratios, respectively. Models included BE group and sex. P values and confidence intervals for the BE-group coefficient used HC3 heteroskedasticity-consistent standard errors, whereas partial eta-squared was calculated from conventional Type III sums of squares. BH-FDR was applied once across the eight BE-group coefficient tests.

## SM4. Exploratory Machine Learning Analysis

**Data and resampling design.**

The machine learning analysis was exploratory and intended to complement the preoperative group comparisons rather than to develop a clinical prediction model. BE was treated as the positive class. Predictors comprised 28 preoperative laboratory variables; sex, age, baseline weight and BMI, Binge Eating Scale (BES) score, perioperative variables, clinical threshold indicators, and postoperative outcomes were excluded.

Stratified fivefold outer cross-validation was repeated 10 times, yielding 50 outer-training/test splits (seed=42) shared across feature panels and algorithms. Within each outer-training sample, threefold inner resampling generated training subsets for feature ranking, yielding 150 subsets in total. Model tuning used separately generated threefold inner splits.

**Integrated feature ranking.**

Three complementary procedures were applied within the inner-training subsets. First, Mann–Whitney U tests were followed by BH-FDR adjustment across the 28 candidate variables at q<0.10; when fewer than three variables met this threshold, variables with the lowest raw P values were added until three were retained. Second, L1-penalized logistic regression identified nonzero coefficients after median imputation and standardization fitted within each training subset. Third, XGBoost selected the three highest-gain variables in each subset.

For each procedure, selection frequencies across the subsets were converted to method-specific ranks. The global score for feature $f$ was the unweighted Borda rank sum

```math
B(f) = R_{\mathrm{FDR}}(f) + R_{\mathrm{L1}}(f) + R_{\mathrm{XGB}}(f),
```

where lower values indicated higher priority. Ties received the minimum rank. For fixed-panel selection, equal Borda sums were ordered by decreasing mean selection frequency and then by the fixed feature order. Three panels were evaluated: all 28 variables, the four highest-ranked variables (fixed Top-4), and the variables occupying positions 5–8 (Table S7).

**Model development and performance.**

CatBoost and LightGBM were fitted separately for each panel using their native handling of missing predictor values. Class weights were derived from the corresponding training subset. Hyperparameters and early-stopping iterations were selected within inner resampling. The selected configuration was then used to refit the model on the complete outer-training sample before evaluation on the corresponding outer-test fold.

Area under the receiver operating characteristic curve (AUROC), area under the precision–recall curve (AUPRC), and Brier scores were calculated within each outer-test fold. AUPRC was computed as average precision. Within each repeat, AUROC and AUPRC were averaged equally across the five folds, whereas Brier scores were weighted by fold size. Performance was reported as mean ± standard deviation (SD) across the 10 complete repeats. Ninety-five percent confidence intervals used 2,000 stratified participant-level bootstrap resamples of the existing outer-test predictions, preserving fold assignments without refitting models. Reduced-panel performance estimates were descriptive post-selection comparisons subject to potential optimism because the global supervised ranking was not nested within the outer evaluation.

**Held-out model interpretation.**

For the all-feature and fixed Top-4 models, permutation importance was calculated in the corresponding outer-test folds using 30 permutations per feature:

```math
\Delta\mathrm{AUROC} = \mathrm{AUROC}_{\mathrm{baseline}} - \mathrm{AUROC}_{\mathrm{permuted}}.
```

For each outer fold, the mean across the 30 permutations was retained; empirical 2.5th and 97.5th percentiles across the 50 outer folds summarized between-fold variation. Features were ordered by the absolute value of their mean signed $\Delta\mathrm{AUROC}$ across outer folds.

SHAP values were likewise calculated only for held-out participant–repeat observations from the corresponding refitted outer-fold models, with contributions expressed on the raw model-output (log-odds) scale. Because SHAP values encode both the magnitude and direction of feature contributions to model output (10), we additionally examined whether these directions were stable across cross-validation folds and algorithms. Cross-validation-based checks of SHAP explanations have been recommended and applied to assess whether feature-importance and directional attribution patterns remain consistent across data splits (11, 12). In our analysis, we used a simple rank-based sign metric to summarize directional consistency across folds and models.

For feature $f$, algorithm $a$, and outer fold $j$, directional behavior was quantified by the Spearman correlation between the raw feature values and their corresponding SHAP contributions:

```math
\rho_{a,j}(f) = \mathrm{Spearman}\!\left(X_{j,f},\,\mathrm{SHAP}_{a,j}(f)\right).
```

Correlations required at least three finite feature–SHAP pairs and variation in both quantities. A trinary fold-level sign was defined for estimable correlations as

```math
s_{a,j}(f) =\begin{cases}\mathrm{sign}\!\left[\rho_{a,j}(f)\right], & |\rho_{a,j}(f)| \geq 0.05, \\
0, & |\rho_{a,j}(f)| < 0.05.\end{cases}
```

The threshold of 0.05 was a near-zero tolerance, not a significance threshold. Let $V_{a,f}$ denote the set of outer folds with estimable directions and $n_{a,f}=|V_{a,f}|$. The algorithm-specific directional score was

```math
S_a(f)=\frac{1}{n_{a,f}}\sum_{j\in V_{a,f}}s_{a,j}(f),\qquad n_{a,f}>0.
```

The cross-algorithm score and absolute directional consistency were

```math
S_{\mathrm{overall}}(f)=\frac{1}{A}\sum_{a=1}^{A}S_a(f),\qquad S_{\mathrm{abs}}(f)=\left|S_{\mathrm{overall}}(f)\right|,
```

where $A=2$. If either algorithm had no estimable folds, its score and both cross-algorithm summaries were recorded as not estimable (NA). The sign of $S_{\mathrm{overall}}(f)$ indicated whether higher feature values tended to have more positive (+) or more negative (−) SHAP contributions; a zero score indicated no net directional tendency across folds and algorithms. When estimable, $S_{\mathrm{abs}}(f)$ ranged from 0 to 1 and summarized net directional consistency.

For cross-algorithm importance, the algorithm-specific mean absolute SHAP value was calculated across all held-out participant–repeat observations, including those with missing raw feature values:

```math
I_a(f)=\mathrm{mean}\!\left(\left|\mathrm{SHAP}_a(f)\right|\right).
```

Importance was then normalized within each algorithm to its most important feature and averaged across algorithms:

```math
I_{\mathrm{norm}}(f)=\frac{1}{A}\sum_{a=1}^{A}\frac{I_a(f)}{\max_g I_a(g)},
```

where $g$ indexes all features in the corresponding panel. The radar profiles combined $I_{\mathrm{norm}}(f)$, displayed as normalized radial bars, with $S_{\mathrm{abs}}(f)$, displayed as the directional-consistency polygon.

## SM5. Postoperative Longitudinal Models and MAR/MNAR Sensitivity Analyses

**Primary post-baseline MMRM.**

Thirty prespecified anthropometric and laboratory outcomes were evaluated at baseline (M0), M1, M3, and M6, with visit treated as categorical. The primary model required an observed baseline value and at least one observed postoperative value for the corresponding outcome; complete postoperative follow-up was not required.

Primary longitudinal inference used a baseline-adjusted mixed model for repeated measures (MMRM) fitted to M1, M3, and M6. Fixed effects included group, visit, group × visit, centered baseline outcome, baseline outcome × visit, sex, age, and baseline BMI. Baseline BMI was omitted from the weight and BMI models to avoid redundant adjustment for baseline body size. The primary test was the global group × visit interaction; an equal-weight average adjusted BE-minus-non-BE contrast across postoperative visits was estimated from the same interaction model.

**Auxiliary baseline-included model.**

An auxiliary unconstrained longitudinal model used all available M0–M6 outcome observations and included group, visit, group × visit, sex, age, and baseline BMI, with the same exception for weight and BMI. Baseline group means were not constrained to be equal. It provided adjusted trajectories, an overall time effect averaged equally across groups, the equal-weight average group contrast across M0–M6, and the M6-minus-M0 difference-in-change, calculated from adjusted group means:

```math
\Delta\Delta = (\mathrm{BE}_{M6}-\mathrm{BE}_{M0})-(\text{non-BE}_{M6}-\text{non-BE}_{M0}).
```

A descriptive reversal indicator denoted a change in the sign of the adjusted BE-minus-non-BE contrast between M0 and M6.

**Model estimation and inference.**

Both model families used restricted maximum likelihood estimation, Kenward–Roger inference (13), and an unstructured within-participant covariance matrix, with heterogeneous first-order autoregressive covariance [ARH(1)] as a fallback if the unstructured model failed. All final models used the unstructured covariance structure. Model-based marginal means, 95% confidence intervals, and contrasts were obtained from the fitted models. The observed-data models used available observations under a working MAR assumption without imputation. Longitudinal P values were interpreted as nominal and hypothesis-generating.

**MAR/MNAR tipping-point sensitivity analysis.**

For sensitivity analyses, multiple imputation under MAR was performed separately for each outcome using predictive mean matching with five donors, 20 iterations, and fixed random seeds. The imputation models included the outcome’s four visit values, group, sex, age, and baseline BMI where applicable. Model-specific participant eligibility was retained from the observed data. Missing M0 values were imputed where required but were not δ-shifted. To examine missing not at random (MNAR) scenarios, signed δ offsets were applied only to originally missing postoperative values at M1, M3, or M6 (14), scaled by the SD of observed changes from baseline at each visit, pooling both groups.

The analysis proceeded in three stages. Preflight used m=50 imputations at δ=0 for all outcomes to identify discrepancies between observed-data and MAR multiple-imputation conclusions. Outcomes with discrepancies at δ=0 were reassessed with m=100. The full tipping-point search evaluated signed δ values from −1.00 to +1.00 in 0.10-unit steps, with 0.01-unit refinement around the first detected conclusion change in each direction. Outcomes flagged at preflight or with an initial tipping point at |δ|≤0.05 used m=100 for the final search; remaining outcomes used m=50.

Two offset scopes were examined. In the BE-only analysis, positive δ increased and negative δ decreased imputed BE values, while non-BE imputations remained at their MAR values. In the symmetric both-arm analysis, positive δ increased BE and decreased non-BE imputations by equal magnitude, with the opposite pattern for negative δ. Estimates and covariance matrices were combined using D1 pooled Wald inference, and reversal was determined from pooled M0 and M6 group contrasts. 0.00→target indicates discordance already present under δ=0 MAR multiple imputation, whereas >|1.00|→target indicates that no tipping point was reached within the examined ±1-SD range.

## SM6. Exploratory Analyses of Associations Between Adiposity and Immune Cell Counts

**Analysis framework and effect standardization.**

Six immune cell outcomes were evaluated: white blood cell (WBC), neutrophil, lymphocyte, monocyte, eosinophil, and basophil counts. BMI and body weight were analyzed separately as related anthropometric indicators of adiposity rather than as independent replications. BE and non-BE participants were pooled to examine cohort-level adiposity–immune associations. Analyses used endpoint-specific available observations without imputation.

Effects were standardized by multiplying the regression slope by 2 kg/m² for BMI or 5 kg for body weight and dividing by the baseline SD of the corresponding immune cell outcome. Coefficients and confidence intervals were therefore expressed in baseline immune-outcome SD units per 2 kg/m² BMI or 5 kg body weight.

**Baseline associations.**

At M0, each immune cell count was regressed separately on BMI or body weight using ordinary least-squares models adjusted for age and sex. The resulting coefficients and 95% confidence intervals were expressed on the standardized scale defined above.

**Postoperative cumulative change-from-baseline associations.**

Postoperative associations across M1, M3, and M6 were estimated using mixed models for repeated measures (MMRM), with the immune cell count at each postoperative visit as the response. Models included categorical visit, adiposity change from M0, baseline adiposity, the baseline value of the corresponding immune cell outcome, age, and sex. Adiposity change was calculated as the postoperative value minus the M0 value. Its coefficient summarized the overall postoperative association after baseline adjustment.

Visit-specific cumulative slopes were estimated from models additionally including the visit × adiposity change interaction. Slopes were obtained separately at M1, M3, and M6, and a joint test assessed heterogeneity across visits. These slopes correspond to cumulative changes from M0 to each postoperative visit, rather than changes over adjacent intervals.

With changes defined as follow-up minus baseline, positive coefficients indicated that greater reductions in BMI or body weight were associated with greater reductions in immune cell counts after baseline adjustment.

**Adjacent-interval associations.**

A separate analysis evaluated adjacent changes over M0→M1, M1→M3, and M3→M6, using observed values at both ends of each interval. Immune cell change was modeled as a function of interval, adiposity change, interval × adiposity change interaction, interval-start adiposity, interval-start immune cell count, age, and sex. Interval-specific slopes were standardized using the same exposure increments and baseline immune-outcome SDs.

**Model estimation and descriptive visualization.**

All repeated-measures models used restricted maximum likelihood estimation, Kenward–Roger inference, and an unstructured within-participant covariance matrix, with first-order autoregressive [AR(1)] covariance as a fallback if the unstructured model failed. P values were nominal and interpreted as exploratory.

Descriptive raw-data visualizations comprised baseline scatterplots without fitted lines or inferential overlays, participant-level current-value paths across M0–M6, and participant-level change-from-baseline paths across M1–M6. Lines connected observed measurements only; no imputation was used for the plotted data.

## References

1. Benjamini Y, Hochberg Y. Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society: Series B (Methodological) (1995) 57(1):289–300. https://doi.org/10.1111/j.2517-6161.1995.tb02031.x.

2. Mu R, Chen W, Pan B, Wang L, Hao X, Huang X, et al. First definition of reference intervals of liver function tests in China: a large-population-based multi-center study about healthy adults. PLOS ONE (2013) 8(9):e72916. https://doi.org/10.1371/journal.pone.0072916.

3. Fang A-P, Liu Z-Y, Liao G-C, Chen P-Y, Wang X-Y, Zhang D-M, et al. Serum folate concentrations at diagnosis are associated with hepatocellular carcinoma survival in the Guangdong Liver Cancer Cohort study. British Journal of Nutrition (2019) 121(12):1376–88. https://doi.org/10.1017/S0007114519000734.

4. Mechanick JI, Apovian C, Brethauer S, Garvey WT, Joffe AM, Kim J, et al. Clinical practice guidelines for the perioperative nutrition, metabolic, and nonsurgical support of patients undergoing bariatric procedures - 2019 update: cosponsored by American Association of Clinical Endocrinologists/American College of Endocrinology, The Obesity Society, American Society for Metabolic and Bariatric Surgery, Obesity Medicine Association, and American Society of Anesthesiologists. Obesity (Silver Spring) (2020) 28(4):O1–O58. https://doi.org/10.1002/oby.22719.

5. Shi G, Zhu N, Qiu L, Yan H, Zeng L, Wang D, et al. Impact of the 2020 China Diabetes Society guideline on the prevalence of diabetes mellitus and eligibility for antidiabetic treatment in China. International Journal of General Medicine (2021) 14:6639–45. https://doi.org/10.2147/IJGM.S331948.

6. Li JJ, Zhao SP, Zhao D, Lu GP, Peng DQ, Liu J, et al. 2023 Chinese guideline for lipid management. Frontiers in Pharmacology (2023) 14:1190934. https://doi.org/10.3389/fphar.2023.1190934.

7. She D, Wang Y, Liu J, Luo N, Feng S, Li Y, et al. Changes in the prevalence of hyperuricemia in clients of health examination in Eastern China, 2009 to 2019. BMC Endocrine Disorders (2022) 22(1):202. https://doi.org/10.1186/s12902-022-01118-z.

8. Wu X, Zhao M, Pan B, Zhang J, Peng M, Wang L, et al. Complete blood count reference intervals for healthy Han Chinese adults. PLOS ONE (2015) 10(3):e0119669. https://doi.org/10.1371/journal.pone.0119669.

9. Xia L, Chen M, Liu M, Tao Z, Li S, Wang L, et al. Nationwide multicenter reference interval study for 28 common biochemical analytes in China. Medicine (Baltimore) (2016) 95(9):e2915. https://doi.org/10.1097/MD.0000000000002915.

10. Lundberg SM, Erion G, Chen H, DeGrave A, Prutkin JM, Nair B, et al. From local explanations to global understanding with explainable AI for trees. Nature Machine Intelligence (2020) 2(1):56–67. https://doi.org/10.1038/s42256-019-0138-9.

11. Park J, Pang C, Lee TY, Yang JY, Berkowitz J, Wei AZ, et al. Toward scalable early cancer detection: evaluating EHR-based predictive models against traditional screening criteria. npj Precision Oncology (2026) 10(1):299. https://doi.org/10.1038/s41698-026-01483-1.

12. Ponce-Bobadilla AV, Schmitt V, Maier CS, Mensing S, Stodtmann S. Practical guide to SHAP analysis: explaining supervised machine learning model predictions in drug development. Clinical and Translational Science (2024) 17(11):e70056. https://doi.org/10.1111/cts.70056.

13. Kenward MG, Roger JH. Small sample inference for fixed effects from restricted maximum likelihood. Biometrics (1997) 53(3):983–97. https://doi.org/10.2307/2533558.

14. Cro S, Morris TP, Kenward MG, Carpenter JR. Sensitivity analysis for clinical trials with missing continuous outcome data using controlled multiple imputation: a practical guide. Statistics in Medicine (2020) 39(21):2815–42. https://doi.org/10.1002/sim.8569.
