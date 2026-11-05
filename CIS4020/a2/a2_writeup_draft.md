# CIS*4020 A2: Write-up draft

**Do not submit this file.** These are AI-drafted answers for you to rewrite in your own words. Each section below matches one `TODO (markdown)` cell in `a2_LucLevesque.ipynb`, in notebook order, and quotes that cell's prompt. Replace the TODO line and `*Your discussion here...*` with your version, and keep any `##` heading in the cell.

All metrics come from the executed notebook and are the same on every run. Timings change slightly between runs, so they're given as approximate. Check them against your final run before you copy them.

---

## Before Part 1: Explain the changes you made to the provided data loading and analysis table: which features are known at your prediction point, and which students you kept.

### Data loading and starter fixes

I replaced the starter's loading cells with my Assignment 1 loader, for three reasons:

- **Missing values:** OULAD marks missing values with `"?"`. The raw `imd_band` column has 1,111 `"?"` values and no real blanks, so without `na_values="?"` the starter's `fillna("Missing")` does nothing.
- **Synthetic data:** the starter's second loader resets `DATA_DIR` and silently falls back to synthetic data if the files aren't found. I removed it, so a missing file now raises an error.
- **Download step:** I removed the `wget`/`unzip` cell because the CSVs are already in `content/`.

I also defined `make_model`, `X`, `X_tr`, `cv5` and the feature lists, which the starter refers to but never creates. `evaluate()` now names at-risk as the positive class and returns its metrics.

### Features allowed at day 30 and who is modelled

**Rule:** a feature is allowed only if it would be known before day 30.

**Features removed or replaced:**

- `total_clicks` and `active_days` cover the whole course. Withdrawn students stop clicking, so these partly record the outcome. I keep them only for the leakage check.
- `date_unregistration` directly reveals withdrawal.
- `id_student` is an identifier. `code_presentation` is a cohort label that a future student wouldn't share.
- My A1 flag `no_vle_activity` was based on the whole course. `early_clicks == 0` is the valid version of it, so no extra flag is needed.
- My A1 flag `registration_missing` is dropped because missing registration dates were closely tied to withdrawal in A1 (39 of 45), which suggests a recording artefact rather than something known at enrolment. Only 7 missing dates remain here, and they're median-imputed inside the Pipeline.

**Students removed:**

- 5,067 students had unregistered before day 30 (5,059 Withdrawn, 8 Fail). Their outcome is already known, so predicting it would inflate the scores without helping anyone.
- 16 students registered on or after day 30, so they weren't enrolled at the prediction point.

That leaves 27,510 of the original 32,593 rows. `date_unregistration` is used only to define this population and is never a feature.

The early window includes activity before the module starts, because those clicks are also known by day 30. This is why `early_active_days` can be higher than 30.

---

## Part 1: State your classification task and target variable.

### Overview and task

This notebook compares a decision tree (an eager learner) with k-nearest neighbours (a lazy learner) on the Open University Learning Analytics Dataset.

The task is an early warning: on day 30 of a module, predict which students are at risk of not succeeding, using only what is known by that day. I chose this framing because a prediction is only useful if there's still time to act on it, and it stops the model from seeing information from after the prediction point.

### Target definition

`at_risk = 1` for Fail or Withdrawn, and `at_risk = 0` for Pass or Distinction. At-risk is the positive class because those are the students an early-warning system exists to find, so precision and recall then have a direct meaning: how many flagged students are truly at risk, and how many at-risk students get flagged.

The two groupings aren't uniform. Failing and withdrawing are different experiences, but both mean the student didn't complete the module successfully.

---

## Part 1: Describe the class distribution and note any imbalance.

There are 12,131 at-risk students (44.1%) and 15,379 not at risk (55.9%). This is only mild imbalance, so I don't resample. It does affect how to read the metrics: always predicting "not at risk" gets 55.9% accuracy, and always predicting "at risk" gets an F1 of about 0.61. Results have to be compared against those baselines.

---

## Part 1: Describe the feature types.

There are 12 features:

- **Numeric (5):** `early_clicks`, `early_active_days`, `date_registration`, `num_of_prev_attempts`, `studied_credits`.
- **Ordinal (3):** `highest_education`, `imd_band`, `age_band`.
- **Nominal (4):** `code_module`, `gender`, `region`, `disability`.

Attribute types follow my A1 table. `imd_band` has 1,027 missing values in this population, and `date_registration` has 7.

---

## Part 1: Describe what the value distributions show for each class.

- **Early engagement separates the classes, but with a lot of overlap.** Median `early_clicks` is 305 for not-at-risk students and 155 for at-risk students. Median `early_active_days` is 16 and 10. At-risk students also show a clear spike at zero clicks.
- **`early_clicks` is very skewed** (skewness 3.47), which is why it's plotted on a log axis.
- **`date_registration` barely differs:** the median is day -53 in both classes.
- **Prior attempts:** the at-risk rate rises from 41.8% with no prior attempts to 69.0% with three or more. However, 24,044 students (87%) have no prior attempts, so this can only affect a small group.
- **Study load:** the at-risk rate is 40.6% at 30 credits or fewer and 56.2% above 120 credits.

The overlap in these plots is an early warning that perfect prediction isn't realistic from day-30 data.

---

## Part 1: Document your preprocessing decisions: how you encoded the categorical features and why, and why you scaled the numeric features.

- **Ordinal features** use an explicit order, such as `No Formal quals` up to `Post Graduate Qualification`, because the order carries information that a tree can split on and k-NN can measure.
- **Nominal features** are one-hot encoded. Any integer coding would invent an order or distance between, say, two regions. This takes the 12 features to 33 columns.
- **Missing `imd_band`** is imputed with the median and given an indicator column. This keeps my A1 keep-and-flag decision in a form that a distance measure can use.
- **`log1p` on `early_clicks` only.** It reduces skewness from 3.47 to -1.37. On `early_active_days` it only flips 0.77 to -0.95, so I left that feature unlogged. I made this choice after looking at the full population's skew, but `log1p` has no fitted parameters, so no test-set values are learned by the model.
- **Standardisation (z-score).** k-NN measures distance, so without scaling a feature with a large range, like `early_clicks`, would dominate one with a small range. The lecture notes also warn that k-NN is sensitive to skew and outliers. Decision trees compare one feature at a time against a threshold, so scaling doesn't affect them. Both models share one pipeline to keep the comparison simple.

One-hot columns add many dimensions to k-NN's distance, which may dilute the useful features. This is a known cost of the encoding.

---

## Part 1: Explain your holdout and 5-fold cross-validation setup, and how preprocessing is fit on training data only.

I used an 80/20 stratified holdout (22,008 train, 5,502 test), plus stratified 5-fold cross-validation with shuffling. Both keep the at-risk share at about 44.1%.

Tuning uses cross-validation inside the training set only, so the test set plays no part in choosing a model.

All preprocessing sits inside a Pipeline with the classifier. Scalers, imputers and encoders are therefore learned from the training portion of each fold and only applied to the validation data, which prevents information leaking from the data used for evaluation.

---

## Part 2: Explain what the baselines show.

Following the lecture advice on baselines, I compare against simple rules:

| Baseline (CV) | Accuracy | F1 |
|---|---|---|
| Always "not at risk" | 0.559 | 0.000 |
| Always "at risk" | 0.441 | 0.612 |
| Random by class share | 0.511 | 0.447 |
| One-split tree | 0.621 | 0.592 |

The always-at-risk rule shows a weakness of F1 here. It reaches 0.612 just by flagging everyone, so a useful model has to be judged on accuracy and on precision and recall as well.

---

## Part 2: Discuss the results with default hyperparameters.

| Default model (CV) | Accuracy | Precision | Recall | F1 | Train F1 |
|---|---|---|---|---|---|
| Decision tree | 0.591 | 0.536 | 0.541 | 0.539 | 1.000 |
| k-NN (k=5) | 0.637 | 0.601 | 0.530 | 0.563 | 0.709 |

The default tree scores a perfect F1 on training data but only 0.539 on validation data, so it has memorised the training set. It's worse than the one-split tree on both accuracy and F1. k-NN also drops between training and validation, though by less.

---

## Part 2: Report the effect of the decision-tree hyperparameters, and compare gini vs entropy.

### Decision-tree hyperparameters

I searched 132 combinations of `criterion`, `max_depth` and `min_samples_leaf`, scored by F1.

- **`max_depth`:** training F1 climbs to 1.000 as depth grows, while validation F1 falls from 0.601 at depth 5 to 0.539 with no limit. The widening gap is the overfitting pattern from lecture.
- **`min_samples_leaf`:** this acts as a brake on deep trees. With no depth limit, validation F1 rises from 0.539 at 1 to 0.591 at 100. At shallow depths it makes almost no difference, because the depth limit already stops the tree first.
- **An uneven curve:** validation F1 doesn't move smoothly with depth, going 0.592, 0.564, 0.557, 0.601 across depths 2 to 5. A hard yes/no threshold means small changes in the tree can swing recall, so I treat depth 5 as a reasonable choice rather than a precise optimum.

### Gini vs entropy

The two criteria perform almost the same. The best entropy tree has a CV F1 of 0.6057 and the best gini tree 0.6032, both at depth 5 with `min_samples_leaf=50`. Across the grid the mean gap is 0.0046 and the largest is 0.0132. Both measure node impurity in similar ways, so this is expected, and the choice of depth and leaf size matters far more.

---

## Part 2: Report the effect of the k-NN hyperparameters, and state which settings you chose for both models.

### k-NN hyperparameters

I searched 40 combinations of odd `n_neighbors`, `weights` and `metric`. I used odd values of k because the lecture recommends them to avoid ties.

- **`n_neighbors`:** F1 rises from 0.538 at k=1 to a peak of 0.581 at k=21, then falls to about 0.55 at k=151. Small k follows noise, and very large k averages over students who aren't really similar.
- **`weights`:** distance weighting gives a small improvement at most values of k.
- **`metric`:** Euclidean and Manhattan are close, and which one is ahead depends on k.

### Chosen models

- **Tree:** `criterion="entropy"`, `max_depth=5`, `min_samples_leaf=50`.
- **k-NN:** `n_neighbors=21`, `weights="distance"`, `metric="euclidean"`.

Both were picked by the highest mean CV F1 on the training set.

---

## Part 3: Discuss the holdout and 5-fold results for both models.

### Test-set results

| Test set | Accuracy | Precision | Recall | F1 |
|---|---|---|---|---|
| Always "at risk" | 0.441 | 0.441 | 1.000 | 0.612 |
| One-split tree | 0.618 | 0.562 | 0.610 | 0.585 |
| Decision tree (defaults) | 0.599 | 0.545 | 0.545 | 0.545 |
| k-NN (defaults) | 0.628 | 0.590 | 0.515 | 0.550 |
| Decision tree (tuned) | 0.664 | 0.622 | 0.610 | 0.616 |
| k-NN (tuned) | 0.658 | 0.644 | 0.503 | 0.565 |

Tuning helped both models, the tree most of all.

The tuned models beat the accuracy baseline of 55.9% by about 10 percentage points, which shows a real signal in the day-30 data. On F1, however, the tree only just matches always-at-risk (0.616 vs 0.612) and k-NN is below it. The models are much more precise than flagging everyone, but they give up recall to get there. Overall this is a modest result.

The two models make different kinds of mistake:

| | Missed at-risk (FN) | False alarms (FP) | Correctly flagged (TP) |
|---|---|---|---|
| Decision tree | 947 | 900 | 1,479 |
| k-NN | 1,206 | 675 | 1,220 |

The tree finds more at-risk students, while k-NN raises fewer false alarms. For early support, where a false alarm costs little compared with missing someone, I'd favour the tree's balance.

### Cross-validation results

| CV on training set | Accuracy | Precision | Recall | F1 |
|---|---|---|---|---|
| Decision tree (tuned) | 0.662 ± 0.004 | 0.625 ± 0.016 | 0.590 ± 0.048 | 0.606 ± 0.020 |
| k-NN (tuned) | 0.668 ± 0.007 | 0.655 ± 0.011 | 0.523 ± 0.011 | 0.581 ± 0.009 |

CV agrees with the test set: similar accuracy, with the tree ahead on F1 and recall and k-NN ahead on precision. These scores are slightly optimistic, because the same folds were used to pick the settings.

The tree's mean F1 is 0.024 higher, but the 95% interval on the paired fold differences is [-0.002, 0.050], which includes zero. The folds also share data, so the interval is only approximate. I can't conclude from this that the tree is clearly better on F1. The tree's recall is also less stable across folds.

---

## Part 3: Discuss (markdown): training time, prediction time, memory. Why are they opposite?

Timed on one thread using already-transformed data, so shared preprocessing doesn't affect the comparison. Values are medians of repeated runs.

| Model | Train | Predict 5,502 students | Stored size |
|---|---|---|---|
| Decision tree (tuned) | about 21 ms | about 0.3 ms | about 6 KB |
| k-NN (tuned) | about 0.6 ms | about 210 ms | about 5,850 KB |

The two models are opposites. The tree does its work up front: training takes about 35 times longer, then prediction is about 700 times faster. k-NN does almost nothing at training and pays at prediction time.

**Memory:** the tuned tree stores 63 nodes (depth 5, 32 leaves), each holding a split rule or a class count. k-NN stores the entire training set: 22,008 rows × 33 columns, or about 5,674 KB plus labels. That's nearly 1,000 times the size of the tuned tree. Tree size also depends on tuning, since the unrestricted tree has 11,449 nodes, reaches depth 39 and takes about 896 KB.

**Complexity** (n = training rows, d = features):

| | Training time | Prediction, per query | Memory |
|---|---|---|---|
| Decision tree | O(d · n log n) | O(depth), about O(log n) if balanced | O(nodes) |
| k-NN, brute force | O(1) beyond storing the data | O(n · d) | O(n · d) |
| k-NN, KD-tree | about O(d · n log n) | near O(d log n) in low dimensions, close to brute force in high dimensions | O(n · d) |

The tree sorts candidate thresholds for each feature at each level, which gives the n log n cost. It then only has to follow one path from root to leaf. k-NN brute force has to compute the distance to every stored row for each query.

**Search method:** scikit-learn's `algorithm="auto"` picked brute force here. Forcing a KD-tree added about 8 ms of training and made prediction slower, at about 800 to 900 ms. With 33 dimensions, tree-based search can't discard many points, so its overhead doesn't pay off. Fast neighbour search doesn't rescue k-NN in this setting.

---

## Part 3: Discuss the extra checks (leakage and repeat students) and the limitations of this analysis.

### Leakage check

These scores are for comparison only and aren't reported model results. They use the tuned settings with 5-fold CV. The populations differ between rows, so the comparison is approximate.

| Scenario | Tree F1 | k-NN F1 |
|---|---|---|
| **Reported:** day-30 features, still enrolled | 0.604 | 0.578 |
| Day-30 features, including students who already left | 0.715 | 0.709 |
| Whole-course features, still enrolled | 0.798 | 0.772 |
| Whole-course features, including students who already left | 0.867 | 0.840 |

Tree F1 rises by 0.26 and accuracy goes from 0.662 to 0.865, yet none of this extra score would exist on day 30. It's the difference between predicting the outcome and describing something that has already happened. The lower reported numbers are the honest ones.

### Repeat students

The 27,510 rows cover 24,822 students, because some students took more than one module or tried again. A random split can place the same student on both sides: 875 test rows (15.9%) have a student who also appears in training.

I expected this to flatter k-NN, which could match a student with their own earlier record. Checking with `StratifiedGroupKFold`, which keeps each student on one side, F1 changed by only 0.004 for the tree and 0.0003 for k-NN (in the opposite direction). The effect is negligible, so I keep the required stratified splits.

### Limitations

- **Modest signal.** Day-30 demographics and clicks predict only moderately, and a richer model could add early assessment results.
- **F1 as the tuning target.** Given the always-at-risk baseline, F1 is not an ideal selection metric here. In practice I'd choose a threshold based on the real cost of a missed student compared with a false alarm.
- **Fixed cutoff.** Day 30 is a judgement call. An earlier or later cutoff trades lead time against accuracy.
- **Single institution.** The data is from one university over a few presentations, so results may not carry over. A random split also doesn't test predicting a future presentation.
- **Clicks aren't learning.** They measure activity, not understanding.
- **Survivorship in the population.** Removing students who left early means the model doesn't address those students at all, and they're a group that needs support too. It would need an earlier intervention point.

---

## Part 4: Which features matter most? Do they match your expectations?

The tuned tree uses only 10 of the 33 encoded columns.

| Original feature | Importance |
|---|---|
| `early_active_days` | 0.531 |
| `code_module` | 0.231 |
| `highest_education` | 0.100 |
| `early_clicks` | 0.096 |
| `imd_band` | 0.022 |
| `num_of_prev_attempts` | 0.012 |
| `date_registration` | 0.008 |

`age_band`, `disability`, `gender`, `region` and `studied_credits` have zero importance. One-hot columns are summed so each feature appears once.

This mostly matches what I expected. Early engagement dominates, which agrees with my A1 finding on early clicks. Consistency seems to matter more than volume, as the number of active days ranks well above total clicks. `code_module` ranks second, led by modules BBB and GGG, which fits the large differences between modules I found in A1.

`num_of_prev_attempts` is lower than I expected given the Part 1 trend. A likely reason is that 87% of students have no prior attempts, so a split on it reduces impurity for only a small group.

Impurity importance has limits: it's calculated on training data, and when two features overlap the tree may use one and leave the other looking unimportant.

---

## Part 4: Explain why k-NN needs permutation importance, and compare its ranking to the tree's.

### Why k-NN needs permutation importance, and what it shows

k-NN has no native importances because it learns no parameters. It stores the training rows and measures distance, with every scaled feature contributing to that distance under the same rule. There are no splits or weights to read.

Permutation importance works around this by treating the model as a black box: shuffle one feature, which breaks its link to the outcome, and measure how much performance drops. I ran it on the whole Pipeline, so a categorical feature is shuffled as one column, not as separate one-hot columns.

| Feature | Drop in test F1 (k-NN) |
|---|---|
| `early_active_days` | 0.044 |
| `early_clicks` | 0.028 |
| `highest_education` | 0.017 |
| `imd_band` | 0.010 |
| `code_module` | 0.006 |

The rest are close to zero, within their variation across repeats. `region` and `date_registration` are slightly negative, meaning shuffling them doesn't hurt the model, or even helps a little. They're contributing noise to the distance calculation.

### Comparing the rankings

The models agree on the broad picture. `early_active_days` ranks first in all three measures, and `highest_education` ranks third in all three. Spearman rank correlation with k-NN's permutation ranking is 0.80 for tree impurity importance and 0.62 for tree permutation importance.

Two differences stand out:

- **`early_clicks`:** k-NN relies on it (0.028 drop) while the tree barely does (0.004 drop). The two early features are strongly correlated (0.886). The tree can choose one and ignore the other, whereas k-NN's distance includes both.
- **`code_module`:** this matters more to the tree (0.040 drop) than to k-NN (0.006 drop). A tree can isolate one module with a single split. In k-NN, each module is spread across several one-hot columns and competes with every other feature in the distance.

---

## Part 4: Discuss the challenge of determining feature importance in a lazy learner, and how understanding feature importance improves interpretability and real-world decision-making.

Feature importance is harder for a lazy learner because no global model exists to inspect. Each prediction depends on the particular neighbours of that student. Permutation importance gives only an indirect estimate, and it has costs:

- It requires many rounds of prediction, which are k-NN's slow step.
- Correlated features can hide each other: if one is shuffled, the other still carries much of the same information, so both can look less important than they are.
- Shuffling can create unrealistic students, such as many clicks with very few active days.
- Scores can be noisy, and they can even be negative, as they are here.

Importance also matters in practice. If early activity is the main signal, an advisor has a concrete reason for a flag and a clear action, such as contacting students who haven't logged in regularly. A tree's rules can be read and challenged, which helps when decisions affect real people.

Importance isn't causation, though. Logging in more may not itself improve results, as motivation or available time could drive both. Features like `imd_band` and `highest_education` also need care. They can identify students who need support, but a system built on them could treat a student as a statistic. Predictions should prompt an offer of help, never a penalty.

---

## Part 5: Explain the conceptual difference between eager and lazy learners; how decision trees and k-NN differ in training vs prediction; and the trade-offs

### Conceptual difference

An eager learner builds a general model from the training data before seeing any new cases, then discards the data and applies the model. A lazy learner builds nothing in advance: it keeps the training data and does the work when a query arrives, reasoning from the stored cases closest to it.

### Training and prediction processes

**Decision tree**
- *Training:* greedily picks the split that most reduces impurity, then repeats on each branch until a stopping rule is reached. Here this took about 21 ms.
- *Prediction:* follows one path of at most 5 tests to a leaf. Here 5,502 students took about 0.3 ms.

**k-NN**
- *Training:* stores the rows. Here this took about 0.6 ms.
- *Prediction:* computes the distance from each student to all 22,008 stored rows, finds the nearest 21 and takes a distance-weighted vote. Here this took about 210 ms.

### Trade-offs and when to prefer each

| | Decision tree (eager) | k-NN (lazy) |
|---|---|---|
| Upfront cost | Higher | Almost none |
| Prediction cost | Very low | Grows with the training data |
| Memory | Small | The whole training set |
| Adding new data | Needs retraining | Just add the rows |
| Interpretability | Readable rules | Needs indirect methods |
| Scaling needed | No | Yes |
| Decision boundaries | One feature at a time | Flexible and local |
| Main risk | Overfits if unrestricted | Slows down, and is affected by irrelevant features and high dimensions |

I'd prefer an eager learner when predictions need to be fast, cheap or explainable. I'd prefer a lazy learner when data changes often, the training set is small, the number of features is low, and slower predictions are acceptable.

For this task I'd choose the tree. Accuracy is about equal, it catches more at-risk students, and it's smaller and faster. Advisors can also see why a student was flagged. k-NN's strengths matter little here, because a university would likely retrain between module presentations and doesn't need instant updates.

---

## Disclosure: Disclose how you used generative-AI tools.

*Edit this to match what you actually did. It has to be accurate on the day you submit.*

Generative AI (Claude Code, Anthropic) was used in this assignment. It wrote the notebook code from my planning decisions and my Assignment 1 cleaning code, ran the experiments, and drafted the written answers in a separate file. I reviewed the code and results and rewrote the written answers in my own words. I understand and can explain everything in this notebook.

---

## Optional: notes to add under Part 6

The starter's Part 6 cell has no TODO, so these are optional additions to it.

- The notebook runs top to bottom on a clean kernel (Kernel → Restart & Run All).
- **Data:** place the 7 OULAD CSVs in `content/` next to the notebook.
- **Seed:** `RANDOM_SEED = 42` is used for NumPy, the splits, the trees, the random baseline and permutation importance, so metrics are identical on each run.
- **Timings:** these vary a little between runs and machines. The relative differences are what matter.
- **Environment:** Python 3.15, pandas 3.1, scikit-learn 1.9.1, NumPy, SciPy and Matplotlib. Runtime is about 1 minute.
