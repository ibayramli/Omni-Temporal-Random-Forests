# Omni Temporal Random Forests

This repository contains code for _Omni-temporal Balanced Random Forests_ (OT-BRFCs) introduced [in this paper](https://academic.oup.com/jamia/advance-article-abstract/doi/10.1093/jamia/ocab225/6415403) developed for suicide prediciton in [Predictive Medicine Group](https://www.predmed.org/ilkin-bayramli) of Boston Children's Hospital. Citation:

> Ilkin Bayramli, Victor Castro, Yuval Barak-Corren, Emily M Madsen, Matthew K Nock, Jordan W Smoller, Ben Y Reis, Temporally informed random forests for suicide risk prediction, Journal of the American Medical Informatics Association, Volume 29, Issue 1, January 2022, Pages 62–71, https://doi.org/10.1093/jamia/ocab225

Traditional random forest models sample a subset of features uniformly at random before fitting a tree. As a result, some variables that are critical for inference can be omitted from some of the trees. Although this helps drive the model variance down, sometimes inclusion of certain variables in every tree can be crucial for good performance based on domain knowledge. OT-BRFCs allow for a subset of features to be fixed across all trees, ensuring that all trees in the forest have access to these variables despite random sampling. This repository expands upon tools offered by libraries such as `scikit-learn`, `imblearn` and `joblib` to offer support for building OT-BRFCs.


To fit a model which fixes features `x1, x2` across all trees, do:

```python
feats_fixed = ['x1', 'x2']
params = {
      'max_features': 0.5,
      'max_samples': None, # size of Bootstrap samples
      'n_estimators': 30,
      'n_jobs': 4, 
      'bootstrap': True, # subsample balanced dataset with replacement
      'verbose': 10,
      'replacement': False, # RandomUnderSampler replacement
      'class_weight': 'balanced_subsample', 
      'sampling_strategy': 0.25,
      'feats_fixed': feats_fixed,
      'backend': 'threading'
    }
rf = OT_BRFC(**params)
```
where the `params` dictionary differs from [arguments of BalancedRandomForestClassifier](https://imbalanced-learn.org/stable/references/generated/imblearn.ensemble.BalancedRandomForestClassifier.html) of `imblearn` library only with the key `feats_fixed`. 
