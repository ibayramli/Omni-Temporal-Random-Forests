# Omni-Temporal-Random-Forests

To fit a model which fixed certain features across trees, pass

```python
feats_fixed = ['feature_1', 'feature_2']
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
rf = FixedFeatureBRFC(**params, backend=backend)
```
