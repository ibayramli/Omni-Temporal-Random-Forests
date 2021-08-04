from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble._forest import _get_n_samples_bootstrap
from imblearn.under_sampling import RandomUnderSampler
from joblib import Parallel, delayed
from collections.abc import Iterable

import numpy as np


class OT_BRFC:
    def __init__(
                 self,
                 feats_fixed=None,
                 max_features=None, 
                 max_samples=None,
                 n_estimators=30,
                 n_jobs=-1,
                 bootstrap=True,
                 verbose=0,
                 replacement=False,
                 class_weight=None,
                 sampling_strategy=None, 
                 n_classes=2,
                 random_state=None,
                 backend=None
                 ):

        self.max_features = max_features
        self.max_samples = max_samples
        self.n_estimators = n_estimators
        self.n_jobs = n_jobs
        self.bootstrap = bootstrap
        self.verbose = verbose
        self.replacement = replacement
        self.class_weight = class_weight
        self.sampling_strategy = sampling_strategy
        self.n_classes = n_classes
        self.backend = backend
        self.feats_fixed = self._validate_feats_fixed(feats_fixed)

        if random_state is None:
            self.random_state = np.random.RandomState()

    def _validate_feats_fixed(self, feats_fixed):
        if feats_fixed is None:
            feats_fixed = np.array([])
        elif type(feats_fixed) == np.ndarray:
            return feats_fixed
        elif isinstance(feats_fixed, Iterable): 
            feats_fixed = np.array(feats_fixed) 
        else:
            raise ValueError('Arguments feats_fixed must be an interable or None')

    def fit(self, X, y):
        self.estimators = []
        self.feats_used = []
        feats_fixed = self.feats_fixed
        if self.max_features is None:
            self.max_features = X.shape[1]

        feats_all = np.arange(X.shape[1])
        
        choice_size = int(self.max_features * X.shape[1]) - len(feats_fixed)
        feats_choosable = np.setdiff1d(feats_all, feats_fixed, assume_unique=True)

        rus = RandomUnderSampler(random_state=self.random_state,
                                 sampling_strategy=self.sampling_strategy,
                                 replacement=self.replacement)

        dtcs = Parallel(
                n_jobs=self.n_jobs, verbose=self.verbose, backend=self.backend
            ) \
            (
                delayed(self._build_tree)(X, y, rus, feats_choosable, feats_fixed, choice_size, i) 
                for i in range(self.n_estimators)
            )
        
        trees, features = [], []
        for d in dtcs:
            trees.append(d[0]); features.append(d[1])
        
        self.estimators.extend(trees); self.feats_used.extend(features)

    def _build_tree(self, X, y, rus, feats_choosable, feats_fixed, choice_size, i):
        feats = self._choose_feats(feats_choosable, feats_fixed, choice_size)

        idxs = self._get_resampled_idxs(y, rus)
        if self.bootstrap:
            idxs = self._get_bootstrap_idxs(idxs)
        
        X_resampled_bs, y_resampled_bs = X.iloc[idxs,feats], y[idxs]
        
        dtc = DecisionTreeClassifier(
            class_weight=self.class_weight,
            random_state=self.random_state,
            max_features=None
            )
        dtc.fit(X_resampled_bs, y_resampled_bs)
        
        return dtc, feats

    def _choose_feats(self, feats_choosable, feats_fixed, choice_size):
        chosen = self.random_state.choice(feats_choosable, size=choice_size, replace=False)
        feats = np.concatenate([chosen, feats_fixed], axis=0)
        
        return feats

    def _get_resampled_idxs(self, y, sampler):
        idxs = np.arange(len(y)).reshape(-1, 1)
        idxs, _ = sampler.fit_resample(idxs, y)
        
        return idxs.ravel()
        
    def _get_bootstrap_idxs(self, idxs):
        n_samples = len(idxs)
        n_bootstrap_samples = min(
            _get_n_samples_bootstrap(n_samples=n_samples, max_samples=self.max_samples),
            n_samples)
        idxs_bootstrap = self.random_state.choice(idxs, size=n_bootstrap_samples, replace=True)

        return idxs_bootstrap

    def _predict_tree(self, X, i):
        return self.estimators[i].predict_proba(X.iloc[:,self.feats_used[i]])

    def predict_proba(self, X):
        outs = Parallel(
                n_jobs=self.n_jobs, verbose=self.verbose, backend=self.backend
            ) \
            (
                delayed(self._predict_tree)(X, i)
                for i in range(self.n_estimators)
            )
        
        out = np.zeros((X.shape[0], self.n_classes))
        for o in outs:
            out += o

        return out / self.n_estimators

    def predict(self, X):
        return self.predict_proba(X).argmax(axis=1)

    def score(self, X, y):
        return (self.predict(X) == y).mean() 

