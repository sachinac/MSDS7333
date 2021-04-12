import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import missingno as msno 
import plotly.express as px
import pickle

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

from sklearn.decomposition import PCA

from sklearn.linear_model import LogisticRegression
from sklearn.metrics import plot_confusion_matrix
from sklearn.model_selection import RepeatedStratifiedKFold
from sklearn.model_selection import GridSearchCV

from sklearn.metrics import make_scorer
from sklearn.metrics import accuracy_score
from sklearn.metrics import precision_score
from sklearn.metrics import recall_score
from sklearn.metrics import f1_score

print('Process Begins')

X_train = pd.read_csv('data/train_set.csv',sep=',')
X_test  = pd.read_csv('data/test_set.csv',sep=',')
y_train_target = pd.read_csv('data/train_target.csv',sep=',')
y_test_target  = pd.read_csv('data/test_target.csv',sep=',')

XS_train_ohe = X_train.values
XS_test_ohe  = X_test.values 
y_train      = y_train_target.values
y_test       = y_test_target.values


log_model = LogisticRegression(random_state=1999,max_iter=1000)

cs07_random_state_ = 1999

#
# Run Grid Search
#

log_param_grid = [
      {'penalty'      : ['l1'],
       'C'            : np.logspace(-4, 4, 10),
       'solver'       : ['liblinear'],
       'random_state' : [cs07_random_state_]},
      {'penalty'      : ['l2'],
       'C'            : np.logspace(-4, 4, 10),
       'solver'       : ['sag'],
       'max_iter'     : [800],
       'random_state' : [cs07_random_state_]}
  ]

log_scoring = {'ROC'      : 'roc_auc', 
               'Accuracy'  : make_scorer(accuracy_score),
               'Recall'    : make_scorer(recall_score),
               'Precision' : make_scorer(precision_score),
               'F1'        : make_scorer(f1_score)
              }

cv = RepeatedStratifiedKFold(n_splits=10, n_repeats=3, random_state=1999)

print('Running grid search : ')

grid_search = GridSearchCV(estimator=log_model, param_grid=log_param_grid, n_jobs=-1, cv=cv, scoring=log_scoring,refit='Recall',error_score=0)

print('Fit grid search : ')
grid_result = grid_search.fit(XS_train_ohe, y_train.flatten())

print('store logistic grid results')
pickle.dump( grid_result, open( "data/logistic_grid_result.dat", "wb" )) 

print('End of Process')

