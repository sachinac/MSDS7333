import pandas as pd
import numpy as np
import pickle
from sklearn.ensemble import RandomForestClassifier
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

cs07_random_state_ = 1999

rf_model = RandomForestClassifier(random_state=cs07_random_state_)


#
# Run Grid Search
#

rf_param_grid = [
      {'max_features' :  np.arange(1,6,1),
       'n_estimators' :  np.arange(10,210,10),
       'random_state' :  [cs07_random_state_],
       'n_jobs'       :  [-1]
      }
  ]

log_scoring = {'ROC'      : 'roc_auc', 
               'Accuracy'  : make_scorer(accuracy_score),
               'Recall'    : make_scorer(recall_score),
               'Precision' : make_scorer(precision_score),
               'F1'        : make_scorer(f1_score)
              }

cv = RepeatedStratifiedKFold(n_splits=10, n_repeats=3, random_state=1999)

print('Running grid search : ')

grid_search = GridSearchCV(estimator=rf_model, param_grid=rf_param_grid, n_jobs=-1, cv=cv, scoring=log_scoring,refit='Recall',error_score=0)

print('Fit grid search : ')
grid_result = grid_search.fit(XS_train_ohe, y_train.flatten())

print('store logistic grid results')
pickle.dump( grid_result, open( "data/rf_grid_result.dat", "wb" )) 

print('End of Process')

