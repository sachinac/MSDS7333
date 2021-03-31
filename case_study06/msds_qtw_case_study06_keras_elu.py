#!/usr/bin/env python
import pandas as pd
import numpy as np
import random
import seaborn as sns
import matplotlib.pyplot as pyplot
from sklearn.model_selection import train_test_split

import matplotlib.pyplot as plt
import tensorflow as tf
from sklearn.preprocessing import MinMaxScaler
from sklearn.metrics import accuracy_score, precision_score, recall_score
from sklearn.model_selection import train_test_split

from tensorflow.keras import layers, losses
from tensorflow.keras.models import Model

from tensorflow import keras
from tensorflow.keras import regularizers
from os import path
import pickle
import warnings
import sys
warnings.filterwarnings('ignore')

random.seed(10)
filename = "data/HIGGS.csv"
n = sum(1 for line in open(filename)) - 1
s = 2700000
skip = sorted(random.sample(range(1,n+1),n-s))
higgs_ds = pd.read_csv(filename,header=None,skiprows=skip)
#higgs_ds = pd.read_csv(filename,header=None)


# ## DataFrame
#
# * First column is target variable 1 for signal and 0 for background followed by 28 features.
# * next 21 features are low level features
# * remaining 7 features are high level features.

# In[3]:


features = ['target','lepton_pT','lepton_eta','lepton_phi','missing_energy_magnitude','missing_energy_phi',
'jet_1pt','jet_1eta','jet_1phi','jet_1b-tag','jet_2pt','jet_2eta','jet_2phi','jet_2b-tag',
'jet_3pt','jet_3eta','jet_3phi','jet_3b-tag','jet_4pt','jet_4eta','jet_4phi','jet_4b-tag',
'm_jj','m_jjj','m_lv','m_jlv','m_bb','m_wbb','m_wwbb']
higgs_ds.rename(columns=dict(zip(higgs_ds.columns, features)),inplace=True)


X_train, X_test, y_train, y_test = train_test_split(
    np.array(higgs_ds.iloc[:,1:]), np.array(higgs_ds.iloc[:,0]), test_size=0.2, random_state=42)



scaler = MinMaxScaler(feature_range=(0, 1))
X_train = scaler.fit_transform(X_train)
X_test  = scaler.transform(X_test)



warnings.filterwarnings('ignore')
print(' Store Model : ',sys.argv[1])
store_model = sys.argv[1]

if(path.exists(store_model)):
    model = keras.models.load_model(store_model)
else:
    model = tf.keras.Sequential()
    model.add(tf.keras.Input(shape=(28,)))
    model.add(layers.Dense(300, activation='elu',name="h0",
                           kernel_initializer=tf.keras.initializers.RandomNormal(mean=0., stddev=.1)))
    model.add(tf.keras.layers.Dropout(0.5))
    model.add(layers.Dense(300, activation='elu',name="h1",
              kernel_initializer=tf.keras.initializers.RandomNormal(mean=0., stddev=.05)))
    model.add(tf.keras.layers.Dropout(0.5))
    model.add(layers.Dense(300, activation='elu',name="h2",
              kernel_initializer=tf.keras.initializers.RandomNormal(mean=0., stddev=.05)))
    model.add(tf.keras.layers.Dropout(0.5))
    model.add(layers.Dense(300, activation='elu',name="h3",
              kernel_initializer=tf.keras.initializers.RandomNormal(mean=0., stddev=.05)))
    model.add(tf.keras.layers.Dropout(0.5))
    model.add(layers.Dense(1, activation='sigmoid',name="y",
              kernel_initializer=tf.keras.initializers.RandomNormal(mean=0., stddev= 0.001)))

# In[17]:


model.summary()


# In[18]:


warnings.filterwarnings('ignore')

print(' Store history : ',sys.argv[2])

model_fit_history = sys.argv[2]

if (not path.exists(store_model)):

    # New model
    #   initial_learning_rate=0.05,
    #   decay_steps=10000,
    #   decay_rate=0.96
    #   momentum=0.9
    #   batch_size=100

    lr_schedule = keras.optimizers.schedules.ExponentialDecay(
                               initial_learning_rate=0.05,
                               decay_steps=10000,
                               decay_rate=0.96)


    opt = tf.keras.optimizers.SGD(learning_rate=lr_schedule, momentum=0.9)
    model.compile( optimizer=opt,
                   loss='binary_crossentropy',
                   metrics=['accuracy','AUC'])
    history= model.fit(X_train, y_train, epochs=200, validation_data=(X_test,y_test), batch_size=1000)

    model.save(store_model)
    pickle.dump( history.history, open( model_fit_history, "wb" ) )
