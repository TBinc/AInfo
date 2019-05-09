#!/usr/bin/env python
# coding: utf-8

# In[84]:


import numpy as np
import random as rn
import math


# In[192]:


class ann:
    def __init__(self, Temp = 1000, Est = None, Dist = None):
        
        self.Temp = Temp
        self.Enf = 0.03
    
        if Est is None:
            self.N = 10
            self.Est = np.array(rn.sample(range(0,self.N),self.N))
        else:
            self.Est = Est
            self.N = len(self.Est)
        
        if Dist is None:
            self.Dist = []
            for i in range(self.N):
                self.Dist.append(rn.sample(range(0,1500),self.N))
            self.Dist = np.triu(self.Dist)+np.tril(self.Dist)
            for i in range(self.N):
                self.Dist[i][i] = 0
        else:
            self.Dist = Dist
        
        self.Best = np.copy(self.Est)
        self.New = np.copy(self.Est)
    
    def show(self):
        print("%s\tEnergy: %i\tTemp: %i"%(self.Best,self.energy(self.Best),self.Temp))
    
    def energy(self,T):
        en = 0
        for i,item in enumerate(T):
            if i < len(T)-1:
                en += self.Dist[item,T[i+1]]
            else:
                en += self.Dist[item,T[0]]
        return en
    
    def new_tour(self):
        chosen = rn.sample(range(0,self.N),2)
        self.New[chosen[0]], self.New[chosen[1]] = self.New[chosen[1]], self.New[chosen[0]]
        eNew = self.energy(self.New)
        eEst = self.energy(self.Est)
        eBest = self.energy(self.Best)
        if eNew < eEst:
            prob = 1
        else:
            prob = math.exp(-(eEst-eNew)/self.Temp)
        
        if rn.random() <= prob:
            self.Est = np.copy(self.New)
        
        if eNew < eBest:
            self.Best = np.copy(self.New)
        
        self.Temp = (1-self.Enf)*self.Temp
    
    def generate(self, cant = 500):
        self.show()
        for i in range(cant):
            if self.Temp < 10:
                break
            self.new_tour()
            self.show()


# In[193]:


a = ann()


# In[194]:


a.generate()


# In[103]:


(np.triu(f)+np.tril(f))


# In[104]:


f = np.array([1,2,5,60])
for i,item in enumerate(f):
    print(item)


# In[45]:


N=250
rn.sample(range(0,N),8)


# In[86]:


math.exp(0)


# In[ ]:




