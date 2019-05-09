#!/usr/bin/env python
# coding: utf-8

# In[3]:


import numpy as np
import random as rn


# In[185]:


class queens:
    def __init__(self,N = 8, rand = 0.05): #rand es la posibilidad de mutación
        if N % 2 == 1:
            N += 1
        self.N = N
        self.rand = rand
        self.Parejas = {}
        self.Individuos = {}
        for i in range(self.N):
            self.Individuos[i] = np.random.choice(range(1,self.N+1),self.N,replace = True)
        
    def couples(self):
        Aleatorio = rn.sample(range(self.N//2,self.N),self.N//2)
        self.Parejas = {}
        for i in range(self.N//2):
            self.Parejas[i] = Aleatorio[i]
            self.Parejas[Aleatorio[i]] = i

    def init(self):
        self.Individuos = {}
        for i in range(self.N):
            self.Individuos[i] = np.random.choice(range(1,self.N+1),self.N,replace = True)
    
    def func(self,T):
        Atk = 0
        for i in range(len(T)):
            for j in range(i+1,len(T)):
                if T[i] == T[j]:
                    Atk += 1
                Dif = j-i
                if T[i] == T[j] - Dif or T[i] == T[j] + Dif:
                    Atk += 1
        return (self.N*(self.N-1)/2) - Atk
    
    def mutate(self,i):
        for j,_ in enumerate(self.Individuos[i]):
            muter = rn.randint(0,self.N-1)
            if rn.random() <= self.rand:
                self.Individuos[i][j] = muter
            
    def sel(self):
        self.couples()
        for k,v in self.Parejas.items():
            if self.func(self.Individuos[k]) >= self.func(self.Individuos[v]):
                self.Individuos[v] = self.Individuos[k]
    
    def cruce(self):
        self.couples()
        temp = 0
        for k,v in self.Parejas.items():
            if temp % 2 == 0:
                split = rn.randint(0,self.N-1)
                H1 = np.concatenate((self.Individuos[k][:split],self.Individuos[v][split:]))
                H2 = np.concatenate((self.Individuos[v][:split],self.Individuos[k][split:]))
                self.Individuos[k] = H1
                self.Individuos[v] = H2
                self.mutate(k)
                self.mutate(v)         
            temp += 1
            
    def generations(self, N=5):
        for i in range(N):
            self.sel()
            self.cruce()
    
    def show(self):
        for i in range(self.N):
            print(self.Individuos[i],"\tfx = ",self.func(self.Individuos[i])," / ", self.N*(self.N-1)/2)


# In[208]:


r = queens(16,0.05)


# In[209]:


r.generations(1000)
r.show()


# In[ ]:




