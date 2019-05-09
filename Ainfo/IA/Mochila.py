#!/usr/bin/env python
# coding: utf-8

# In[59]:


import numpy as np
import copy
import random


# In[60]:


class it:
    def __init__(self,id = 0, v = 0,w = 0):
        self.id = id
        self.v = v
        self.w = w


# In[162]:


cap = 16
items = [it(1,8,5),it(2,7,6),it(3,12,10),it(4,6,4),it(5,2,1),it(6,3,1)]


# In[164]:


cap = 255
items = [it(1,204,51),it(2,51,119),it(3,119,68),it(4,68,34),it(5,51,17),it(6,136,102)]


# In[169]:


maxv = 0
best = []
for i,_ in enumerate(items):
    w = 0
    v = 0
    end = False
    init = copy.deepcopy(items)
    random.shuffle(init)
    start = init.pop(i)
    sol = []
    sol.append(start)
    w += start.w
    v += start.v
    print("INICIO: %i %i"%(start.v,start.w))
    while not end:
        bestit = sol[-1]
        auxw = 9999
        auxv = 0
        for item in init:
            if (item.w + w) <= cap and (item.v + v) > auxv and (item.w + w) < auxw:
                auxv = item.v
                bestit = item
        if bestit != sol[-1]:
            w += bestit.w
            v += bestit.v
            init.remove(bestit)
            sol.append(bestit)
        else:
            if v > maxv:
                best = copy.deepcopy(sol)
                maxv = v
            print("Máximo hasta ahora: %i"%(maxv))
            print("Valor: %i, Peso: %i de %i\n"%(v,w,cap))
            #print("Solución:")
            #for item in sol:
                #print('\t',item.id, item.v,item.w)
            end = True
print("\nMEJOR SOLUCION: ")
print("Valor: %i, Peso: %i de %i"%(sum([x.v for x in best]),sum([x.w for x in best]),cap))
for item in best:
    print('\t',item.id, item.v,item.w)


# In[152]:


maxv


# In[ ]:




