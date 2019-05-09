#!/usr/bin/env python
# coding: utf-8

# In[128]:


import random
import math


# In[182]:


def func(x):
    #return abs((x-5)/(2+math.sin(math.radians(x))))
    return abs((x-5)/(2+math.sin(x)))
def to_bin(n,l = 4):
    return bin(n)[2:].rjust(l,'0')
def to_dec(n):
    return int(n,2)


# In[183]:


def init(n = 6, mx = 4):
    arr = []
    for x in range(n):
        arr.append(to_bin(random.randint(0,2**mx - 1),mx))
    return arr
def genetic(a,n = 30):
    for gen in range(n):
        b = [func(to_dec(x)) for x in a]
        a[b.index(min(b))] = a[b.index(max(b))]
        random.shuffle(a)    
        for i,_ in enumerate(a[::2]):
            split = random.randint(0,3)
            x,y = a[i],a[i+1]
            a[i] = x[:split] + y[split:]
            a[i+1] = y[:split] + x[split:]
            #Agregar mutación aquí
    return a


# In[198]:


a = init(32,8)
print([to_dec(x) for x in a],"\n")
b = genetic(a)
print([to_dec(x) for x in b])

