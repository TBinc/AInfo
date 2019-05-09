#!/usr/bin/env python
# coding: utf-8

# In[1]:


from kanren import *


# In[58]:


hijo = Relation()
esposo = Relation()


# In[45]:


def nieto(x, z):
    y = var()
    return conde((hijo(x, y), hijo(y, z)))
def mama(x,z):
    y = var()
    return conde((hijo(x, y), esposo(y, z)))
def hermano(x,z):
    y = var()
    return conde((hijo(x, y), hijo(z, y)))


# In[4]:


M,P,H = vars(3)


# In[5]:


facts(esposo, ("Sabino", "Rosalia"),("David", "Flor"),("Rene", "Nelith"))


# In[6]:


facts(hijo, ("David", "Sabino"),("Rene", "Sabino"),("Hernan", "Sabino"),("Angie","David"),("Marcelo","Rene"))


# In[14]:


run(3,H,esposo(M,H),mama(M,"Rosalia"))


# In[49]:


[(x,y) for x,y in run(0,(H,P),hermano(H,P)) if x != y]


# In[57]:


run(0,P,hermano(P,"David"))


# In[ ]:




