
import numpy as np
import pandas as pd

# Kézi mozgási kategóriák × PAM-klaszterek kimentése külön
tabla = np.array([
    [117, 100,  68, 211],
    [256, 257,  30,   0],
    [ 27,   2,  40,  27],
    [322,  44, 426,   0]
])

#lambda
# A kézi mozgási kategória előrejelzése a PAM alapján
n = tabla.sum()
E1 = n - tabla.sum(axis=1).max()
E2 = n - tabla.max(axis=0).sum()

lam = (E1 - E2) / E1

# ARI
def parok(x):
    return x * (x - 1) / 2

a = parok(tabla.sum(axis=1)).sum()
b = parok(tabla.sum(axis=0)).sum()
c = parok(tabla).sum()

varhato = a * b / parok(n)
ari = (c - varhato) / ((a + b) / 2 - varhato)

eredmenyek = pd.DataFrame({
    "Mutató": ["lambda", "ARI"],
    "Érték": [lam, ari]
})

print(eredmenyek.to_string(index=False))
