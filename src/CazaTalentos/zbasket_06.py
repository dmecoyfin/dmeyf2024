import  numpy as np

np.random.seed(102191)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# que hace qyt tiros libres

def ftirar(prob, qty):
  return sum(np.random.rand(qty) < prob)



# defino las jugadoras
taurasi = 0.85
peloton = [0.6] * 99
jugadoras = np.append(taurasi, peloton)

# vectorizo la funcion  ftirar
vec_ftirar = np.vectorize(ftirar)

for i in range(10):
  vaciertos = vec_ftirar(jugadoras, 100) # 10 tiros libres cada jugadora
  mejor = np.argmax(vaciertos)
  aciertos_torneo = vaciertos[mejor]
  aciertos_segunda = vec_ftirar(jugadoras[mejor], 100)
  print(aciertos_torneo, "\t", aciertos_segunda)


