# intencionalmente la mejor jugadora va al final de la lista de jugadoras
# porque la funcion np.argmax() de Python hace trampa
# si hay un empate ( dos máximos) se queda con la que esta primera en el vector
import  numpy as np
import  time

np.random.seed(102191)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# que hace qyt tiros libres
def vec_ftirar(prob, qty):
  return sum(np.random.rand(qty, len(prob)) < prob) 


# defino los jugadoras
taurasi = 0.7
peloton = np.array(range(501, 600)) / 1000
jugadoras = np.append(peloton, taurasi)


t0 = time.time()


for tiros_libres in [10, 20, 50, 100, 200, 300, 400, 415, 500, 600, 700, 1000]:
  ultima_ganadora = 0
  for i in range(10000):
    vaciertos = vec_ftirar(jugadoras, tiros_libres) # 10 tiros libres cada jugadora
    mejor = np.argmax(vaciertos)
    if mejor == 99:
      ultima_ganadora += 1
  print(tiros_libres, "\t", ultima_ganadora/10000)


t1 = time.time()
print(t1-t0)
