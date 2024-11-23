using Random

Random.seed!(102191)

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# que hace qyt tiros libres

function ftirar(prob, qty)
  return  sum( rand() < prob for i in 1:qty )
end


# defino las jugadoras
jugadoras = fill( 0.7, 100 )





global suma_diferencias = 0

for i = 1:10000
  vaciertos = ftirar.(jugadoras, 100)  # 10 tiros libres cada jugadora
  mejor = findmax( vaciertos )[2]
  aciertos_torneo = vaciertos[ mejor ] 

  aciertos_segunda = ftirar.( jugadoras[ mejor ], 100 )

  global suma_diferencias += ( aciertos_torneo - aciertos_segunda )
end

println(  suma_diferencias/10000 )
