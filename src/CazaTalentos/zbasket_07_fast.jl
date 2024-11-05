# intencionalmente la mejor jugadora va al final de la lista de jugadoras
# porque la funcion findmax() de Julia hace trampa
# si hay un empate ( dos máximos) se queda con la que esta primera en el vector
using Random

Random.seed!(102191)

function ftirar(prob, qty)
  return  sum( rand() < prob for i in 1:qty )
end


# defino las jugadoras
taurasi = [0.7]
peloton = Vector((501:599) / 1000)
jugadoras = append!(peloton, taurasi) # intencionalmente la mejor esta al final


function  explorar()

  for tiros_libres in [ 10, 20, 50, 100, 200, 300, 400, 415, 500, 600, 700, 1000 ]

    ultima_ganadora = 0

    for  i in  1:10000
      vaciertos = ftirar.(jugadoras, tiros_libres)
      mejor = findmax( vaciertos )[2]

      if mejor == 100   ultima_ganadora += 1  end
    end

    println( tiros_libres,  "\t", ultima_ganadora/10000 )
  end
end


@time  explorar()

