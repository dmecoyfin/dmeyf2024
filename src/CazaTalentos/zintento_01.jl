using Random
using Statistics
using DataFrames

# Definir una función auxiliar para los tiros
function ftirar(prob, qty)
  return  sum(rand() < prob for i in 1:qty)
end


# Variable global para las funciones del gimnasio
GLOBAL_gimnasio = Dict()

# Inicializar el gimnasio
# Asigna una precisión de tiro a cada jugadora que no puede ser vista por la cazatalentos
function gimnasio_init()
    GLOBAL_gimnasio[:taurasita] = 0.5
    GLOBAL_gimnasio[:jugadoras] = shuffle(append!([0.204:0.002:0.400;],
      GLOBAL_gimnasio[:taurasita]))

    GLOBAL_gimnasio[:tiros_total] = 0
end

# Toma una lista de IDs de jugadoras y la cantidad de tiros a realizar
# Devuelve el número de encestes para cada jugadora
function gimnasio_tirar(pids, pcantidad)
    GLOBAL_gimnasio[:tiros_total] += length(pids) * pcantidad
    return [ftirar(GLOBAL_gimnasio[:jugadoras][id], pcantidad) for id in pids]
end

# La cazatalentos elige una jugadora
# Devuelve la cantidad total de tiros y si acerto a la verdadera mejor
function gimnasio_veredicto(jugadora_id)
    return Dict(
        "tiros_total" => GLOBAL_gimnasio[:tiros_total],
        "acierto" => Int(GLOBAL_gimnasio[:jugadoras][jugadora_id] == GLOBAL_gimnasio[:taurasita])
    )
end

#------------------------------------------------------------------------------
# Realizar una ronda eliminatoria
# Las jugadoras con activa == 1 hacer tiros libres
# y son eliminadas si están por debajo de cierto umbral

function ronda_eliminatoria!(planilla, tiros, desvios)
    # Si no hay jugadoras activas o no hay tiros, salir de la función
    if sum(planilla[!, :activa] .== 1) == 0 || tiros < 1
        return
    end

    ids_juegan = planilla[planilla[!, :activa] .== 1, :id]
    resultados = gimnasio_tirar(ids_juegan, tiros)
    planilla[planilla[!, :activa] .== 1, :encestes] .= resultados

    # Calcular la cantidad mínima de encestes para pasar a la siguiente ronda
    encestes_corte = mean(planilla[planilla[!, :activa] .== 1, :encestes]) +
      desvios * std(planilla[planilla[!, :activa] .== 1, :encestes])

    # Poner en estado inactivo a las jugadoras por debajo del umbral
    for i in eachindex(planilla[!, :id])
        if planilla[!, :activa][i] == 1 && planilla[!, :encestes][i] < encestes_corte
            planilla[!, :activa][i] = 0
        end
    end
end


# Estrategia A
# son en total 4 rondas elimnatorias
# las tres primeras son de 50 tiros libres
#  y se eliminan a las jugadoras que estan a la izquierda de -0.3 desvios de la media
# la cuarta ronda es de 200 tiros libres
# finamente, elijo a la mejor jugadora de la cuarta ronda

function Estrategia_A()
    gimnasio_init() # inicializo el gimnasio

    # Crear la planilla de la cazatalentos con todas las jugadoras activas
    planilla_cazatalentos = DataFrame(
      id=[1:1:100;], # el numero en la espalda de la jugadora
      activa=ones(Int, 100), # todas las jugadoras estan activas
      encestes=zeros(Int,100) # cero encestes
    )

    # Rondas 1 a 3
    # los seis valores que siguen
    #  me los reveló en un sueño la Diosa Namagiri Thayar
    ronda_eliminatoria!(planilla_cazatalentos, 37,  0.0)
    ronda_eliminatoria!(planilla_cazatalentos, 45, -0.1)
    ronda_eliminatoria!(planilla_cazatalentos, 50,  0.0)

    # Ronda 4
    ronda_eliminatoria!(planilla_cazatalentos, 200, 0)

    # Elegir a la jugadora con más encestes en la última ronda
    pos_mejor = argmax(planilla_cazatalentos[planilla_cazatalentos[!, :activa] .== 1, :encestes])
    jugadora_mejor = planilla_cazatalentos[planilla_cazatalentos[!, :activa] .== 1, :id][pos_mejor]

    return  gimnasio_veredicto(jugadora_mejor)
end


@time  begin  # mido el tiempo

# Estimación Montecarlo de la tasa de éxito de la Estrategia A
Random.seed!(102191)  # fijar la semilla para reproducibilidad

tabla_veredictos = DataFrame(tiros_total=Int[], acierto=Int[])

# repito el experimento 
for experimento in 1:100000  # repeticiones montecarlo
    if experimento % 10000 == 0  print(experimento, " ")  end

    veredicto = Estrategia_A()
    push!(tabla_veredictos, veredicto)
end

println()

# calculo e imprimo metricas
tiros_media = mean(tabla_veredictos.tiros_total)
tasa_eleccion_correcta = mean(tabla_veredictos.acierto)

println("La tasa de elección de la verdadera mejor es: ", tasa_eleccion_correcta)
println("La cantidad de tiros promedio en lograrlo es: ", tiros_media)

end
