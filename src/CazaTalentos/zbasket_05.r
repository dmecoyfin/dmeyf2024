

set.seed( 102191 )

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# que hace qyt tiros libres

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}


# defino las jugadoras
jugadoras  <- rep( 0.7, 100 )




suma_diferencias <- 0

for( i in 1:10000 ){
  vaciertos  <- mapply( ftirar, jugadoras, 100 )  # cada jugador tira 100 tiros libres
  mejor  <- which.max( vaciertos )
  aciertos_torneo  <- vaciertos[ mejor ]

  aciertos_segunda  <- ftirar( jugadoras[mejor], 100 )

  suma_diferencias  <- suma_diferencias +  (aciertos_torneo - aciertos_segunda )
}


print(  suma_diferencias / 10000 )
