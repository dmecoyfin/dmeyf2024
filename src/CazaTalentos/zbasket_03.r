

set.seed( 102191 )

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}


# defino los jugadoras
taurasi <-  0.7
peloton    <-  ( 501:599 ) / 1000
jugadoras  <-  c( taurasi, peloton )

# veo que tiene el vector
jugadoras




for( i in 1:10 ){
  vaciertos  <- mapply( ftirar, jugadoras, 10 )  # cada jugadora tira 10 tiros libres
  mejor  <- which.max( vaciertos )
  aciertos_torneo  <- vaciertos[ mejor ]

  aciertos_segunda  <- ftirar( jugadoras[ mejor ], 10 )

  cat( aciertos_torneo, "\t", aciertos_segunda, "\n" )
}