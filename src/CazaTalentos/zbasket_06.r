

set.seed( 102191 )

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# que hace qyt tiros libres

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}


# defino las jugadoras
taurasi    <- 0.85
peloton   <- rep( 0.6, 99 )  # jugadoras identicas
jugadoras <- c( taurasi, peloton )




for( i in 1:10 ){
  vaciertos  <- mapply( ftirar, jugadoras, 100 )  # cada jugadora tira 100 tiros libres
  mejor  <- which.max( vaciertos )
  aciertos_torneo  <- vaciertos[ mejor ]

  aciertos_segunda  <- ftirar( jugadoras[ mejor ], 100 )

  cat(aciertos_torneo, "\t", aciertos_segunda, "\n" )
}

