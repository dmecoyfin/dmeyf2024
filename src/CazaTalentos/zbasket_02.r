

set.seed( 102191 )

# calcula cuantos encestes logra una jugadora con indice de enceste prob
# haciendo qyt tiros libres

ftirar  <- function( prob, qty ){
  return( sum( runif(qty) < prob ) )
}


# defino las jugadoras
taurasi    <- 0.7
peloton    <- ( 501:599 ) / 1000
jugadoras  <- c( taurasi, peloton )

# veo que tiene el vector
jugadoras




# hago que las 100 jugadoras tiren 10 veces cada una
mapply( ftirar, jugadoras, 10 )

primera_ganadora  <- 0

for( i in 1:10000 ){  # diez mil experimentos

  vaciertos  <- mapply( ftirar, jugadoras, 10 )  # 10 tiros libres cada jugadora

  mejor  <- which.max( vaciertos )
  if( mejor == 1 )  primera_ganadora  <- primera_ganadora + 1
}


print(  primera_ganadora )
