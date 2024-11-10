# intencionalmente la mejor jugadora va al final de la lista de jugadoras
# porque la funcion which.max() de R hace trampa
# si hay un empate ( dos máximos) se queda con la que esta primera en el vector


set.seed( 102191 )

# calcula cuantos encestes logra una jugadora con indice de enceste prob que hace qyt tiros libres
ftirar  <- function( prob, qty ){
  vsize <- length(prob)
  return( rowSums( matrix( runif( qty*vsize ), vsize, qty)  <
                   matrix( rep(prob,qty), vsize, qty)  ) )
}

# defino los jugadoras
taurasi    <- 0.7
peloton    <- ( 501:599 ) / 1000
jugadoras  <- c( peloton, taurasi ) # intencionalmente la mejor esta al final


t0  <- Sys.time()

for( tiros_libres in c(10, 20, 50, 100, 200, 300, 400, 415, 500, 600, 700, 1000 ) ){

  ultima_ganadora  <- 0

  for( i in 1:10000 ) {  # diez mil experimentos

    vaciertos  <- ftirar( jugadoras, tiros_libres )
    mejor  <- which.max( vaciertos )

    if( mejor == 100 )  ultima_ganadora  <- ultima_ganadora + 1
  }

  cat( tiros_libres, "\t", ultima_ganadora/10000, "\n" )
}

t1  <- Sys.time()

print( t1 - t0 )
