# este script no es para personas fragiles de espiritu

using CSV, DataFrames, Random, Statistics
using Primes
using DecisionTree, Impute
using Base.Threads

struct ttree
   n_subfeatures::UInt
   maxdepth::UInt
   min_samples_split::UInt
   min_samples_leaf::UInt
   min_purity_increase::Float64
end

ptree = ttree(0, 7, 800, 20, 0)
training = 0.7
semilla = 17
qsemillas = 100

#------------------------------------------------------------------------------

function  EstimarGanancia( psemilla, training, ptree )

    # particion
    Random.seed!(psemilla)
    vfold = 2 .-  Int.( rand(Float64, length(dataset_clase)) .< training )

    # train_campos =  replace!( Matrix( dataset[ vfold .== 1 ,  Not(:clase_ternaria)] ), missing => 0  )

    # clase = string.(dataset[ vfold .== 1 , :clase_ternaria ])
    # datos = Matrix( dataset[ vfold .== 1 ,  Not(:clase_ternaria)] )
    # genero el modelo en training
    modelo = DecisionTree.build_tree(
        dataset_clase[ vfold .== 1 ],
        dataset_matriz[ vfold .== 1 ,:],
        ptree.n_subfeatures,
        ptree.maxdepth,
        ptree.min_samples_leaf,
        ptree.min_samples_split,
        ptree.min_purity_increase
    )

    # aplico el modelo a testing vfold = 2
    pred = apply_tree_proba(modelo, 
        dataset_matriz[ vfold .== 2 ,:],
        ["BAJA+1","BAJA+2","CONTINUA"]
    )


   ganancia_test_normalizada = sum(
        ( dataset_clase[ vfold .== 2][ (pred[:, 2 ] .> 0.025) ] .== "BAJA+2"   )
        .* 280000
        .- 7000
       ) / ( 1.0 - training )

   return  ganancia_test_normalizada
end
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa

# leo el dataset
dataset = CSV.read("competencia_01_julia.csv", DataFrame)

# restrinjo al periodo 202104
dataset = dataset[ dataset.foto_mes .== 202104, : ]

# Lamentablemente debo imputar nulos, 
#  porque la libreria DecisionTree no los soporta
dataset = Impute.substitute( dataset ) 

# formato para  DecisionTrees
dataset_clase = string.(dataset[ :, :clase_ternaria ])
dataset_matriz = Matrix( dataset[ :, Not(:clase_ternaria)] )

# elimino  dataset
dataset = Nothing

# genero la cantidad de qsemillas  nuevas semillas
Random.seed!(semilla)
semillas = rand( Primes.primes( 100000, 999999 ), qsemillas )


# vector donde almaceno los resultados
ganancia = Array{Float64}( undef, length( semillas ))

# calculo las  ganancias
@threads for i=1:length(semillas) 
   ganancia[i] = EstimarGanancia( semillas[i], training, ptree )
end

print( ganancia )
print( Statistics.mean( ganancia ) )

