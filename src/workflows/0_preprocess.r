library(data.table)

# Leer el dataset
dt <- fread("./datasets/kaggle_competencia_02_muestra.csv")

# Armo listado de las fotos-meses que sean únicos
listado_meses <- sort(unique(dt$foto_mes))
print(listado_meses)

# Armo un set que contenga los clientes correspondientes a cada foto-mes y ordeno por foto-mes
clientes_por_mes <- dt[, .(clientes_unicos = uniqueN(numero_de_cliente)), by = foto_mes]

# Muestro la cantidad total de clientes únicos
total_clientes <- sum(clientes_por_mes$clientes_unicos)
print(clientes_por_mes)
print(total_clientes)

# Genero una tabla con clientes únicos por foto_mes
clientes_por_mes_dt <- dt[, .(numero_de_cliente), by = foto_mes]

# Uno para agregar columnas de presencia en los siguientes meses
# Columna para presencia en el siguiente mes
dt <- merge(dt, clientes_por_mes_dt[, .(foto_mes = foto_mes - 1, numero_de_cliente, presente_siguiente = TRUE)],
            by = c("foto_mes", "numero_de_cliente"), all.x = TRUE)

# Columna para presencia en dos meses posteriores
dt <- merge(dt, clientes_por_mes_dt[, .(foto_mes = foto_mes - 2, numero_de_cliente, presente_dos_meses = TRUE)],
            by = c("foto_mes", "numero_de_cliente"), all.x = TRUE)

# Relleno los valores NA con FALSE, porque los clientes que no aparecen en el siguiente mes se marcan como ausentes
dt[, presente_siguiente := fifelse(is.na(presente_siguiente), FALSE, presente_siguiente)]
dt[, presente_dos_meses := fifelse(is.na(presente_dos_meses), FALSE, presente_dos_meses)]

# Asigno la clase ternaria
dt[, clase_ternaria := fifelse(presente_siguiente & presente_dos_meses, "CONTINUA",
                    fifelse(presente_siguiente & !presente_dos_meses, "BAJA+2",
                    fifelse(!presente_siguiente & !presente_dos_meses, "BAJA+1", NA_character_)))]

# Resultado
print(head(dt[, .(foto_mes, numero_de_cliente, clase_ternaria)]))

# Chequeo la proporción de cada clase
proporciones <- dt[, .N, by = clase_ternaria][, .(clase_ternaria, porcentaje = round((N / sum(N)) * 100, 2))]
print(proporciones)

# Exporto el dataset a un nuevo archivo CSV para la primera entrega
fwrite(dt, "competencia_01.csv")
