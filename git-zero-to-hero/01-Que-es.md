# Qué es?

Git es un software de control de versiones. No es el único, pero es el más
difundido en el mundo del software desarrollado colaborativamente. Nació como
una solución al problema de trabajar con muchos desarrolladores en el kernel
Linux.

# Qué hace?

Git toma un directorio que uno le indique y lo marca como un "repositorio",
es decir, ese directorio y todo lo que tiene debajo, pasa a estar "versionado".

Cuántas veces se vieron en esta situación con algún trabajo a entregar?
```
+----------+
|Documentos|
+----------+
 |---Trabajo_V1.doc
 |---Trabajo_VFinal.doc
 |---Trabajo_VFinal1.doc
 |---Trabajo_VFinal_Esta vez si.doc
```

Imaginen además tener que trabajar colaborativamente.

Git sólo muestra la última versión en el directorio actual:
```
+----------+
|Documentos|
+----------+
 |---Trabajo.doc
```

Pero a la vez mantiene un árbol (sí, es un árbol, incluso tiene ramas) de
todas las versiones anteriores:

```
* 8892467 (HEAD -> main, origin/main) Agregada bibliografia
* 50cd3c1 Corrección sobre el tema
* 087d5f2 Agregado Capítulo 2
* 395c174 Agregado Capítulo 1
* 1f7b109 Creación carátula, Título e Introducción
```

# Ok, me lo vendiste, cómo se usa?

Primero [instalarlo](https://git-scm.com/book/es/v2/Inicio---Sobre-el-Control-de-Versiones-Instalaci%C3%B3n-de-Git)

Luego amigarse con la línea de comandos. Vamos a usarla bastante. Para los
usuarios de VSCode ya lo tienen integrado abierto en el directorio adecuado.
Para los usuarios de Windows sin ninguna herramienta de este tipo lo más
prolijo es usar `git bash` o en la terminal `PowerShell` que se configura con
la instalación del sitio ofical de Git. No usen `cmd` a menos que les guste
pasar por mucho sufrimiento.

Y ahora a tomar los primeros pasos.
