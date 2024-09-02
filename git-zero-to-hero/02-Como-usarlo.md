# Cómo empezar?

La mejor forma de empezar es jugar y romper un poco. Para eso creemos un
directorio que luego vamos a borrar cuando terminemos.

Para agregar una pequeña cuota de maldad voy a mostrar como hacer todo desde
la terminal explicando conceptualmente qué se está haciendo. Lo mismo se puede
hacer (a veces) con herramientas graficas, pero seamos sinceros: nos gusta
hacernos los hackers. :P

# Salvandonos de mucho sufrimiento

Git necesita una configuración inicial para saber quienes somos y como queremos
editar archivos de texto plano. Git abre automáticamente el editor y espera su
cierre para tomar lo que uno escribió y pegarlo como comentario al árbol de
versiones. Detengámonos un minuto en este aspecto para evitarnos muchos dolores
de cabeza luego.

### Inicializar nuestro usuario

Configurar globalmente nuestro nombnre y mail:
```shell
$ git config --global user.name "Nuestro Nombre"
$ git config --global user.email nuestro@mail.com
```

Para trabajar en GitHub o GitLab además es necesario configurar llaves SSH.
Sigan [éstos pasos](https://docs.github.com/es/authentication/managing-commit-signature-verification/generating-a-new-gpg-key)
y que no panda el cúnico.

### Inicializar nuestro editor

Esta parte va en gustos, pero es necesario usar un editor que nos sintamos
cómodos. Vamos a escribir mucho en texto plano. Por defecto Git usa `vi` o su
derivado `vim`. Si no conocen ese tipo de editores, lo mejor es poner alguno
más amigable. Elijan alguno de los siguientes según preferencia pero solo uno.

#### VSCode

Se puede usar el editor VSCode:
```shell
$ git config --global core.editor 'code --wait'
```

#### Sublime

Hay varios por ahí que les gusta este editor:
```shell
$ git config --global core.editor 'sublime -nw'
```

#### Notepad++

Otra opción más sencilla es usar Notepad++
```shell
$ git config core.editor "'C:/Program Files (x86)/Notepad++/notepad++.exe' -multiInst -notabbar -nosession -noPlugin"
```

#### Notepad

Para los amantes del peligro y el sufrimiento también es posible usar Notepad
```shell
$ git config core.editor notepad
```

#### Lista de otros tantos

Pueden darle una mirada a [otras opciones](https://www.git-scm.com/book/en/v2/Appendix-C%3A-Git-Commands-Setup-and-Config)

## Iniciar un repositorio desde cero

Creamos un directorio:
```shell
$ mkdir primer-repo
```

Y nos movemos a él:
```shell
$ cd primer-repo
```

Ahora le decimos a Git que tiene que versionar este repositorio. Inicializamos
con `git init`:
```shell
$ git init
Inicializado repositorio Git vacío en /home/user/primer-repo/.git/
```

Git es muy verborrágico, le gusta explicar mucho y dar resultados escritos.
Eso es bueno porque todo el tiempo te da feedback del estado y donde estás
parado en el árbol.

Corroboremos qué estamos en un repositorio, para eso usamos `git status`, un
comando que usaremos todo el tiempo para saber como estamos:
```shell
$ git status
En la rama master

No hay commits todavía

no hay nada para confirmar (crea/copia archivos y usa "git add" para hacerles seguimiento)
```

Git nos dice que estamos en la rama `master` (siempre estamos en una rama,
`master` o `main` es la rama por defecto cuando se crea un repositorio); nos
dice que no hay "commits" todavía (ya explicaremos, no entren en pánico) y nos
dice que no hay nada para confirmar. Además nos explcia qué tenemos que hacer
para "confirmar" (lo que sea que sea eso).

## Ahora vamos a crear cosas

Parados en nuestro directorio versionado (nuestro repositorio) agreguemos un
archivo de texto simple que diga algo, lo que sea. Usen el editor de texto que
les guste, puede ser notepad inclusive, pero guarden ese archivo en el lugar
correcto para que git lo encuentre.

```shell
$ ls
texto-simple.txt
$ cat texto-simple.txt
Un primer archivo de texto simple
$
```

Admito que introduje [`cat`](https://es.wikipedia.org/wiki/Cat_(Unix)) que lo
que hace es mostrar el contenido de un archivo. Pueden verlo también en su editor
favorito.

Ahora que dice git?

```shell
$ git status
En la rama master

No hay commits todavía

Archivos sin seguimiento:
  (usa "git add <archivo>..." para incluirlo a lo que será confirmado)
        texto-simple.txt

no hay nada agregado al commit pero hay archivos sin seguimiento presentes (usa "git add" para hacerles seguimiento)
```

Descubrió que cambiamos algo. Y nos dice que está sin seguimiento. Incluso nos
lo pone en rojo (si nuestra terminal soport colores). Si leemos bien nos
indica el próximo paso.

## Commits? Con qué se comen?

Aquí viene lo bueno, jóvenes. Lo que todos estuvimos esperando tanto tiempo.
Git necesita que le digamos qué archivos versionar y que le digamos cuando y
qué partes.

Para decirle "monitoreame este archivo" se usa `git add`:
```shell
$ git add texto-simple.txt
$
```

Ahora que pasa si hacemos `git status`:

```shell
$ git status
En la rama master

No hay commits todavía

Cambios a ser confirmados:
  (usa "git rm --cached <archivo>..." para sacar del área de stage)
        nuevos archivos: texto-simple.txt
```

Ahora parece que el archivo que estaba en rojo pasó a verde. Y nos lo puso en
otra categoría "Cambios a ser confirmados". Entonces, como se sigue? Git
necesita que le digamos cuando terminamos de trabajar en nuestros archivos para
"sacarles una foto". Esa foto se llama "commit" o "confirmación". Es el momento
que git toma la diferencia entre el archivo antes y después y la guarda en su
árbol. Vamos a confirmar ("commitear") nuestros cambios:

```shell
$ git commit
```

Aquí Git nos abrirá el editor de texto configurado y nos pide que al menos
escriubamos una línea con lo que cambiamos. Es buena práctica hacer un resumen
de una línea, dejar un espacio en blanco y luego explicar todo lo que se hizo
y por qué en detalle.

```text
Generar archivo texto-simple.txt

Se genera este archivo de prueba con contenido simple para demostración
de las capacidades de Git.
```

Felicidades! Ya saben como versionar su código. Ahora veremos qué cosas se
pueden hacer con Git además.

## El ciclo general de trabajo

En general el ciclo de trabajo se resume en:

1. Entrar al repositorio
1. Codear y probar todo lo que queramos
1. Seleccionar los archivos modificados para confirmar
1. Confirmarlos
1. Enjuagar y Repetir

Ahora, para qué todo esto? Sólo tengo la última versión. Qué pasa si quiero
ver versiones anteriores? O si quiero deshacer algo que hice mal? O si quiero
ver qué se modificó en cada una de las versiones (commits)?

Los invito a la próxima.
