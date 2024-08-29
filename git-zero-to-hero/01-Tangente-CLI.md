# Brevísimo recordatorio sobre la terminal, la línea de comandos

En muchos lugares no se usa el término "línea de comandos" sino "la terminal".
No es que seamos fanáticos de Tom Hanks actuando de europeo oriental varado en
un aeropuerto del amado país del norte. Históricamente las computadoras en su
albores tenían una unidad central de procesamiento que ocupaba una habitación
entera y varias pantallitas con teclados para interactuar con ellas en lugares
remotos (o no tanto) de forma concurrente. Luego de alguna evolución, se
estandarizó la [VT-100 como **LA** terminal](https://es.wikipedia.org/wiki/VT100).
A partir de ahí todos los sistemas Unix-like (incluso MacOS hoy en día) tienen
un pequeño programita que llaman "emulador de terminal" para exponer esa
misma funcionalidad.

Existen muchos programas que emulan una terminal pero todos tienen por debajo
lo que se llama `shell` o "cascarón" que son las formas de comunicarse con la
computadora. La que pasó a ser estandar fue la [Bourne Shell](https://es.wikipedia.org/wiki/Bourne_Shell)
y sus compatibles. Simplemente exponen una interfaz de comandos (de allí "línea
de comandos") para interactuar con el sistema.

En general los comandos básicos son los mismos en todas (incluso en PowerShell).
Repasemos lo básico.

Al existir miles de combinaciones y configuraciones, voy a usar el signo de
moneda (`$`) para indicar el "prompt", o sea cuando la terminal está lista para
recibir comandos

### Donde estoy?

Para saber el directorio actual de trabajo ("print working directory") se usa
el comando `pwd`:

```shell
$ pwd
/home/user
$
```

### Dónde voy?

Para saber a donde puedo ir, necesitaría listar los directori y archivos que hay.
Para eso se usa un programa que hace eso mismo ("list") llamado `ls`:

```shell
$ ls
Descargas     Dropbox     Imágenes  Público
Documentos    Escritorio  Música    Plantillas  Vídeos
$
```

Es un comando muy versátil y acepta muchas opciones para la visualización (no
todas disponibles en PowerShell):

```shell
$ ls -l
... # Lista los archivos y directorios con detalles

$ ls -lh
... # Lista detalles con forma legible por humanos (cambia el formato del tamaño)

$ ls -a
... # Lista TODO, incluso los archivos y directorios ocultos

$ ls <ruta-a-directorio>
... # Lista lo que hay en un direcotrio específico
```

Las opciones se pueden concatenar y combinar, en caso de usar una ruta para
verla, las opciones se suelen poner antes.

### Cómo me muevo?

Para cambiar de directorio se usa el comando `cd` ("Change Directory"):
```shell
$ cd <ruta-a directorio>
```

Se puede verificar con `pwd` el cambio.

Existen dos rutas "especiales" que son muy útiles con `cd`, una es el
"directorio actual" que se indica con `.` (punto simple) que lleva al mismo
directorio actual. El más interesante es el "directorio padre" indicado por
`..` (dos puntos) que permite "subir" un nivel en el árbol de directorios.
