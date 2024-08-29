# Publicar nuestros cambios

Una vez que tengamos un remoto y nuestros cambios para publicar se hace de
manera muy sencilla con el comando `git push`.

```shell
$ git push <remoto> <rama>
```

# Traernos los cambios de remotos

A la vez es sencillo traernos los cambios que estan subidos por otros usuarios.
Con nuestro remoto configurado, hacemos `git pull`.

```shell
$ git pull <remoto> <rama>
```

# El ciclo normal de desarrollo colaborativo

En general el ciclo general de trabajo que vimos antes se completa con pull y
push.

1. Entrar al repositorio
1. Codear y probar todo lo que queramos
1. Seleccionar los archivos modificados para confirmar
1. Confirmarlos
1. Pullear los cambios de los demás
1. Revisar y manejar los conflictos
1. Pushear nuestros cambios


# Y que pasa si se rompe?

Es muy común que en cada pull/push se encuentren conflictos. Esos se manjan de
la misma manera que ya se describió. Tener en cuenta que antes de cada push,
siempre es necesario un pull para manejar los conflictos de manera local. No se
preocupen, git les avisa cada vez que eso pase.

