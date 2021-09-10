# MathGame

Haskell Project basado en  stack con GLOSS como motor grafico

#Requisitos

1. GHC *El compilador de haskell*
2. Stack
3. Cabal
4. Haskell Extension for VScode/IntellijIDEA

____

5. Vim / Emacs Instructions proximamente


# Documentacion \ Tutoriales

Aca va la documentacion oficial de la librerias y otros que se añadan al proyecto

- [Stack](https://docs.haskellstack.org/en/stable/README/)
- [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html)
- [Gloss](http://gloss.ouroborus.net/)
- [Tutorial 1](https://andrew.gibiansky.com/blog/haskell/haskell-gloss/)
- [Tutorial 2](https://mmhaskell.com/blog/2019/3/25/making-a-glossy-game-part-1)
- [Tutorial 3](https://blog.jayway.com/2020/11/01/making-a-small-game-with-gloss/)
- [Tutorial 4]()
---
- [Gloss Wiki](https://hackage.haskell.org/package/gloss)
-

###Extras
> No son necesarios pero es bueno saberlos
> 
> Añadir aca cosas no requeridas por el proyecto pero utiles de todas formas
- [Vim Cheatsheet](https://vim.rtorr.com/)
- 

---

Añadan  aca todos los tutoriales y guias utiles que encuentren

- [Cosas que necesitas saber al aprender Haskell](http://dev.stephendiehl.com/hask/)



# Haskell Instrucciones

## Primera Instalacion

Ejecutar solo la primeara vez que se cola el repositorio

```shell
stack setup
```

****

## Build and Test

### Build

Construye y ejecuta el codigo

```shell
stack build
stack exec MathGame-exe
```

Alternativa

``` shell
stack run
```

### REPL

Ejecuta el codigo y observe por los cambios *(Similar al Live Server de Web)*

```shell
stack repl
```

> Aun no lo pruebo con este codigo pero en otros proyectos me funciono bien

----
## Arch Linux notas

Notas para hacer funcionar el codigo en Arch Linux (No tengo la menor idea de Devian)

### Paquetes

- freegult *pacman*
- stack *pacman*
- cabal-install *pacman*
- ghc *pacman*
----
