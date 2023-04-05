# TodoMVC App written in Haskell using htmx

All styles taken from [https://todomvc.com](https://todomvc.com/) and their npm packages

This project uses [htmx](https://htmx.org/) which allows for rich applications with no (or little) Javascript!

The entire application was written Haskell

## To run
Ensure you have [Haskell](https://www.haskell.org/ghcup/) installed

`cabal install ghcid`

If on Linux or macos:
```
    :> make
```
If on Windows:
```
    :> ghcid -c cabal repl -W -T main
```

And navigate to [http://localhost:8080/](http://localhost:8080/)