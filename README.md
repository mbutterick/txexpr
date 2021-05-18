## txexpr ![Build Status](https://github.com/mbutterick/txexpr/workflows/CI/badge.svg)


Convenience functions for working with HTML-style tagged X-expressions in Racket 6.0+.

Install from the command line like so:

    raco pkg install txexpr

Then require it in your Racket file, in standard mode:

    (require txexpr)
    
In safe mode (with contracts):

    (require (submod txexpr safe))

Full docs are installed with the package. You can also [read the docs here](http://pkg-build.racket-lang.org/doc/txexpr).

## License

MIT


## Project status

Complete. I will maintain the code but no major updates are planned. Certain aspects of the API and implementation could be better, because much of this code was originally written during a more naive era of personal Racketeering. Still, it gets the job done, and I personally lack enthusiasm for diving back in.
