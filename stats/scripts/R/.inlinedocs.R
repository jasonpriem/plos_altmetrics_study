### This function will be applied to every object in your package.
author.from.author <- function(desc,...){
  list(author=desc[,"Author"])
### Returns a list with named values that will be used to add
### documentation to Rd files.
}

## author.from.description is the name of the default parser for the
## author section. We copy all the other parsers functions in the
## list, then add our custom author parser to the end of the list.
parsers <-
  c(default.parsers["author.from.description"!=names(default.parsers)],
    forall(author.from.author))
## forall(FUN) returns a function that will apply FUN to all
## documentation objects in your package. Likewise, forfun(FUN) will
## apply FUN to all functions in your package.

