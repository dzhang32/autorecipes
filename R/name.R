setGeneric("name", function(x) standardGeneric("name"))

setMethod("name", "Ingredients", function(x) x@name)
