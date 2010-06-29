setMethod("length", signature(x = "Wave"), 
function(x){
    validObject(x)
    length(x@left)
})
