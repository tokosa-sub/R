innerproduct<-function (x, y = NULL) 
{
    if (is.matrix(x) && is.null(y)) {
        co = array(0, c(ncol(x), ncol(x)))
        f = colnames(x)
        dimnames(co) = list(f, f)
        for (i in 2:ncol(x)) {
            for (j in 1:(i - 1)) {
                co[i, j] = innerproduct(x[, i], x[, j])
            }
        }
        co = co + t(co)
        diag(co) = 1
        return(as.matrix(co))
    }
    else if (is.vector(x) && is.vector(y)) {
        return(crossprod(x, y))
    }
    else {
        stop("argument mismatch. Either one matrix or two vectors needed as input.")
    }
}

