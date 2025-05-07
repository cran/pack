#-------------------------------------------------------------------------#
# pack R package, copyright (C) Joshua M. Ulrich, 2007-2008               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

pack <-
function(template, ...)
{
    # http://perldoc.perl.org/functions/pack.html

    template <- unlist(strsplit(template, "\\s"))
    values <- list(...)

    types <- gsub("[0-9]|\\*", "", template)
    bytes <- gsub("[[:alpha:]]|/", "", template)
    bytes <- gsub("\\*", "-1", bytes)
    suppressWarnings(bytes <- as.numeric(bytes))
    result <- NULL

    # Loop over template / value pairs
    shift <- 0
    pad <- c(a = as.raw(0), A = charToRaw(" "))
    for (i in seq_along(template)) {

        value <- values[i-shift][[1]]
        type <- types[i]
        byte <- bytes[i]
        nul <- raw(0)

        if (is.null(value)) {
            val <- as.raw(0)  # template requires more args than given
        } else if (regexpr("/", type, fixed = TRUE) > 0) {
            # Packed item count followed by packed items
            seq <- strsplit(type, "/")[[1]]
            len <- nchar(value)
            val <- c(pack(seq[1], len),
                     pack(paste0(seq[2], len), value))
        } else {
            val <-
                switch(type,
                       a = ,
                       A = {
                           value <- as.character(value)
                           if (byte == -1) {
                               byte <- nchar(value)  # "a*" or "A*"
                           }
                           if (nchar(value) > byte) {
                               stop("list value (", value ,") too large for template value", call. = FALSE)
                           }
                           nul <- rep(pad[[type]], byte - nchar(value))  # null/space padding
                           charToRaw(value)
                       },
                       b = packBits(value),      # bit string, low-to-high order
                       B = packBits(rev(value)), # bit string, high-to-low order
                       C = numToRaw(value, 1),   # unsigned char (octet) value
                       v = numToRaw(value, 2),   # unsigned short (16-bit), little-endian
                       V = numToRaw(value, 4),   # unsigned long  (32-bit), little-endian
                       d = writeBin(as.numeric(value), raw(), 8L),  # double-precision float (native format)
                       f = writeBin(as.numeric(value), raw(), 4L),  # single-precision float (native format)
                       x = {
                           shift <- shift + 1
                           as.raw(0)  # nul byte
                       },
                       stop(sQuote(type), " is an unsupported template value"))
        }

        # Combine result
        result <- c(result, val, nul)
    }
    return(result)
}
