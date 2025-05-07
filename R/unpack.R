#-------------------------------------------------------------------------#
# pack R package, copyright (C) Joshua M. Ulrich, 2007-2008               #
# Distributed under GNU GPL version 3                                     #
#-------------------------------------------------------------------------#

unpack <-
function(template, ...)
{
    # http://perldoc.perl.org/functions/unpack.html

    template <- unlist(strsplit(template,"\\s"))
    values <- unlist(list(...))

    types <- gsub("[0-9]|\\*", "", template)
    counts <- gsub("[[:alpha:]]|/", "", template)
    counts <- gsub("\\*", "-1", counts)
    suppressWarnings(counts <- as.numeric(counts))
    result <- list()

    # Loop over template / value pairs
    for (i in seq_along(template)) {

        type <- types[i]
        count <- counts[i]

        # A null byte
        if (type == "x") {
            values <- values[-1]
            next
        }

        # Process remaining values
        if (length(values) > 0) {
            # Check count
            if (is.na(count)) {
                count <- 1               # Letter without count
            } else if (count == -1) {
                count <- length(values)  # Letter with a '*'
            } else if (count > length(values)) {
                stop("template element ", type, count,
                     " is longer than the number of values left to unpack")
            }
            # Process values
            if (regexpr("/", type, fixed = TRUE) > 0) {
                # Packed item count followed by packed items
                seq <- strsplit(type, "/")[[1]]
                num <- unpack(paste0(seq[1], " H*"), values)
                val <- unpack(paste0(seq[2], num[[1]], " H*"), num[[2]])
                values <- val[[2]]
                val <- unlist(val[[1]])
            } else {
                # number of 'values' vector elements to unpack
                n <-
                    switch(type,
                           H = count,
                           a = count,
                           A = count,
                           b = 1,
                           B = 1,
                           C = 1,
                           v = 2,
                           V = 4,
                           d = 8,
                           f = 4,
                           stop(sQuote(type), " is an unsupported template value"))


                # unpacked value
                uv <- values[seq_len(n)]
                val <-
                    switch(type,
                           H = uv,                              # Hex string - high nibble first
                           a = ,
                           A = {
                               tmp <- uv
                               tmp <- rawToChar(uv[as.logical(uv)])  # remove embedded nulls (a)
                               sub(" +$", "", tmp)                   # remove trailing spaces (A)
                           },
                           b = rawToBits(uv),                   # bit string, low-to-high order
                           B = rev(rawToBits(uv)),              # bit string, high-to-low order
                           C = as.integer(uv),                  # unsigned char (octet) value
                           v = rawToNum(uv, 2),                 # unsigned short (16-bit), little-endian
                           V = rawToNum(uv, 4),                 # unsigned long  (32-bit), little-endian
                           d = readBin(uv, "numeric", 1L, 8L),  # double-precision float (native format)
                           f = readBin(uv, "numeric", 1L, 4L),  # single-precision float (native format)
                           stop(sQuote(type), " is an unsupported template value"))

                # remove unpacked values
                values <- values[-seq_len(n)]
            }
        } else {
            val <- NULL  # no remaining values
        }

        # Combine result
        result <- c(result, list(val))
    }
    return(result)
}
