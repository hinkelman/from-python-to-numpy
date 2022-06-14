# procedural
random_walk <- function(n = 1000) {
    steps <- vector(mode = "numeric", length = n)
    for (i in seq_len(n)){
        steps[i] <- sample(c(-1, 1), size = 1)
    }
    c(0, steps)
}

# naive approach using concatenation
random_walk_c <- function(n = 1000) {
    steps <- 0
    for (i in seq_len(n)) {
        steps <- c(steps, sample(c(-1, 1), size = 1))
    }
    steps
}


# vectorized
random_walk_v <- function(n = 1000) {
    steps <- sample(c(-1, 1), size = n, replace = TRUE)
    cumsum(c(0, steps))
}

bench::mark(
    random_walk(),
    random_walk_c(),
    random_walk_v(),
    check = FALSE)

# current version below is relatively straightforward translation of python version
# see this link for multiple other options: https://stat.ethz.ch/pipermail/r-help/2012-February/303756.html

find_crossing_1 <- function(seq, sub){
    out <- c()
    sublen <- length(sub) - 1    # adjustment for 1-based indexing
    for (i in seq_len(length(seq) - sublen)) {
        if (identical(seq[i:(i + sublen)], sub)) out <- c(out, i)
    }
    out
}

occur1 <- function(exmpl, patrn) {
    m <- length(patrn)
    n <- length(exmpl)
    candidate <- seq_len(n-m+1)
    for (i in seq_len(m)) {
        candidate <- candidate[patrn[i] == exmpl[candidate + i - 1]]
    }
    candidate
  }

W <- random_walk_v()
bench::mark(
    find_crossing_1(W, c(1, 0, -1)),
    occur1(W, c(1, 0, -1))
)