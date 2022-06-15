# procedural
random_walk <- function(n = 1000) {
  position <- 0
  walk <- c()
  for (i in seq_len(n)) {
    step <- sample(c(-1, 1), size = 1)
    position <- position + step
    walk <- c(walk, position)
  }
  walk
}

# procedural with pre-allocated vector
random_walk_pre <- function(n = 1000) {
  position <- 0
  walk <- vector(mode = "numeric", length = n)
  for (i in seq_len(n)){
    step <- sample(c(-1, 1), size = 1)
    position <- position + step
    walk[i] <- position
  }
  walk
}

# vectorized
random_walk_v <- function(n = 1000) {
  steps <- sample(c(-1, 1), size = n, replace = TRUE)
  cumsum(steps)
}

bench::mark(
  random_walk(),
  random_walk_pre(),
  random_walk_v(),
  check = FALSE)

# find_crossing_1 is a translation of python version
# following link provides multiple other options:
# https://stat.ethz.ch/pipermail/r-help/2012-February/303756.html
# included a few for testing here

find_crossing_1 <- function(seq, sub) {
  n <- length(seq)
  m <- length(sub)
  out <- c()
  for (i in seq_len(n - m + 1)) {
    if (identical(seq[i:(i + m - 1)], sub)) out <- c(out, i)
  }
  out
}

# slightly modified version of occur1 from link
# it is iteratively filtering candidate
find_crossing_2 <- function(seq, sub) {
  n <- length(seq)
  m <- length(sub)
  candidate <- seq_len(n - m + 1)
  for (i in seq_len(m)) {
    candidate <- candidate[sub[i] == seq[candidate + i - 1]]
  }
  candidate
}

# slightly modified version of occur3 from link
# reversing is based on how embed creates the matrix
# vector formed by repeat can be compared directly to matrix, w
# rowSums indicates if all columns match the values in sub
find_crossing_3 <- function(seq, sub) {
  w <- embed(seq, length(sub))
  which(rowSums(w == rep(rev(sub), each = nrow(w))) == ncol(w))
}

# slightly modified version of occur4 from link
# wasn't found to be very fast but I find it highly readable
find_crossing_4 <- function(seq, sub) {
  which(zoo::rollapply(seq, length(sub), identical, sub, fill = FALSE, align = "left"))
}

# trying data.table version of rollapply
# need to jump through some extra hoops because not able to work directly with logical vectors
# roughly 10x as fast as zoo version but not nearly as fast as find_crossing_2
find_crossing_5 <- function(seq, sub) {
  identical_mod <- function(x, y) if (identical(x, y)) 1 else 0
  x <- data.table::frollapply(seq, length(sub), identical_mod, sub, align = "left")
  which(x == 1)
}

w <- random_walk_v()
bench::mark(
  find_crossing_1(w, c(1, 0, -1)),
  find_crossing_2(w, c(1, 0, -1)),
  find_crossing_3(w, c(1, 0, -1)),
  find_crossing_4(w, c(1, 0, -1)),
  find_crossing_5(w, c(1, 0, -1))
)
