gen_seq <- function(prefix="", txt, seqnum, sep, sufflix=""){
  len <- length(seqnum)
  numlen <- 1
  if (max(seqnum) > 10) numlen <- 2
  if (max(seqnum) > 100) numlen <- 3
  sequence <- paste0(txt, prefix0(seqnum[-len], numlen), sep=sep, collapse = "" )
  sequence <- paste0(prefix, sequence, txt, seqnum[len], sufflix)
  sequence
}
