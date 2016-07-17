format_table <- function(x, y, type="f", add.total=T){
  tt <- table(x,y)
  nr <- dim(tt)[1]
  nc <- dim(tt)[2]
  if (type=="c") {
    if (add.total){
      tt <- cbind(tt, rowSums(tt))
      colnames(tt)[nc+1] <- "Total"}
    tt <- rbind(round(100*prop.table(tt,2),1), rep(100,nc+1))}
  if (type=="r") {
    if (add.total){
      tt <- rbind(tt, colSums(tt))
      rownames(tt)[nr+1] <- "Total"}
    tt <- cbind(round(100*prop.table(tt,1),1), rep(100,nr+1))}
  if (type=="f") {
    if (add.total){
      tt <- rbind(tt, colSums(tt))
      tt <- cbind(tt, rowSums(tt))
      rownames(tt)[nr+1] <- "Total"
      colnames(tt)[nc+1] <- "Total"}
    }
  tt
  }

# dat <- c(1,2,3,3,2,2,2,2,1,1,3)
# dat1 <- c(1,2,3,2,2,2,2,2,1,1,2)
# format_table(dat, dat1, type = "c")
# format_table(dat, dat1, type = "r")
# format_table(dat, dat1, type = "f")
