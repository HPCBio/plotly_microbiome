findnonmissing <- function(x){
  !is.na(x) &
    !grepl("(unclassified|uncultured|metagenome|unidentified)", x, ignore.case = TRUE) &
    x != "human_gut" & x != ""
}
