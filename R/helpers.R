findnonmissing <- function(x){
  !is.na(x) &
    !grepl("(unclassified|uncultured|metagenome|unidentified|unknown)", x, ignore.case = TRUE) &
    x != "human_gut" & x != ""
}
