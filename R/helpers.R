findnonmissing <- function(x){
  !is.na(x) &
    !grepl("(unclassified|uncultured|metagenome|unidentified|unknown)", x, ignore.case = TRUE) &
    x != "human_gut" & x != ""
}

make_taxa_labels <- function(taxtab){
  species_temp <- taxtab[,"Species"]
  species_temp[!findnonmissing(species_temp)] <- "sp."
  out <- character(nrow(taxtab))
  for(rnk in colnames(taxtab)[-ncol(taxtab)]){
    nm <- findnonmissing(taxtab[,rnk])
    out[nm] <- taxtab[nm,rnk]
  }
  out <- paste(out, species_temp)
  names(out) <- rownames(taxtab)
  return(out)
}
