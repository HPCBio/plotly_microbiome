composition_df <- function(psobj, rank = "Family",
                           keepcols = c("Sample", "Group", "Label", "ID"),
                           minprop = 0.05, mean_across_samples = NULL){
  if(!all(keepcols %in% colnames(phyloseq::sample_data(psobj)))){
    stop("All column names in keepcols should be found in sample_data(psobj).")
  }
  if(!is.null(mean_across_samples) && !mean_across_samples %in% keepcols){
    stop("mean_across_samples should be a column in keepcols.")
  }
  
  otus <- phyloseq::otu_table(psobj)@.Data
  if(!phyloseq::taxa_are_rows(psobj)){
    otus <- t(otus)
  }
  grps <- as.vector(phyloseq::tax_table(psobj)[,rank])
  subs <- findnonmissing(grps)
  grps[!subs] <- "Unclassified"
  rs <- rowsum(otus, grps) # sum counts by taxonomic group
  rs_norm <- sweep(rs, 2, colSums(rs), "/") # normalize to proportions
  # identify labels to keep
  keeptaxa <- rowSums(rs_norm >= minprop) > 0
  if(!all(keeptaxa)){
    rs <- rbind(rs[keeptaxa,],
                Other = colSums(rs[!keeptaxa,]))
    rs_norm <- rbind(rs_norm[keeptaxa,],
                     Other = colSums(rs_norm[!keeptaxa,]))
  }
  
  # Get everything into long format for ggplot
  df1 <- data.frame(ID = colnames(rs), t(rs), check.names = FALSE)
  df2 <- data.frame(ID = colnames(rs_norm), t(rs_norm), check.names = FALSE)
  df1 <- tidyr::pivot_longer(df1, !.data$ID, names_to = rank,
                             values_to = "Counts")
  df2 <- tidyr::pivot_longer(df2, !.data$ID, names_to = rank,
                             values_to = "Proportion")
  dfout <- suppressMessages(dplyr::left_join(df1, df2))
  sams <- data.frame(phyloseq::sample_data(psobj)[,keepcols])
  if(!"ID" %in% keepcols){
    sams$ID <- rownames(sams)
  }
  dfout <- suppressMessages(dplyr::left_join(dfout, sams))
  lvls <- rownames(rs_norm)[order(rowSums(rs_norm), decreasing = TRUE)]
  if("Other" %in% lvls){
    lvls <- c(setdiff(lvls, "Other"), "Other")
  }
  if("Unclassified" %in% lvls){
    lvls <-  c(setdiff(lvls, "Unclassified"), "Unclassified")
  }
  dfout[[rank]] <- factor(dfout[[rank]], levels = lvls)
  # summarize across sample groups if desired
  if(!is.null(mean_across_samples)){
    dfout <- dplyr::summarise(dplyr::group_by(dfout, .data[[mean_across_samples]], .data[[rank]]),
                     Proportion = mean(.data$Proportion), Counts = sum(.data$Counts), .groups = "drop_last")
    grplvls <- length(unique(sams[[mean_across_samples]]))
    # Add other columns back in if they have one value per group
    for(col in setdiff(keepcols, c(rank, mean_across_samples))){
      if(nrow(unique(sams[,c(mean_across_samples, col)])) == grplvls){
        dfout[[col]] <- sams[[col]][match(dfout[[mean_across_samples]], sams[[mean_across_samples]])]
      }
    }
  }
  return(dfout)
}
