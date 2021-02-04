beta_diversity_3d <- function(x, ...){
  UseMethod("beta_diversity_3d", x)
}

beta_diversity_3d.data.frame <- function(x, axes = colnames(x)[1:3],
                                         color.column = colnames(x)[4],
                                         label.column = colnames(x)[5],
                                         color.key = NULL,
                                         ...){
  if(length(axes) != 3) stop("Need three axes for 3D scatter plot")
  if(length(color.column) != 1) stop("Can only have one color column")
  if(length(label.column) != 1) stop("Can only have one label column")
  if(!all(axes %in% colnames(x))) stop("Axes must be column names in x")
  if(!color.column %in% colnames(x)) stop("color.column must be in column names of x")
  if(!label.column %in% colnames(x)) stop("label.column must be in column names of x")
  
  # make a color scheme using dittoSeq for a categorical color column
  if(is.null(color.key)){
    if(is.character(x[[color.column]])){
      catvals <- unique(x[[color.column]])
    }
    if(is.factor(x[[color.column]])){
      catvals <- levels(x[[color.column]])
    }
    if(is.factor(x[[color.column]]) || is.character(x[[color.column]])){
      color.key <- dittoSeq::dittoColors()[seq_along(catvals)]
      names(color.key) <- catvals
    }
  }
  if(is.factor(x[[color.column]])){
    x[[color.column]] <- as.character(x[[color.column]])
  }
  if(is.character(x[[color.column]]) && !all(x[[color.column]] %in% names(color.key))){
    stop("color.key needs names for all values in color column.")
  }
  
  # make plotly object
  p <- plot_ly(x,
               x = reformulate(axes[1]),
               y = reformulate(axes[2]),
               z = reformulate(axes[3]),
               color = reformulate(color.column),
               colors = ~color.key,
               text = reformulate(label.column),
               type = "scatter3d", mode = "markers",
               ...)
  return(p)
}