modal_filter_df <- function(df,
                            class_col,
                            filter_span = 5,
                            new_col = "pred_class_filt",
                            na.rm = TRUE,
                            majority_threshold = 0.5) {
  
  if (!class_col %in% names(df)) stop("Column '", class_col, "' not found.")
  if (new_col %in% names(df)) stop("Column '", new_col, "' already exists.")
  
  x <- df[[class_col]]
  n <- length(x)
  s <- as.integer(filter_span)
  
  if (is.na(s) || s < 0L) stop("filter_span must be a non-missing integer >= 0.")
  if (!is.numeric(majority_threshold) || length(majority_threshold) != 1L ||
      is.na(majority_threshold) || majority_threshold <= 0 || majority_threshold > 1) {
    stop("majority_threshold must be a single number in (0, 1].")
  }
  
  is_factor <- is.factor(x)
  lev <- if (is_factor) levels(x) else NULL
  
  # Inner helper: mode only if strict majority > threshold; else keep original (fallback)
  mode_if_majority <- function(v, fallback) {
    if (na.rm) v <- v[!is.na(v)]
    if (!length(v)) return(fallback)
    
    tab <- table(v)
    top_n <- max(tab)
    top_prop <- top_n / sum(tab)
    
    if (top_prop > majority_threshold) {
      names(tab)[which.max(tab)]  # deterministic tie-break
    } else {
      fallback
    }
  }
  
  out <- x
  if (!(s == 0L || n <= 1L)) {
    for (k in seq_len(n)) {
      left  <- max(1L, k - s)
      right <- min(n,  k + s)
      out[k] <- mode_if_majority(x[left:right], fallback = x[k])
    }
  }
  
  # Append as factor with same levels, if original was factor
  if (is_factor) {
    df[[new_col]] <- factor(out, levels = lev)
  } else {
    # keep numeric type if numeric-coded; otherwise character is fine
    df[[new_col]] <- if (is.numeric(x)) as.numeric(out) else out
  }
  
  df
}


#Example Useage

#df_filt <- modal_filter_df(
  #scored_dataset4,
  #class_col = "pred_class",
  #filter_span = 5,
  #new_col = "pred_class_filt",
  #majority_threshold = 0.5
#)

#Predicted activity classes were temporally smoothed using a centred modal filter 
#spanning ±5 windows (44 s total), with labels updated only when the modal class 
#represented a strict majority (>50%) of the window.

#Predicted activity classes were temporally smoothed using a centred modal filter spanning
#±5 consecutive 4-s windows (11-window kernel; 44 s total). The class label at each window 
#was updated only when the modal class accounted for a strict majority (>50%) of the surrounding windows.
