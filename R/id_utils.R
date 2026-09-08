# transform id into kids2health project format for ids
standardize_id <- function(df, id_col) {
  
  id_col <- rlang::sym(id_col)
  
  df <- df %>%
    mutate(
      !!id_col := str_trim(!!id_col), # trim whitespace
      !!id_col := tolower(!!id_col), # convert to lowercase
      # if the id has 7 characters and ends with '1', remove the last character
      !!id_col := if_else(nchar(!!id_col) == 7 & stringr::str_sub(!!id_col, -1) == "1",
                          stringr::str_sub(!!id_col, 1, -2), !!id_col),
      # insert underscores after the first 2 characters and before the last character
      !!id_col := stringr::str_replace(!!id_col, "^(..)(.*)(.)$", "\\1_\\2_\\3")
    )
  
  return(df)
}

# check pattern of the ids in the id column after standardization
check_id_format <- function(df, id_col, id_pattern = "^[a-z]\\d_\\d{3}_[012]$") {
  
  id_col <- rlang::sym(id_col)
  
  match_pattern <- all(str_detect(df$id_col, id_pattern))
  if(!match_pattern) {
    failing_cases <- df %>%
      filter(!str_detect(id, id_pattern))
    print(paste("IDs not matching the expected pattern:", failing_cases$id, sep = " "))
    stop("IDs are not in the expected format!")
  } else {
    print("All IDs match the expected k2h pattern.")
  }
}
