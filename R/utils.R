desctats <- function(x, name = NULL, digits = 3, use_percent = F, max_levels = 6, dropna = F) {
  if (is.character(x)) {
    x <- as_factor(x)
  }

  if (is.null(name)) {
    name <- 'variable'
  }

  if (is.numeric(x)) {
    mean_val <- round(mean(x, na.rm = T), digits = digits)
    sd_val <- round(sd(x, na.rm = T), digits = digits)
    return(as.character(glue::glue("{format(mean_val, nsmall = digits)} ({format(sd_val, nsmall = digits)})")))
  } else if (is.factor(x)) {
    if (dropna) {
      x <- na.omit(x)
    }
    n_levels <- length(levels(x))
    if (n_levels > max_levels) {
      bad(glue::glue("{name} has too many levels ({n_levels})."))
      return(as.character(glue::glue("Too many levels ({n_levels}).")))
    } else {
      if (use_percent) {
        destr <- forcats::fct_count(x, prop = T) %>% mutate(
          destr = glue::glue_col("{f}:{round(p * 100)}%"))
      } else {
        destr <- forcats::fct_count(x) %>% mutate(destr = glue::glue_col("{f}: {n}"))
      }
      return(as.character(stringr::str_c(destr$destr, collapse = '; ')))
    }
  } else {
    bad("Input is neither numeric nor factor. Return NA.")
    return("NA")
  }
}


#' Show statistics for all the columns of a data frame
#' @export
show_stats <- function(df, digits = 3, use_percent = F, max_levels = 6, dropna = F) {
  stat <- df %>%
    purrr::map2_dfc(., colnames(.), desctats,
                    digits = digits,
                    use_percent = use_percent,
                    max_levels = max_levels,
                    dropna = dropna) %>%
    gather(key = "Variable", value = "Statistics")

  counts <- c(Variable = 'N', Statistics = nrow(df))

  return(bind_rows(counts, stat))
}


#' Show statistics for all the columns of a data frame group by the group_var
#' @export
show_stats_by_group <- function(df, group_var, digits = 3, use_percent = F, max_levels = 6, dropna = F) {
  df %>% group_by(!!enquo(group_var)) %>% nest() %>%
    mutate(stats = map(data, show_stats, digits, use_percent, max_levels, dropna)) %>%
    arrange(!!enquo(group_var)) %>% select(-data) %>%
    unnest(stats) %>% pivot_wider(
      id_cols = Variable,
      names_from = !!enquo(group_var),
      values_from = Statistics)
}


#' Show statistics for all the columns of a data frame group by group_var and stratify by strata
#' @export
show_stats_by_group_strata <- function(df, group_var, strata, digits = 3, use_percent = F, max_levels = 6, dropna = F) {
  note(glue::glue("Group by {dplyr::quo_name(enquo(group_var))} and strata by {dplyr::quo_name(enquo(strata))}."))
  stratum <- unique(df[[dplyr::quo_name(enquo(strata))]])
  dfs <- list()
  for (name in stratum) {
    dfs[[name]] <- df %>% dplyr::filter(!!enquo(strata) == name) %>% select(-!!enquo(strata)) %>%
      show_stats_by_group(!!enquo(group_var), digits, use_percent, max_levels, dropna)
  }
  return(dplyr::bind_rows(dfs, .id = dplyr::quo_name(enquo(strata))))
}
