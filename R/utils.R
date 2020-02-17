desctats <- function(x, name = NULL, digits = 3, use_percent = T, percentStr=T, max_levels = 6, dropna = T) {
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
  } else if (is.logical(x)) {
    n_cases <- sum(x, na.rm = T)
    if (use_percent) {
      if (dropna) {
        warn(glue::glue("Samples contains missing values for {name} were dropped before the calculation."))
        percent <- round(mean(x, na.rm = T), digits = digits)
      } else {
        warn(glue::glue("Samples contains missing values for {name} were counted in the denominator."))
        percent <- round(n_cases / length(x), digits = digits)
      }
      if (percentStr) {
        return(as.character(glue::glue("{format(round(percent * 100, digits = digits - 2), nsmall = digits - 2)}%")))
      } else {
        return(as.character(round(percent * 100, digits = digits - 2)))
      }
    } else {
      return(n_cases)
    }

  }
  else if (is.factor(x)) {
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
          destr = glue::glue_col("{f}:{round(p * 100, digits = digits - 2)}%"))
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
show_stats <- function(df, digits = 3, use_percent = T, max_levels = 6, dropna = T) {
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
show_stats_by_group <- function(df, group_var, digits = 3, use_percent = F, max_levels = 6, dropna = T) {
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
show_stats_by_group_strata <- function(df, group_var, strata, digits = 3, use_percent = T, max_levels = 6, dropna = T) {
  note(glue::glue("Group by {dplyr::quo_name(enquo(group_var))} and strata by {dplyr::quo_name(enquo(strata))}."))
  stratum <- unique(df[[dplyr::quo_name(enquo(strata))]])
  dfs <- list()
  for (name in stratum) {
    dfs[[name]] <- df %>% dplyr::filter(!!enquo(strata) == name) %>% select(-!!enquo(strata)) %>%
      show_stats_by_group(!!enquo(group_var), digits, use_percent, max_levels, dropna)
  }
  return(dplyr::bind_rows(dfs, .id = dplyr::quo_name(enquo(strata))))
}
