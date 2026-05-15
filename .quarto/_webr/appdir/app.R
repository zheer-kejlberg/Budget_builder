library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr) 
library(tibble)
library(DT)
library(ggplot2)
library(scales)
library(plotly)

start_default <- floor_date(Sys.Date(), "month")
end_default <- start_default %m+% years(5) - days(1)

categories <- c(
  "Salary, clinician",
  "Salary for technicians",
  "Salary Ph.D. students",
  "Tuition fees for Ph.D. students",
  "Salary for postdocs",
  "Salary for research year (RY)",
  "Salary, employees",
  "Travel and accommodation",
  "Travel, extended - applicant",
  "Conferences",
  "Publication costs",
  "Communication and outreach",
  "Equipment",
  "Operating expenses",
  "Consumables",
  "Bench fee",
  "Project supplements",
  "Administrative expenses"
)

make_empty_posts <- function() {
  tibble(
    id = integer(),
    center = character(),
    post_name = character(),
    category = character(),
    start_date = as.Date(character()),
    end_date = as.Date(character()),
    fte = numeric(),
    value_mode = character(),
    value_unit = character(),
    constant_expr = character(),
    function_expr = character(),
    value_vector = list(),
    sum_multiplier = character(),
    sum_sites = character(),
    sum_statuses = character(),
    sum_posts = character(),
    note = character(),
    needs_amendment = logical(),
    application_status = character(),
    import_issues = character()
  )
}

make_empty_salaries <- function() {
  tibble(
    id = integer(),
    identifier = character(),
    name = character(),
    unit = character(),
    base_salary = numeric(),
    pension_mode = character(),
    pension_value = numeric(),
    own_pension_pct = numeric(),
    wage_supplement = numeric(),
    holiday_rate = numeric(),
    subtract_holiday = logical(),
    base_salary_monthly = numeric(),
    wage_supplement_monthly = numeric(),
    pension_amount_monthly = numeric(),
    own_pension_amount_monthly = numeric(),
    holiday_allowance_total_monthly = numeric(),
    holiday_allowance_monthly = numeric(),
    total_plus_holiday_salary_monthly = numeric(),
    total_salary_monthly = numeric(),
    base_salary_yearly = numeric(),
    wage_supplement_yearly = numeric(),
    pension_amount_yearly = numeric(),
    own_pension_amount_yearly = numeric(),
    holiday_allowance_total_yearly = numeric(),
    holiday_allowance_yearly = numeric(),
    total_plus_holiday_salary_yearly = numeric(),
    total_salary_yearly = numeric()
  )
}

make_default_salaries <- function() {
  ry  <- calc_salary_fields(12000,  "month", "percentage",  0,     0,    wage_supplement = 0, holiday_rate_pct = 0,    subtract_holiday = FALSE)
  cr  <- calc_salary_fields(48500,  "month", "percentage", 19.36, 33.3, wage_supplement = 0, holiday_rate_pct = 12.5, subtract_holiday = TRUE)
  phd <- calc_salary_fields(503543, "year",  "percentage", 17.1,  0,    wage_supplement = 0, holiday_rate_pct = 12.5, subtract_holiday = TRUE)
  tibble(
    id                                = 1L:3L,
    identifier                        = c("research_year", "clinical_researcher", "phd_student"),
    name                              = c("Research year", "Clinical Researcher", "PhD student"),
    unit                              = c("month", "month", "year"),
    base_salary                       = c(12000, 48500, 503543),
    pension_mode                      = c("percentage", "percentage", "percentage"),
    pension_value                     = c(0, 19.36, 17.1),
    own_pension_pct                   = c(0, 33.3, 0),
    wage_supplement                   = c(0, 0, 0),
    holiday_rate                      = c(0, 12.5, 12.5),
    subtract_holiday                  = c(FALSE, TRUE, TRUE),
    base_salary_monthly               = c(ry$base_salary_monthly,              cr$base_salary_monthly,              phd$base_salary_monthly),
    wage_supplement_monthly           = c(ry$wage_supplement_monthly,          cr$wage_supplement_monthly,          phd$wage_supplement_monthly),
    pension_amount_monthly            = c(ry$pension_amount_monthly,           cr$pension_amount_monthly,           phd$pension_amount_monthly),
    own_pension_amount_monthly        = c(ry$own_pension_amount_monthly,       cr$own_pension_amount_monthly,       phd$own_pension_amount_monthly),
    holiday_allowance_total_monthly   = c(ry$holiday_allowance_total_monthly,  cr$holiday_allowance_total_monthly,  phd$holiday_allowance_total_monthly),
    holiday_allowance_monthly         = c(ry$holiday_allowance_monthly,        cr$holiday_allowance_monthly,        phd$holiday_allowance_monthly),
    total_plus_holiday_salary_monthly = c(ry$total_plus_holiday_salary_monthly, cr$total_plus_holiday_salary_monthly, phd$total_plus_holiday_salary_monthly),
    total_salary_monthly              = c(ry$total_salary_monthly,             cr$total_salary_monthly,             phd$total_salary_monthly),
    base_salary_yearly                = c(ry$base_salary_yearly,               cr$base_salary_yearly,               phd$base_salary_yearly),
    wage_supplement_yearly            = c(ry$wage_supplement_yearly,           cr$wage_supplement_yearly,           phd$wage_supplement_yearly),
    pension_amount_yearly             = c(ry$pension_amount_yearly,            cr$pension_amount_yearly,            phd$pension_amount_yearly),
    own_pension_amount_yearly         = c(ry$own_pension_amount_yearly,        cr$own_pension_amount_yearly,        phd$own_pension_amount_yearly),
    holiday_allowance_total_yearly    = c(ry$holiday_allowance_total_yearly,   cr$holiday_allowance_total_yearly,   phd$holiday_allowance_total_yearly),
    holiday_allowance_yearly          = c(ry$holiday_allowance_yearly,         cr$holiday_allowance_yearly,         phd$holiday_allowance_yearly),
    total_plus_holiday_salary_yearly  = c(ry$total_plus_holiday_salary_yearly, cr$total_plus_holiday_salary_yearly, phd$total_plus_holiday_salary_yearly),
    total_salary_yearly               = c(ry$total_salary_yearly,              cr$total_salary_yearly,              phd$total_salary_yearly)
  )
}

make_default_categories <- function() {
  tibble(
    id = 1:19,
    name = c(
      "Uncategorized",
      "Salary, clinician",
      "Salary for technicians",
      "Salary Ph.D. students",
      "Tuition fees for Ph.D. students",
      "Salary for postdocs",
      "Salary for research year (RY)",
      "Salary, employees",
      "Travel and accommodation",
      "Travel, extended - applicant",
      "Conferences",
      "Publication costs",
      "Communication and outreach",
      "Equipment",
      "Operating expenses",
      "Consumables",
      "Bench fee",
      "Project supplements",
      "Administrative expenses"
    ),
    operator = rep("", 19),
    amount = rep(NA_real_, 19),
    per_unit = rep("", 19),
    is_default = rep(TRUE, 19),
    is_locked = c(TRUE, rep(FALSE, 18)),
    is_deleted = rep(FALSE, 19)
  )
}

make_default_sites <- function() {
  tibble(
    id = 1L,
    name = "Main",
    is_default = TRUE,
    is_locked = TRUE,
    is_deleted = FALSE
  )
}

calc_salary_fields <- function(base_salary, unit, pension_mode, pension_value, own_pension_pct,
                               wage_supplement = 0, holiday_rate_pct = 12.5, subtract_holiday = TRUE) {
  base_input <- as.numeric(base_salary)
  own_pct <- as.numeric(own_pension_pct)
  wage_supp_input <- as.numeric(wage_supplement)
  holiday_rate <- as.numeric(holiday_rate_pct) / 100
  if (is.na(base_input) || base_input < 0) stop("Base salary must be a non-negative number.", call. = FALSE)
  if (is.na(own_pct) || own_pct < 0) stop("Own part of pension (%) must be a non-negative number.", call. = FALSE)
  if (is.na(wage_supp_input) || wage_supp_input < 0) stop("Wage supplement must be a non-negative number.", call. = FALSE)
  if (is.na(holiday_rate) || holiday_rate < 0) stop("Holiday rate must be a non-negative number.", call. = FALSE)

  base_monthly <- if (identical(unit, "year")) base_input / 12 else base_input
  wage_supplement_monthly <- if (identical(unit, "year")) wage_supp_input / 12 else wage_supp_input

  pension_amount_monthly <- if (identical(pension_mode, "percentage")) {
    base_monthly * as.numeric(pension_value) / 100
  } else {
    raw_pension <- as.numeric(pension_value)
    if (identical(unit, "year")) raw_pension / 12 else raw_pension
  }
  if (is.na(pension_amount_monthly) || pension_amount_monthly < 0) {
    stop("Pension must be a non-negative number.", call. = FALSE)
  }

  own_pension_amount_monthly <- pension_amount_monthly * own_pct / 100
  # Holiday applies to base + own pension portion + wage supplement
  holiday_allowance_total_monthly <- base_monthly + own_pension_amount_monthly + wage_supplement_monthly
  holiday_allowance_monthly <- holiday_allowance_total_monthly * holiday_rate
  # Total including holiday but before holiday-absence deduction
  total_plus_holiday_salary_monthly <- base_monthly + pension_amount_monthly + wage_supplement_monthly + holiday_allowance_monthly
  # Final total: optionally apply 47/52 to account for holiday absence
  total_salary_monthly <- if (isTRUE(subtract_holiday)) total_plus_holiday_salary_monthly * 47 / 52 else total_plus_holiday_salary_monthly

  list(
    base_salary_monthly = base_monthly,
    wage_supplement_monthly = wage_supplement_monthly,
    pension_amount_monthly = pension_amount_monthly,
    own_pension_amount_monthly = own_pension_amount_monthly,
    holiday_allowance_total_monthly = holiday_allowance_total_monthly,
    holiday_allowance_monthly = holiday_allowance_monthly,
    total_plus_holiday_salary_monthly = total_plus_holiday_salary_monthly,
    total_salary_monthly = total_salary_monthly,
    base_salary_yearly = base_monthly * 12,
    wage_supplement_yearly = wage_supplement_monthly * 12,
    pension_amount_yearly = pension_amount_monthly * 12,
    own_pension_amount_yearly = own_pension_amount_monthly * 12,
    holiday_allowance_total_yearly = holiday_allowance_total_monthly * 12,
    holiday_allowance_yearly = holiday_allowance_monthly * 12,
    total_plus_holiday_salary_yearly = total_plus_holiday_salary_monthly * 12,
    total_salary_yearly = total_salary_monthly * 12
  )
}

month_sequence <- function(start_date, end_date) {
  seq(floor_date(start_date, "month"), floor_date(end_date, "month"), by = "1 month")
}

n_months_between <- function(start_date, end_date) {
  length(month_sequence(start_date, end_date))
}

n_years_between <- function(start_date, end_date) {
  ceiling(n_months_between(start_date, end_date) / 12)
}

safe_eval_expr <- function(expr_text, fte = NA_real_, n = 1L, extra_env = list()) {
  if (is.null(expr_text) || !nzchar(trimws(expr_text))) {
    stop("Expression cannot be empty.", call. = FALSE)
  }

  env <- new.env(parent = baseenv())
  env$FTE <- fte
  env$n <- n
  if (length(extra_env) > 0) {
    list2env(extra_env, envir = env)
  }

  value <- eval(parse(text = expr_text), envir = env)
  value
}

make_salary_lookup <- function(salaries_tbl) {
  if (!nrow(salaries_tbl)) {
    return(data.frame())
  }

  out <- salaries_tbl %>%
    transmute(
      identifier = identifier,
      base = base_salary_monthly,
      pension = pension_amount_monthly,
      own_pension = own_pension_amount_monthly,
      holiday_base = holiday_allowance_total_monthly,
      holiday = holiday_allowance_monthly,
      total_plus_holiday_m = total_plus_holiday_salary_monthly,
      total_plus_holiday_y = total_plus_holiday_salary_yearly,
      total_m = total_salary_monthly,
      total_y = total_salary_yearly
    ) %>%
    as.data.frame(stringsAsFactors = FALSE)

  rownames(out) <- out$identifier
  out$identifier <- NULL
  out
}

# Issue #4: Create salary environment objects for formula access (identifier$total_m style)
make_salary_env_objects <- function(salaries_tbl) {
  if (!nrow(salaries_tbl)) {
    return(list())
  }
  
  salaries_list <- list()
  for (i in seq_len(nrow(salaries_tbl))) {
    row <- salaries_tbl[i, ]
    identifier <- row$identifier
    # Create a one-row data frame for each salary
    salary_obj <- data.frame(
      base_m = row$base_salary_monthly,
      pension_m = row$pension_amount_monthly,
      own_pension_m = row$own_pension_amount_monthly,
      holiday_base_m = row$holiday_allowance_total_monthly,
      holiday_m = row$holiday_allowance_monthly,
      total_plus_holiday_m = row$total_plus_holiday_salary_monthly,
      total_plus_holiday_y = row$total_plus_holiday_salary_yearly,
      total_m = row$total_salary_monthly,
      total_y = row$total_salary_yearly,
      stringsAsFactors = FALSE
    )
    rownames(salary_obj) <- identifier
    salaries_list[[identifier]] <- salary_obj
  }
  salaries_list
}

expand_year_values <- function(values, n_months) {
  rep(values / 12, each = 12)[seq_len(n_months)]
}

# === WORKBOOK NAMING HELPERS (Issue #3) ===
sanitize_workbook_name <- function(name) {
  if (is.null(name) || !nzchar(trimws(name))) return("")
  # Allow letters, numbers, spaces, dash, underscore, parentheses
  sanitized <- gsub("[^A-Za-z0-9 _.()-]", "", trimws(name), perl = TRUE)
  substring(sanitized, 1, 80)
}

validate_workbook_name <- function(name) {
  if (is.null(name) || !nzchar(trimws(name))) {
    return("Workbook name is required.")
  }
  if (nchar(name) > 80) {
    return("Workbook name must be 80 characters or fewer.")
  }
  if (grepl("[/\\\\:|*?\"<>|]", name)) {
    return("Workbook name contains invalid characters.")
  }
  NULL
}

workbook_name_from_filename <- function(filename) {
  if (is.null(filename) || !nzchar(filename)) return("")
  basename(filename) %>%
    sub("\\.xlsx$", "", ., ignore.case = TRUE)
}

# === SALARY IDENTIFIER HELPERS (Issue #4) ===
generate_salary_identifier <- function(salary_name, existing_identifiers = character()) {
  if (is.null(salary_name) || !nzchar(trimws(salary_name))) {
    return("salary_1")
  }
  
  # Convert name to lowercase slug
  slug <- tolower(trimws(salary_name)) %>%
    gsub("[^a-z0-9]", "_", .) %>%
    gsub("_+", "_", .) %>%
    gsub("^_|_$", "", .)
  
  if (nchar(slug) == 0) slug <- "salary"
  
  # Check for collision
  if (!(slug %in% existing_identifiers)) {
    return(slug)
  }
  
  # Add numeric suffix for duplicates
  counter <- 1
  while (paste0(slug, "_", counter) %in% existing_identifiers) {
    counter <- counter + 1
  }
  paste0(slug, "_", counter)
}

# === CATEGORY VALIDATION HELPERS (Issue #2) ===
make_empty_category_registry <- function() {
  tibble(
    id = integer(),
    name = character(),
    operator = character(),
    amount = numeric(),
    per_unit = character(),
    is_default = logical(),
    is_locked = logical(),
    is_deleted = logical()
  )
}

validate_category_rule <- function(resolved_values, category_row, budget_start) {
  if (is.null(category_row) || !nrow(category_row)) return(NULL)
  operator <- trimws(category_row$operator[[1]])
  threshold <- suppressWarnings(as.numeric(category_row$amount[[1]]))
  per_unit <- trimws(category_row$per_unit[[1]])
  if (!nzchar(operator) || is.na(threshold) || !nzchar(per_unit)) return(NULL)

  if (!nrow(resolved_values)) return(NULL)
  month_values <- resolved_values %>% mutate(month = as.Date(month))

  compare_one <- function(value, label) {
    bad <- switch(operator,
      "<" = !(value < threshold),
      "<=" = !(value <= threshold),
      "=" = !(value == threshold),
      ">" = !(value > threshold),
      ">=" = !(value >= threshold),
      "!=" = !(value != threshold),
      FALSE
    )
    if (bad) {
      paste0("Category rule failed for ", label, ": expected ", operator, " ", format(threshold, nsmall = 2), " per ", per_unit)
    } else {
      NULL
    }
  }

  if (per_unit == "month") {
    for (i in seq_len(nrow(month_values))) {
      msg <- compare_one(month_values$value[[i]], format(month_values$month[[i]], "%Y-%m"))
      if (!is.null(msg)) return(msg)
    }
    return(NULL)
  }

  if (per_unit == "calendar year") {
    yearly <- month_values %>%
      mutate(calendar_year = year(month)) %>%
      group_by(calendar_year) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    for (i in seq_len(nrow(yearly))) {
      msg <- compare_one(yearly$value[[i]], paste0("calendar year ", yearly$calendar_year[[i]]))
      if (!is.null(msg)) return(msg)
    }
    return(NULL)
  }

  if (per_unit == "project year") {
    yearly <- month_values %>%
      mutate(project_year = interval(budget_start, month) %/% months(12) + 1L) %>%
      group_by(project_year) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    for (i in seq_len(nrow(yearly))) {
      msg <- compare_one(yearly$value[[i]], paste0("project year ", yearly$project_year[[i]]))
      if (!is.null(msg)) return(msg)
    }
  }

  NULL
}

make_empty_template_registry <- function() {
  tibble(
    id = integer(),
    name = character(),
    category = character(),
    center = character(),
    mode = character(),
    unit = character(),
    constant_expr = character(),
    function_expr = character(),
    fte = numeric(),
    note = character(),
    values = list(),
    duration_years = numeric(),
    is_default = logical(),
    is_deleted = logical()
  )
}

make_default_templates <- function() {
  tibble(
    id = 1L:3L,
    name = c("Custom", "Research Year", "PhD student"),
    category = c("", "Salary for research year (RY)", "Salary Ph.D. students"),
    center = rep("", 3),
    mode = c("function", "function", "function"),
    unit = c("month", "year", "month"),
    constant_expr = rep("0", 3),
    function_expr = c("rep(0, n)", "research_year$total_y", "apply_inflation_month(phd_student$total_m)"),
    fte = c(NA_real_, 1, 1),
    note = c("", "", ""),
    values = list(numeric(), numeric(), numeric()),
    duration_years = c(NA_real_, 3, NA_real_),
    is_default = rep(TRUE, 3),
    is_deleted = rep(FALSE, 3),
    application_status = rep("Applied for", 3)
  )
}

# === POST INTEGRITY HELPERS ===
find_posts_with_deleted_category <- function(posts_tbl, category_registry) {
  if (!nrow(posts_tbl) || !nrow(category_registry)) return(integer())
  deleted_cats <- category_registry %>%
    filter(is_deleted) %>%
    pull(name)
  posts_tbl %>%
    filter(category %in% deleted_cats) %>%
    pull(id)
}

find_posts_with_deleted_site <- function(posts_tbl, site_registry) {
  if (!nrow(posts_tbl) || !nrow(site_registry)) return(integer())
  deleted_sites <- site_registry %>%
    filter(is_deleted) %>%
    pull(name)
  posts_tbl %>%
    filter(center %in% deleted_sites) %>%
    pull(id)
}

get_post_amendment_fields <- function(post_row, budget_start, budget_end, category_registry, site_registry, post_import_issues = NULL) {
  fields <- character(0)

  if (post_row$start_date < budget_start || post_row$end_date > budget_end) fields <- c(fields, "post_date_range")

  bad_category <- category_registry %>%
    filter(name == post_row$category, is_deleted) %>%
    nrow() > 0
  if (bad_category) fields <- c(fields, "category")

  bad_site <- site_registry %>%
    filter(name == post_row$center, is_deleted) %>%
    nrow() > 0
  if (bad_site) fields <- c(fields, "center")

  # Check for formula errors in import issues
  if (!is.null(post_import_issues) && nrow(post_import_issues) > 0) {
    issue_row <- post_import_issues %>% filter(id == post_row$id)
    if (nrow(issue_row) > 0 && !is.na(issue_row$import_issues[1]) && nzchar(issue_row$import_issues[1])) {
      if (grepl("Formula error", issue_row$import_issues[1])) {
        fields <- c(fields, "function_expr")
      }
    }
  }

  unique(fields)
}

# === IMPORT VALIDATION HELPERS ===
check_import_post_issues <- function(post_row, existing_posts, salaries_lookup = data.frame(), inflation_pct = 0, salaries_tbl = NULL, site_registry = NULL, category_registry = NULL) {
  issues <- character(0)
  
  # Check for invalid application status
  valid_statuses <- c("Applied for", "Applied for elsewhere", "Not applied for", "Funded")
  if (!is.null(post_row$application_status) && nzchar(trimws(post_row$application_status))) {
    if (!(post_row$application_status %in% valid_statuses)) {
      issues <- c(issues, paste("Invalid application status:", post_row$application_status, "- must be one of:", paste(valid_statuses, collapse = ", ")))
    }
  }

  # Check for duplicate post names within same site
  dup <- existing_posts %>%
    filter(
      tolower(center) == tolower(post_row$center),
      tolower(post_name) == tolower(post_row$post_name),
      id != post_row$id
    )
  if (nrow(dup) > 0) {
    issues <- c(issues, "Duplicate post name within the same site")
  }

  # Check for invalid site
  if (!is.null(site_registry) && nrow(site_registry) > 0) {
    valid_sites <- site_registry %>% filter(!is_deleted) %>% pull(name)
    if (!post_row$center %in% valid_sites) {
      issues <- c(issues, paste("Site not in registry:", post_row$center))
    }
  }

  # Check for invalid category
  if (!is.null(category_registry) && nrow(category_registry) > 0) {
    valid_categories <- category_registry %>% filter(!is_deleted) %>% pull(name)
    if (!post_row$category %in% valid_categories) {
      issues <- c(issues, paste("Category not in registry:", post_row$category))
    }
  }

  # Check if formula evaluates without error
  if (post_row$value_mode == "function" && nzchar(trimws(post_row$function_expr))) {
    fte_monthly_total <- rep(NA_real_, 12)
    # Combine existing_posts with current post for context (excluding current post to avoid issues)
    all_posts_for_eval <- bind_rows(existing_posts, post_row)
    tryCatch({
      resolve_post_values(
        post_row,
        salaries_lookup = salaries_lookup,
        inflation_pct = inflation_pct,
        fte_monthly_total = fte_monthly_total,
        salaries_tbl = salaries_tbl,
        budget_start = post_row$start_date,
        all_posts_tbl = all_posts_for_eval
      )
    }, error = function(e) {
      issues <<- c(issues, paste("Formula error:", conditionMessage(e)))
    })
  }

  list(
    has_issues = length(issues) > 0,
    issues_text = paste(issues, collapse = "; ")
  )
}

resolve_post_values <- function(post_row, salaries_lookup = data.frame(), inflation_pct = 0, fte_monthly_total = NULL, salaries_tbl = NULL, budget_start = NULL, all_posts_tbl = NULL) {
  months <- month_sequence(post_row$start_date, post_row$end_date)
  n_months <- length(months)
  n_years <- ceiling(n_months / 12)

  mode <- post_row$value_mode
  unit <- post_row$value_unit
  fte <- post_row$fte
  current_site <- post_row$center
  inflation_rate <- as.numeric(inflation_pct) / 100
  if (is.na(inflation_rate)) inflation_rate <- 0
  # Use budget start year as the reference year (multiplier 1.0) so that posts
  # starting later than the budget start already carry accumulated inflation.
  base_calendar_year <- if (!is.null(budget_start)) year(as.Date(budget_start)) else year(months[1])
  post_start_year_offset <- year(months[1]) - base_calendar_year
  inflation_month_factors <- (1 + inflation_rate)^(year(months) - base_calendar_year)
  inflation_year_factors <- (1 + inflation_rate)^(seq_len(n_years) - 1 + post_start_year_offset)
  if (is.null(fte_monthly_total)) {
    fte_monthly_total <- rep(NA_real_, n_months)
  } else {
    fte_monthly_total <- rep(as.numeric(fte_monthly_total), length.out = n_months)
  }
  fte_yearly_total <- vapply(seq_len(n_years), function(i) {
    idx_start <- (i - 1) * 12 + 1
    idx_end <- min(i * 12, n_months)
    sum(fte_monthly_total[idx_start:idx_end], na.rm = TRUE)
  }, numeric(1))

  # Helper to compute FTE vectors for a given site or vector of sites
  get_site_fte <- function(sites) {
    if (is.null(all_posts_tbl) || !nrow(all_posts_tbl)) {
      return(rep(NA_real_, n_years))
    }
    
    sites <- as.character(sites)
    site_posts <- all_posts_tbl[all_posts_tbl$center %in% sites, ]
    
    monthly <- map_dbl(months, function(m) {
      sum(site_posts$fte[site_posts$start_date <= m & site_posts$end_date >= m], na.rm = TRUE) / 12
    })
    
    vapply(seq_len(n_years), function(i) {
      idx_start <- (i - 1) * 12 + 1
      idx_end <- min(i * 12, n_months)
      sum(monthly[idx_start:idx_end], na.rm = TRUE)
    }, numeric(1))
  }

  all_sites <- if (!is.null(all_posts_tbl) && nrow(all_posts_tbl) > 0) {
    unique(as.character(all_posts_tbl$center))
  } else {
    character(0)
  }

  # Helper to compute sum of amounts across posts in given site(s)
  # Issue #4: Generate salary objects for identifier$total_m access
  salary_objs <- list()
  if (!is.null(salaries_tbl) && nrow(salaries_tbl) > 0) {
    salary_objs <- make_salary_env_objects(salaries_tbl)
  }
  
  salary_ids <- rownames(salaries_lookup)
  extra_env <- list(
    inflation_pct = as.numeric(inflation_pct),
    this_site = current_site,
    all_sites = all_sites,
    fte = function(site = current_site) {
      get_site_fte(site)
    },
    inflation_factors = if (unit == "year") inflation_year_factors else inflation_month_factors,
    apply_inflation = function(base_value) {
      if (unit == "year") {
        x2 <- rep(as.numeric(base_value), length.out = n_years)
        x2 * inflation_year_factors
      } else {
        x2 <- rep(as.numeric(base_value), length.out = n_months)
        x2 * inflation_month_factors
      }
    }
  )
  
  # Inject salary objects for new syntax (issue #4)
  if (length(salary_objs) > 0) {
    for (sid in names(salary_objs)) {
      extra_env[[sid]] <- salary_objs[[sid]]
    }
  }

  if (mode == "constant") {
    constant_expr_safe <- if (is.null(post_row$constant_expr) || !nzchar(trimws(post_row$constant_expr))) "0" else post_row$constant_expr
    scalar <- safe_eval_expr(constant_expr_safe, fte = fte, n = n_months, extra_env = extra_env)
    if (length(scalar) != 1L || !is.numeric(scalar) || is.na(scalar)) {
      stop("Constant amount expression must resolve to one numeric value.", call. = FALSE)
    }
    values <- rep(as.numeric(scalar), if (unit == "month") n_months else n_years)
  } else if (mode == "function") {
    function_expr_safe <- if (is.null(post_row$function_expr) || !nzchar(trimws(post_row$function_expr))) "rep(0, n)" else post_row$function_expr
    n_values <- if (unit == "year") n_years else n_months
    values <- safe_eval_expr(function_expr_safe, fte = fte, n = n_values, extra_env = extra_env)
    if (!is.numeric(values)) {
      stop("Amount formula must resolve to numeric values.", call. = FALSE)
    }
  } else if (mode == "sum") {
    # Sum mode: use only the selected posts (sites and statuses are already baked into selection)
    current_post_id <- as.numeric(post_row$id[[1]])
    
    # Parse the sum fields (stored as "||"-separated strings)
    sum_posts_labels <- if (nzchar(trimws(as.character(post_row$sum_posts[[1]])))) {
      strsplit(as.character(post_row$sum_posts[[1]]), "||", fixed = TRUE)[[1]]
    } else {
      character(0)
    }
    
    # Filter posts: ONLY use the selected posts (sum_posts_labels)
    # Don't re-filter by sites/statuses since those are already in the selected posts list
    filtered_posts <- if (!is.null(all_posts_tbl) && nrow(all_posts_tbl) > 0 && length(sum_posts_labels) > 0) {
      # Create labels to match against
      all_posts_tbl %>%
        mutate(label = paste0(post_name, " (", center, ")")) %>%
        filter(
          label %in% sum_posts_labels,
          id != current_post_id,
          value_mode != "sum"
        ) %>%
        select(-label)
    } else {
      tibble()
    }
    
    # Sum the filtered posts
    values <- rep(0, n_months)
    
    for (i in seq_len(nrow(filtered_posts))) {
      other_post <- filtered_posts[i, ]
      tryCatch({
        resolved_tibble <- resolve_post_values(
          post_row = other_post,
          salaries_lookup = salaries_lookup,
          inflation_pct = inflation_pct,
          fte_monthly_total = fte_monthly_total,
          salaries_tbl = salaries_tbl,
          budget_start = budget_start,
          all_posts_tbl = all_posts_tbl
        )
        
        if (!is.null(resolved_tibble) && nrow(resolved_tibble) > 0) {
          other_amounts_monthly <- resolved_tibble$value
          other_months <- resolved_tibble$month
          
          # Align to calling post's date range
          for (j in seq_len(length(other_months))) {
            calling_idx <- which(months == other_months[j])
            if (length(calling_idx) > 0) {
              values[calling_idx[1]] <- values[calling_idx[1]] + other_amounts_monthly[j]
            }
          }
        }
      }, error = function(e) { NULL })
    }
    
    # Apply multiplier formula
    multiplier_expr <- if (nzchar(trimws(as.character(post_row$sum_multiplier[[1]])))) {
      as.character(post_row$sum_multiplier[[1]])
    } else {
      "1"
    }
    
    multiplier <- safe_eval_expr(multiplier_expr, fte = fte, n = n_months, extra_env = extra_env)
    if (length(multiplier) == 1) {
      multiplier <- rep(as.numeric(multiplier), n_months)
    }
    values <- values * multiplier
    
    # Now expand if needed (sum mode always returns monthly, but we need to match unit)
    if (unit == "year") {
      values <- expand_year_values(values, n_months)
    }
    
  } else {
    # Variable mode
    values <- unlist(post_row$value_vector, use.names = FALSE)
    if (!is.numeric(values)) {
      stop("Variable amounts must be numeric.", call. = FALSE)
    }
  }

  if (unit == "year") {
    if (length(values) == 1L) {
      values <- rep(values, n_years)
    }
    if (length(values) != n_years) {
      stop(
        "Year-based values must have length 1 or match required years (",
        n_years,
        ").",
        call. = FALSE
      )
    }
    values <- expand_year_values(values, n_months)
  } else {
    if (length(values) == 1L) {
      values <- rep(values, n_months)
    }
    if (length(values) != n_months) {
      stop(
        "Month-based values must have length 1 or match required months (",
        n_months,
        ").",
        call. = FALSE
      )
    }
  }

  if (any(is.na(values))) {
    stop("Resolved amounts contain missing values.", call. = FALSE)
  }

  tibble(
    month = months,
    value = as.numeric(values)
  )
}

flag_posts <- function(posts_tbl, budget_start, budget_end, preserve_existing = FALSE, post_import_issues = NULL) {
  posts_tbl %>%
    mutate(
      date_issue = start_date < budget_start | end_date > budget_end,
      # Check if this post has non-daterange import issues
      has_other_issues = if (!is.null(post_import_issues) && nrow(post_import_issues) > 0) {
        id %in% post_import_issues$id
      } else {
        FALSE
      },
      needs_amendment = if (preserve_existing) {
        # Always recalculate daterange issue, but preserve non-daterange issues
        has_other_issues | date_issue
      } else {
        # Full replacement (for initial import)
        date_issue
      }
    ) %>%
    select(-date_issue, -has_other_issues)
}

build_long_budget <- function(posts_tbl, budget_start, budget_end, salaries_lookup = data.frame(), inflation_pct = 0, salaries_tbl = NULL) {
  if (!nrow(posts_tbl)) {
    return(tibble())
  }

  month_fte_total_for <- function(months) {
    map_dbl(months, function(m) {
      sum(posts_tbl$fte[posts_tbl$start_date <= m & posts_tbl$end_date >= m], na.rm = TRUE) / 12
    })
  }

  map_dfr(seq_len(nrow(posts_tbl)), function(i) {
    row <- posts_tbl[i, ]
    row_months <- month_sequence(row$start_date, row$end_date)
    row_fte_monthly_total <- month_fte_total_for(row_months)

    resolved <- tryCatch(
      resolve_post_values(
        row,
        salaries_lookup = salaries_lookup,
        inflation_pct = inflation_pct,
        fte_monthly_total = row_fte_monthly_total,
        salaries_tbl = salaries_tbl,
        budget_start = budget_start,
        all_posts_tbl = posts_tbl
      ),
      error = function(e) NULL
    )

    # Create base tibble (either from resolved values or with NAs for errors)
    if (is.null(resolved)) {
      base_result <- tibble(
        month = row_months,
        value = NA_real_
      )
    } else {
      base_result <- resolved
    }
    
    # Add post metadata and calculations to all rows
    base_result %>%
      mutate(
        id = row$id,
        center = row$center,
        post_name = row$post_name,
        category = row$category,
        fte = row$fte
      ) %>%
      filter(month >= budget_start, month <= budget_end) %>%
      mutate(
        calendar_year = year(month),
        project_year = interval(budget_start, month) %/% months(12) + 1L,
        period_month = format(month, "%Y-%m")
      )
  })
}

post_total <- function(post_row, all_posts_tbl = post_row, salaries_lookup = data.frame(), inflation_pct = 0, salaries_tbl = NULL, budget_start = NULL) {
  row_months <- month_sequence(post_row$start_date, post_row$end_date)
  row_fte_monthly_total <- map_dbl(row_months, function(m) {
    sum(all_posts_tbl$fte[all_posts_tbl$start_date <= m & all_posts_tbl$end_date >= m], na.rm = TRUE) / 12
  })

  resolved <- resolve_post_values(
    post_row,
    salaries_lookup = salaries_lookup,
    inflation_pct = inflation_pct,
    fte_monthly_total = row_fte_monthly_total,
    salaries_tbl = salaries_tbl,
    budget_start = budget_start,
    all_posts_tbl = all_posts_tbl
  )
  sum(resolved$value)
}

serialize_posts <- function(posts_tbl) {
  posts_tbl %>%
    mutate(
      start_date = as.character(start_date),
      end_date = as.character(end_date),
      value_vector = map_chr(value_vector, ~ paste(.x, collapse = ";"))
    )
}

serialize_inactive_posts <- function(posts_tbl) {
  if (!nrow(posts_tbl)) return(tibble())
  
  posts_tbl %>%
    select(id, center, post_name, category, start_date, end_date, fte, value_mode, value_unit, 
           constant_expr, function_expr, value_vector, note, needs_amendment, application_status, import_issues) %>%
    mutate(
      start_date = as.character(start_date),
      end_date = as.character(end_date),
      value_vector = map_chr(value_vector, ~ paste(.x, collapse = ";")),
      import_issues = if_else(is.na(import_issues), "", import_issues)
    )
}

parse_inactive_posts <- function(posts_tbl) {
  if (is.null(posts_tbl) || !nrow(posts_tbl)) {
    return(make_empty_posts() %>% mutate(import_issues = character()))
  }

  required <- c(
    "id", "center", "post_name", "category", "start_date", "end_date",
    "fte", "value_mode", "value_unit", "constant_expr", "function_expr",
    "value_vector", "note", "needs_amendment", "application_status", "import_issues"
  )

  if (!all(required %in% names(posts_tbl))) {
    missing_cols <- setdiff(required, names(posts_tbl))
    for (col in missing_cols) {
      posts_tbl[[col]] <- switch(col,
        import_issues = NA_character_,
        NA
      )
    }
  }

  posts_tbl %>%
    mutate(
      start_date = as.Date(start_date),
      end_date = as.Date(end_date),
      fte = as.numeric(fte),
      id = as.integer(id),
      needs_amendment = as.logical(needs_amendment),
      value_vector = strsplit(ifelse(is.na(value_vector), "", value_vector), ";", fixed = TRUE),
      value_vector = map(value_vector, ~ as.numeric(.x[nzchar(.x)])),
      application_status = {
        v <- as.character(application_status)
        ifelse(is.na(v) | !nzchar(v), "Applied for", v)
      },
      import_issues = if_else(is.na(import_issues) | import_issues == "", NA_character_, import_issues)
    )
}

parse_posts <- function(posts_tbl) {
  if (is.null(posts_tbl) || !nrow(posts_tbl)) {
    return(make_empty_posts())
  }

  required <- c(
    "id", "center", "post_name", "category", "start_date", "end_date",
    "fte", "value_mode", "value_unit", "constant_expr", "function_expr",
    "value_vector", "note", "needs_amendment", "application_status"
  )

  if (!all(required %in% names(posts_tbl))) {
    missing_cols <- setdiff(required, names(posts_tbl))
    for (col in missing_cols) {
      posts_tbl[[col]] <- switch(col,
        id = NA_integer_,
        center = "",
        post_name = "",
        category = "",
        start_date = as.character(NA),
        end_date = as.character(NA),
        fte = NA_real_,
        value_mode = "function",
        value_unit = "month",
        constant_expr = "0",
        function_expr = "rep(0, n)",
        value_vector = "",
        note = "",
        needs_amendment = FALSE,
        application_status = "Applied for",
        ""
      )
    }
  }

  posts_tbl %>%
    mutate(
      start_date = as.Date(start_date),
      end_date = as.Date(end_date),
      fte = as.numeric(fte),
      id = as.integer(id),
      needs_amendment = as.logical(needs_amendment),
      value_vector = strsplit(ifelse(is.na(value_vector), "", value_vector), ";", fixed = TRUE),
      value_vector = map(value_vector, ~ as.numeric(.x[nzchar(.x)])),
      application_status = {
        v <- as.character(application_status)
        ifelse(is.na(v) | !nzchar(v), "Applied for", v)
      }
    )
}

serialize_salaries <- function(salaries_tbl) {
  salaries_tbl
}

serialize_categories <- function(categories_tbl) {
  categories_tbl %>%
    filter(!(is_deleted & !is_default)) %>%
    mutate(
      id = as.integer(id),
      name = as.character(name),
      operator = as.character(operator),
      amount = as.numeric(amount),
      per_unit = as.character(per_unit),
      is_default = as.logical(is_default),
      is_locked = as.logical(is_locked),
      is_deleted = as.logical(is_deleted)
    )
}

serialize_sites <- function(sites_tbl) {
  sites_tbl %>%
    filter(!(is_deleted & !is_default)) %>%
    mutate(
      id = as.integer(id),
      name = as.character(name),
      is_default = as.logical(is_default),
      is_locked = as.logical(is_locked),
      is_deleted = as.logical(is_deleted)
    )
}

parse_sites <- function(sites_tbl) {
  required <- c("id", "name", "is_default", "is_deleted")
  if (is.null(sites_tbl) || !nrow(sites_tbl) || !all(required %in% names(sites_tbl))) {
    return(make_default_sites())
  }

  parsed <- sites_tbl
  if (!"is_locked" %in% names(parsed)) {
    parsed$is_locked <- FALSE
  }
  parsed <- parsed %>%
    mutate(
      id = suppressWarnings(as.integer(id)),
      name = as.character(name),
      is_default = as.logical(is_default),
      is_locked = as.logical(is_locked),
      is_deleted = as.logical(is_deleted)
    )

  if (!any(parsed$is_locked & !parsed$is_deleted, na.rm = TRUE)) {
    parsed <- bind_rows(make_default_sites(), parsed)
  }
  parsed
}

parse_categories <- function(categories_tbl) {
  required <- c("id", "name", "operator", "amount", "per_unit", "is_default", "is_deleted")
  if (is.null(categories_tbl) || !nrow(categories_tbl) || !all(required %in% names(categories_tbl))) {
    return(make_default_categories())
  }

  parsed <- categories_tbl
  if (!"is_locked" %in% names(parsed)) {
    parsed$is_locked <- FALSE
  }
  parsed <- parsed %>%
    mutate(
      id = suppressWarnings(as.integer(id)),
      name = as.character(name),
      operator = as.character(operator),
      amount = suppressWarnings(as.numeric(amount)),
      per_unit = as.character(per_unit),
      is_default = as.logical(is_default),
      is_locked = as.logical(is_locked),
      is_deleted = as.logical(is_deleted)
    )

  if (!any(parsed$name == "Uncategorized")) {
    parsed <- bind_rows(make_default_categories() %>% slice(1), parsed)
  }
  parsed
}

serialize_templates <- function(templates_tbl) {
  templates_tbl %>%
    filter(!(is_deleted & !is_default)) %>%
    mutate(
      id = as.integer(id),
      name = as.character(name),
      category = as.character(category),
      center = as.character(center),
      mode = as.character(mode),
      unit = as.character(unit),
      constant_expr = as.character(constant_expr),
      function_expr = as.character(function_expr),
      fte = as.numeric(fte),
      note = as.character(note),
      values = map_chr(values, ~ paste(as.numeric(.x), collapse = ";")),
      duration_years = as.numeric(duration_years),
      is_default = as.logical(is_default),
      is_deleted = as.logical(is_deleted)
    )
}

parse_templates <- function(templates_tbl) {
  required <- c("id", "name", "category", "center", "mode", "unit", "constant_expr", "function_expr", "fte", "note", "values", "duration_years", "is_default", "is_deleted", "application_status")
  if (is.null(templates_tbl) || !nrow(templates_tbl)) {
    return(make_default_templates())
  }

  if (!all(required %in% names(templates_tbl))) {
    missing_cols <- setdiff(required, names(templates_tbl))
    for (col in missing_cols) {
      templates_tbl[[col]] <- switch(col,
        id = NA_integer_,
        name = "",
        category = "",
        center = "",
        mode = "function",
        unit = "month",
        constant_expr = "0",
        function_expr = "rep(0, n)",
        fte = NA_real_,
        note = "",
        values = "",
        duration_years = NA_real_,
        is_default = FALSE,
        is_deleted = FALSE,
        application_status = "Applied for",
        ""
      )
    }
  }
  if (!"is_locked" %in% names(templates_tbl)) {
    templates_tbl$is_locked <- FALSE
  }

  templates_tbl %>%
    mutate(
      id = suppressWarnings(as.integer(id)),
      name = as.character(name),
      category = as.character(category),
      center = as.character(center),
      mode = as.character(mode),
      unit = as.character(unit),
      constant_expr = as.character(constant_expr),
      function_expr = as.character(function_expr),
      fte = suppressWarnings(as.numeric(fte)),
      note = as.character(note),
      values = strsplit(ifelse(is.na(values), "", values), ";", fixed = TRUE),
      values = map(values, ~ as.numeric(.x[nzchar(.x)])),
      duration_years = suppressWarnings(as.numeric(duration_years)),
      is_default = as.logical(is_default),
      is_locked = as.logical(is_locked),
      is_deleted = as.logical(is_deleted),
      application_status = {
        v <- as.character(application_status)
        ifelse(is.na(v) | !nzchar(v), "Applied for", v)
      }
    )
}

parse_salaries <- function(salaries_tbl) {
  if (is.null(salaries_tbl) || !nrow(salaries_tbl)) {
    return(make_empty_salaries())
  }

  required_inputs <- c("id", "identifier", "name", "unit", "base_salary", "pension_mode", "pension_value", "own_pension_pct")
  if (!all(required_inputs %in% names(salaries_tbl))) {
    return(make_empty_salaries())
  }

  # Backward compat: add new input fields if missing
  if (!"wage_supplement" %in% names(salaries_tbl)) salaries_tbl$wage_supplement <- 0
  if (!"holiday_rate" %in% names(salaries_tbl)) salaries_tbl$holiday_rate <- 12.5
  if (!"subtract_holiday" %in% names(salaries_tbl)) salaries_tbl$subtract_holiday <- TRUE

  salaries_tbl <- salaries_tbl %>%
    mutate(
      id = as.integer(id),
      identifier = as.character(identifier),
      name = as.character(name),
      unit = as.character(unit),
      pension_mode = as.character(pension_mode),
      base_salary = as.numeric(base_salary),
      pension_value = as.numeric(pension_value),
      own_pension_pct = as.numeric(own_pension_pct),
      wage_supplement = as.numeric(wage_supplement),
      holiday_rate = as.numeric(holiday_rate),
      subtract_holiday = as.logical(subtract_holiday)
    )

  # Recalculate all derived fields from input params
  map_dfr(seq_len(nrow(salaries_tbl)), function(i) {
    row <- salaries_tbl[i, ]
    calc <- tryCatch(
      calc_salary_fields(row$base_salary, row$unit, row$pension_mode, row$pension_value,
                         row$own_pension_pct, row$wage_supplement, row$holiday_rate, row$subtract_holiday),
      error = function(e) NULL
    )
    na_num <- NA_real_
    tibble(
      id = row$id, identifier = row$identifier, name = row$name, unit = row$unit,
      base_salary = row$base_salary, pension_mode = row$pension_mode,
      pension_value = row$pension_value, own_pension_pct = row$own_pension_pct,
      wage_supplement = row$wage_supplement, holiday_rate = row$holiday_rate,
      subtract_holiday = row$subtract_holiday,
      base_salary_monthly               = if (is.null(calc)) na_num else calc$base_salary_monthly,
      wage_supplement_monthly           = if (is.null(calc)) na_num else calc$wage_supplement_monthly,
      pension_amount_monthly            = if (is.null(calc)) na_num else calc$pension_amount_monthly,
      own_pension_amount_monthly        = if (is.null(calc)) na_num else calc$own_pension_amount_monthly,
      holiday_allowance_total_monthly   = if (is.null(calc)) na_num else calc$holiday_allowance_total_monthly,
      holiday_allowance_monthly         = if (is.null(calc)) na_num else calc$holiday_allowance_monthly,
      total_plus_holiday_salary_monthly = if (is.null(calc)) na_num else calc$total_plus_holiday_salary_monthly,
      total_salary_monthly              = if (is.null(calc)) na_num else calc$total_salary_monthly,
      base_salary_yearly                = if (is.null(calc)) na_num else calc$base_salary_yearly,
      wage_supplement_yearly            = if (is.null(calc)) na_num else calc$wage_supplement_yearly,
      pension_amount_yearly             = if (is.null(calc)) na_num else calc$pension_amount_yearly,
      own_pension_amount_yearly         = if (is.null(calc)) na_num else calc$own_pension_amount_yearly,
      holiday_allowance_total_yearly    = if (is.null(calc)) na_num else calc$holiday_allowance_total_yearly,
      holiday_allowance_yearly          = if (is.null(calc)) na_num else calc$holiday_allowance_yearly,
      total_plus_holiday_salary_yearly  = if (is.null(calc)) na_num else calc$total_plus_holiday_salary_yearly,
      total_salary_yearly               = if (is.null(calc)) na_num else calc$total_salary_yearly
    )
  })
}

append_total_row <- function(df, amount_col = "amount") {
  if (!nrow(df)) {
    return(df)
  }

  total_row <- df[1, , drop = FALSE]
  total_row[1, ] <- NA
  if ("period" %in% names(total_row)) total_row$period <- "TOTAL"

  for (nm in names(total_row)) {
    if (is.character(total_row[[nm]]) && nm != "period") {
      total_row[[nm]] <- "All"
    }
  }

  total_row[[amount_col]] <- sum(df[[amount_col]], na.rm = TRUE)
  bind_rows(df, total_row)
}

amend_label <- function(text, show_amend = FALSE, required = FALSE) {
  tagList(
    if (required) tags$span(style = "color:#b00020;font-weight:400;", "*"),
    text,
    tags$span(
      style = if (show_amend) "color:#b00020;font-weight:700;font-size:large;" else "display:none;visibility:hidden;",
      " !!!"
    )
  )
}

required_label <- function(text) {
  amend_label(text, show_amend = FALSE, required = TRUE)
}

ui <- fluidPage(
  tags$head(tags$style(HTML(
    "
    #header-row input, #header-row .btn {
      height:38px;
      margin:0 0 5px 0;
    },
    input.form-control, div.selectize-input, container-fluid {
      font-size: 12px !important;
    }
    label.control-label {
      font-size: 13px !important;
    }
    .btn {
      padding: 0.8rem 1.2rem !important;
      margin: 0 0 10px 0;
      font-size: 13px !important;
    }
    .inline-error {
      color: #b00020;
      font-weight: 700;
      margin: 6px 0;
    }
    .flash-msg {
      overflow: hidden;
      border-radius: 6px;
      padding: 10px;
      margin: 8px 0;
      font-weight: 600;
      opacity: 0;
      max-height: 0;
      transform: translateY(-4px);
      animation: msgEnter 0.22s ease-out forwards, msgExit 0.35s ease-in 3.65s forwards;
    }
    .flash-msg.flash-error {
      background: #fdecea;
      border: 1px solid #f5c2c7;
      color: #842029;
    }
    .flash-msg.flash-success {
      background: #e7f8ec;
      border: 1px solid #95d5a6;
      color: #1d6f33;
    }
    #form_error:empty,
    #success_feedback:empty,
    #export_error:empty {
      height: 0 !important;
      margin: 0 !important;
      padding: 0 !important;
      min-height: 0 !important;
      line-height: 0 !important;
      font-size: 0 !important;
      border: 0 !important;
      overflow: hidden !important;
    }
    @keyframes msgEnter {
      from {
        opacity: 0;
        max-height: 0;
        transform: translateY(-4px);
        margin-top: 0;
        margin-bottom: 0;
        padding: 0 10px;
      }
      to {
        opacity: 1;
        max-height: 140px;
        transform: translateY(0);
        margin-top: 8px;
        margin-bottom: 8px;
        padding: 10px;
      }
    }
    @keyframes msgExit {
      from {
        opacity: 1;
        max-height: 140px;
        transform: translateY(0);
        margin-top: 8px;
        margin-bottom: 8px;
        padding: 10px;
      }
      to {
        opacity: 0;
        max-height: 0;
        transform: translateY(-4px);
        margin-top: 0;
        margin-bottom: 0;
        padding: 0 10px;
      }
    }
    .dataTables_wrapper .dataTables_filter {
      display: none;
    }
    table.dataTable td, table.dataTable th {
      font-size: 12px !important;
    }
    table.dataTable tbody td.wide-col-selected {
      background-color: rgba(176, 190, 217, 0.55) !important;
    }
    .wide-header-site {
      text-align: center;
    }
    .wide-header-spacer {
      min-width: 16px;
      width: 16px;
      border: none;
      background: #fff;
    }
    .wide-header-total {
      font-weight: 700;
    }

    h4 {
      font-size: 20px;
      text-align: center;
      padding: 15px 0 10px;
      font-weight: 500;
    }
    .settings-tab .btn {
      width: 100% !important;
    }
    #export_xlsx.btn {
      width: 100% !important;
    }
    .import-file-wrap .form-group,
    .import-file-wrap .input-group,
    .import-file-wrap .progress {
      margin-bottom: 0 !important;
    }
    .shiny-input-container .input-group .form-control,
    .shiny-input-container .input-group .input-group-btn .btn {
      height: 38px !important;
    }
    .nav-tabs > li {
      width: 33.33%;
      text-align: center;
    }
    .nav-tabs > li > a {
      text-align: center;
    }
    .tab-content {
      margin-top: 30px;
    }
    .amend-badge {
      color: #b00020;
      font-weight: 700;
      margin-left: 4px;
    }
    @media (min-width: 768px) {
      .sidebar-split-row {
        margin-left: -15px;
        margin-right: -15px;
      }
      .sidebar-split-left {
        padding: 0 5px 0 15px;
      }
      .sidebar-split-right {
        padding: 0 15px 0 5px;
      }
      .import-side-border {
        border-left: 1px solid #ddd;
        border-right: 1px solid #ddd;
      }
      .period-side-border {
        border-right: 1px solid #ddd;
      }
    }
    "
  ))),
  tags$head(tags$script(src = "https://cdn.jsdelivr.net/npm/xlsx-js-style@1.2.0/dist/xlsx.bundle.js")),
  tags$head(tags$script(src = "https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js")),
  tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jspdf/2.5.1/jspdf.umd.min.js")),
  tags$head(tags$script(HTML(
    "
    Shiny.addCustomMessageHandler('download-xlsx', function(payload) {
      if (typeof XLSX === 'undefined') {
        console.error('SheetJS (XLSX) failed to load.');
        return;
      }

      function writeWorkbookBlob(workbook) {
        try {
          var workbookBytes = XLSX.write(workbook, {
            bookType: 'xlsx',
            bookSST: false,
            type: 'array'
          });
          return new Blob([
            workbookBytes
          ], {
            type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
          });
        } catch (arrayError) {
          var binary = XLSX.write(workbook, {
            bookType: 'xlsx',
            bookSST: false,
            type: 'binary'
          });
          var buffer = new ArrayBuffer(binary.length);
          var view = new Uint8Array(buffer);
          for (var i = 0; i < binary.length; i++) {
            view[i] = binary.charCodeAt(i) & 0xFF;
          }
          return new Blob([
            buffer
          ], {
            type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
          });
        }
      }

      function downloadWorkbook(workbook, filename) {
        var safeName = filename || 'Budget.xlsx';

        try {
          if (typeof XLSX.writeFile === 'function') {
            XLSX.writeFile(workbook, safeName, { bookType: 'xlsx', bookSST: false });
            return;
          }
        } catch (writeFileError) {
          console.warn('XLSX.writeFile failed, falling back to Blob download.', writeFileError);
        }

        var blob = writeWorkbookBlob(workbook);
        var link = document.createElement('a');
        link.href = URL.createObjectURL(blob);
        link.download = safeName;
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
        setTimeout(function() {
          URL.revokeObjectURL(link.href);
        }, 1000);
      }

      var wb = XLSX.utils.book_new();

      Object.keys(payload.sheets).forEach(function(sheetName) {
        var sheetInfo = payload.sheets[sheetName];
        var rows = Array.isArray(sheetInfo) ? sheetInfo : (sheetInfo.rows || []);
        var rawCols = Array.isArray(sheetInfo) ? [] : (sheetInfo.columns || []);
        // Guard: Shiny may serialise a length-1 R character vector as a scalar
        // string instead of a one-element array.
        var columns = Array.isArray(rawCols) ? rawCols : (rawCols ? [rawCols] : []);
        var meta = Array.isArray(sheetInfo) ? {} : (sheetInfo.meta || {});

        var ws;
        var colNames;

        if (columns.length > 0) {
          var aoa = [columns].concat(rows.map(function(row) {
            return columns.map(function(_, idx) {
              return Array.isArray(row) ? row[idx] : null;
            });
          }));
          ws = XLSX.utils.aoa_to_sheet(aoa);
          colNames = columns.slice();
        } else {
          ws = XLSX.utils.json_to_sheet(rows);
          colNames = rows.length > 0 ? Object.keys(rows[0]) : [];
        }

        if (rows.length > 0) {
          var range = XLSX.utils.decode_range(ws['!ref']);

          // Collect total column indices (0-based)
          var totalColIdx = [];
          colNames.forEach(function(name, idx) {
            if (/total/i.test(name)) totalColIdx.push(idx);
          });

          // Total row: last data row when meta.hasTotalRow is true
          var totalRowSheetIdx = (meta.hasTotalRow && rows.length > 0)
            ? range.e.r  // last row in sheet = header + data rows - 1
            : -1;

          for (var R = range.s.r; R <= range.e.r; R++) {
            for (var C = range.s.c; C <= range.e.c; C++) {
              var addr = XLSX.utils.encode_cell({r: R, c: C});
              if (!ws[addr]) ws[addr] = {v: '', t: 's'};

              var isHeader   = (R === range.s.r);
              var isTotalRow = (R === totalRowSheetIdx);
              var isTotalCol = totalColIdx.indexOf(C) !== -1;
              var bold = isTotalRow || (isHeader && isTotalCol) || isTotalCol;

              if (bold || isTotalRow) {
                ws[addr].s = {
                  font: { bold: bold },
                  border: isTotalRow ? {
                    top:    { style: 'thin',   color: { rgb: 'FF000000' } },
                    bottom: { style: 'double', color: { rgb: 'FF000000' } }
                  } : {}
                };
              }
            }
          }

          // Merged header spans
          if (meta.merges && meta.merges.length > 0) {
            ws['!merges'] = meta.merges;
          }
        }

        XLSX.utils.book_append_sheet(wb, ws, sheetName);
      });

      try {
        downloadWorkbook(wb, payload.filename || 'Budget.xlsx');
      } catch (downloadError) {
        console.error('Workbook export failed.', downloadError);
      }
    });
    "
  ))),
  tags$head(tags$script(HTML(
    "
    document.addEventListener('input', function(evt) {
      if (evt.target && evt.target.id === 'workbook_name' && window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('workbook_name_live', evt.target.value, {priority: 'event'});
      }
    });

    
    "
  ))),
  tags$head(tags$script(HTML(
    "
    Shiny.addCustomMessageHandler('save-pdf', function(payload) {
      if (typeof window.jspdf === 'undefined') { console.error('jsPDF not loaded'); return; }

      var vizEl = document.getElementById('visualization_plot');
      var dtWrapper = document.querySelector('#wide_form_table .dataTables_wrapper');
      if (!dtWrapper || !vizEl) { console.warn('Elements not found'); return; }

      var headTable = dtWrapper.querySelector('.dataTables_scrollHead table.dataTable');
      var bodyTable = dtWrapper.querySelector('.dataTables_scrollBody table.dataTable');
      if (!headTable) headTable = dtWrapper.querySelector('table.dataTable');
      if (!bodyTable) bodyTable = headTable;

      var firstBodyRow = bodyTable ? bodyTable.querySelector('tbody tr') : null;
      if (!firstBodyRow) { console.warn('No body rows'); return; }

      var colWidths = Array.from(firstBodyRow.cells).map(function(c) {
        return Math.ceil(c.getBoundingClientRect().width) || 60;
      });
      var nCols = colWidths.length;
      var totalW = colWidths.reduce(function(a, b) { return a + b; }, 0);
      var colX = [];
      var cx = 0;
      for (var i = 0; i < nCols; i++) { colX.push(cx); cx += colWidths[i]; }

      var FONT_SZ = 10;
      var PAD = 5;
      var LINE_H = FONT_SZ + 4;
      var HEAD_PAD_V = 6;
      var BODY_ROW_H = 22;

      // Collect header rows
      var headRows = [];
      var theadEl = headTable ? headTable.querySelector('thead') : null;
      if (theadEl) {
        Array.from(theadEl.rows).forEach(function(tr) {
          headRows.push(Array.from(tr.cells).map(function(th) {
            return { text: (th.innerText || th.textContent || '').trim(), cs: th.colSpan || 1, rs: th.rowSpan || 1 };
          }));
        });
      }
      var nHeadRows = headRows.length;

      // Collect body rows
      var bodyRows = [];
      var tbodyEl = bodyTable ? bodyTable.querySelector('tbody') : null;
      if (tbodyEl) {
        Array.from(tbodyEl.rows).forEach(function(tr) {
          var rBg = window.getComputedStyle(tr).backgroundColor;
          bodyRows.push(Array.from(tr.cells).map(function(td) {
            var cs = window.getComputedStyle(td);
            var bg = cs.backgroundColor;
            if (bg === 'rgba(0, 0, 0, 0)' || bg === 'transparent') bg = rBg;
            if (bg === 'rgba(0, 0, 0, 0)' || bg === 'transparent') bg = 'rgb(255,255,255)';
            return {
              text: (td.innerText || td.textContent || '').trim(),
              cs: td.colSpan || 1,
              bg: bg,
              bold: (parseInt(cs.fontWeight, 10) || 400) >= 600,
              italic: cs.fontStyle === 'italic',
              color: cs.color || 'rgb(0,0,0)',
              align: cs.textAlign || 'left'
            };
          }));
        });
      }
      var nBodyRows = bodyRows.length;

      // Use a scratch canvas for text measurement
      var measCanvas = document.createElement('canvas');
      var measCtx = measCanvas.getContext('2d');

      function measureLines(text, maxW, fontStr) {
        measCtx.font = fontStr;
        if (!text) return [''];
        var words = text.split(' ');
        var lines = [];
        var line = '';
        for (var n = 0; n < words.length; n++) {
          var test = line ? line + ' ' + words[n] : words[n];
          if (measCtx.measureText(test).width > maxW && line) {
            lines.push(line);
            line = words[n];
          } else {
            line = test;
          }
        }
        if (line) lines.push(line);
        return lines.length ? lines : [''];
      }

      // Pre-compute header row heights based on wrapped text
      var headFontStr = 'bold ' + FONT_SZ + 'px Arial,sans-serif';
      var rsUntilPre = [];
      for (var ip = 0; ip < nCols; ip++) rsUntilPre.push(-1);

      var headRowHeights = headRows.map(function(row, ri) {
        var maxLines = 1;
        var col = 0;
        row.forEach(function(cell) {
          while (col < nCols && rsUntilPre[col] >= ri) col++;
          if (col >= nCols) return;
          var span = Math.min(cell.cs || 1, nCols - col);
          var cellW = 0;
          for (var k = 0; k < span; k++) { if (col + k < nCols) cellW += colWidths[col + k]; }
          var lines = measureLines(cell.text, cellW - 2 * PAD, headFontStr);
          maxLines = Math.max(maxLines, lines.length);
          for (var k2 = 0; k2 < span; k2++) { if (col + k2 < nCols) rsUntilPre[col + k2] = ri + (cell.rs || 1) - 1; }
          col += span;
        });
        return maxLines * LINE_H + 2 * HEAD_PAD_V;
      });

      // Compute cumulative Y for header rows
      var headRowY = [];
      var cumY = 0;
      headRowHeights.forEach(function(rh) { headRowY.push(cumY); cumY += rh; });
      var totalHeadH = cumY;
      var totalH = totalHeadH + nBodyRows * BODY_ROW_H;

      var SCALE = 2;
      var tblCanvas = document.createElement('canvas');
      tblCanvas.width = totalW * SCALE;
      tblCanvas.height = totalH * SCALE;
      var ctx = tblCanvas.getContext('2d');
      ctx.scale(SCALE, SCALE);
      ctx.fillStyle = '#ffffff';
      ctx.fillRect(0, 0, totalW, totalH);

      function drawCell(x, w, y, h, text, opts) {
        ctx.fillStyle = opts.bg || '#ffffff';
        ctx.fillRect(x, y, w, h);
        if (opts.borderBottom) {
          ctx.strokeStyle = opts.borderBottom === 'thick' ? '#888888' : '#bbbbbb';
          ctx.lineWidth = opts.borderBottom === 'thick' ? 1.5 : 0.5;
          ctx.beginPath(); ctx.moveTo(x, y + h); ctx.lineTo(x + w, y + h); ctx.stroke();
        }
        var fontStr = (opts.italic ? 'italic ' : '') + (opts.bold ? 'bold ' : '') + FONT_SZ + 'px Arial,sans-serif';
        ctx.font = fontStr;
        ctx.fillStyle = opts.color || '#000000';
        var al = opts.align || 'left';
        ctx.textAlign = al === 'right' ? 'right' : (al === 'center' ? 'center' : 'left');
        ctx.textBaseline = 'middle';
        var tx = al === 'right' ? x + w - PAD : (al === 'center' ? x + w / 2 : x + PAD);
        ctx.save();
        ctx.beginPath(); ctx.rect(x, y, w, h); ctx.clip();
        if (opts.wrap) {
          var lines = measureLines(text, w - 2 * PAD, fontStr);
          var totalTH = lines.length * LINE_H;
          var startY = y + (h - totalTH) / 2 + LINE_H / 2;
          lines.forEach(function(l, li) { ctx.fillText(l, tx, startY + li * LINE_H); });
        } else {
          ctx.fillText(text, tx, y + h / 2);
        }
        ctx.restore();
      }

      // Draw header rows
      var rsUntil = [];
      for (var id = 0; id < nCols; id++) rsUntil.push(-1);

      headRows.forEach(function(row, ri) {
        var isLast = ri === nHeadRows - 1;
        var col = 0;
        row.forEach(function(cell) {
          while (col < nCols && rsUntil[col] >= ri) col++;
          if (col >= nCols) return;
          var span = Math.min(cell.cs || 1, nCols - col);
          var rspan = cell.rs || 1;
          var cw2 = 0;
          for (var k = 0; k < span; k++) { if (col + k < nCols) cw2 += colWidths[col + k]; }
          var cellH = 0;
          for (var rr = 0; rr < rspan && ri + rr < nHeadRows; rr++) cellH += headRowHeights[ri + rr];
          var isSpanning = !isLast && span > 1;
          drawCell(colX[col], cw2, headRowY[ri], cellH, cell.text, {
            bg: '#f0f0f0', bold: true, align: 'center', wrap: true,
            borderBottom: isSpanning ? 'thin' : (isLast ? 'thick' : null)
          });
          for (var k2 = 0; k2 < span; k2++) { if (col + k2 < nCols) rsUntil[col + k2] = ri + rspan - 1; }
          col += span;
        });
      });

      // Draw body rows
      var yy = totalHeadH;
      bodyRows.forEach(function(row) {
        var col = 0;
        row.forEach(function(cell) {
          var span = Math.min(cell.cs || 1, nCols - col);
          var cw2 = 0;
          for (var k = 0; k < span; k++) { if (col + k < nCols) cw2 += colWidths[col + k]; }
          drawCell(colX[col], cw2, yy, BODY_ROW_H, cell.text, {
            bg: cell.bg, bold: cell.bold, italic: cell.italic, color: cell.color, align: cell.align
          });
          col += span;
        });
        yy += BODY_ROW_H;
      });

      // Capture visualization via Plotly.toImage for full-width output at any resolution
      var vizW = 1200;
      var vizH = 700;

      Plotly.toImage(vizEl, { format: 'png', width: vizW, height: vizH }).then(function(vizDataUrl) {
        var jsPDFCtor = window.jspdf.jsPDF;
        var doc = new jsPDFCtor({ orientation: 'landscape', unit: 'mm', format: 'a3' });
        var pageW = doc.internal.pageSize.getWidth();
        var pageH = doc.internal.pageSize.getHeight();
        var margin = 10;
        var cW = pageW - 2 * margin;
        var cH = pageH - 2 * margin;

        var tA = tblCanvas.height / tblCanvas.width;
        var tW = cW; var tH = tW * tA;
        if (tH > cH) { tH = cH; tW = tH / tA; }
        doc.addImage(tblCanvas.toDataURL('image/png'), 'PNG', margin + (cW - tW) / 2, margin, tW, tH);

        doc.addPage();
        var vRatio = vizH / vizW;
        var vW = cW; var vH = vW * vRatio;
        if (vH > cH) { vH = cH; vW = vH / vRatio; }
        doc.addImage(vizDataUrl, 'PNG', margin + (cW - vW) / 2, margin, vW, vH);

        doc.save(payload.filename || 'budget.pdf');
      });
    });
    "
  ))),
  # Issue #5: Restructured header layout
  # Row 1: Workbook name | Import | Export
  fluidRow(
    style = "margin: 0; padding: 15px 0; width: 100%;", id = "header-row",
    column(4,
      tags$label(strong("Workbook name")),
      textInput("workbook_name", NULL, value = "Budget", width = "100%"),
      uiOutput("workbook_name_error")
    ),
    column(4, class = "import-side-border",
      tags$label(strong("Import workbook")),
      tags$p(" ...to continue where you left off", style = "font-size: 0.85em; color: #666; margin: 4px 0; text-align: left;display: inline;"),
      tags$div(
        class = "import-file-wrap",
        fileInput("import_file", NULL, accept = ".xlsx", width = "100%")
      )
    ),
    column(4,
      tags$label(strong("Export workbook")),
      tags$div(style = "width: 100%;",
        uiOutput("export_control")
      ),
      uiOutput("export_error")
    )
  ),
  # Row 2: Global values (dates + inflation)
  fluidRow(
    style = "margin: 0 0 30px 0; padding: 15px 0; width: 100%; background-color: #f8f9fa;",
    column(6, class = "period-side-border",
      dateRangeInput("budget_range", "Project period", start = start_default, end = end_default, width = "100%"),
      tags$div(
        style = "display:none;",
        dateInput("budget_start", NULL, value = start_default),
        dateInput("budget_end", NULL, value = end_default)
      ),
      uiOutput("budget_period_error")
    ),
    column(6,
      numericInput("inflation_pct", "Yearly inflation (%)", value = 0, min = 0, step = 0.1, width = "100%")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Add or edit post"),
      selectizeInput(
        "template_name",
        "Template",
        choices = make_default_templates()$name,
        selected = character(0),
        options = list(
          create = TRUE,
          persist = FALSE,
          placeholder = "Select or type a new template name",
          onInitialize = I('function() { this.clear(); }')
        )
      ),
      actionButton("save_as_template_btn", "Save template", class = "btn-success", style = "margin: 0 max(30px, calc(50% - 100px));width: min(calc(100% - 60px), 200px);max-width: min(calc(100% - 60px), 200px);"),
      tags$hr(style = "margin: 8px 0;"),
      textInput("post_name", required_label("Post name")),
      selectInput("center", required_label("Site"), choices = c("", make_default_sites()$name), selected = ""),
      selectInput("category", required_label("Category"), choices = c("", make_default_categories()$name), selected = ""),
      dateRangeInput("post_date_range", required_label("Date range"), start = NA, end = NA, width = "100%"),
      numericInput("fte", "Full-time equivalent (FTE) / year", value = NA_real_, min = 0, step = 0.1),
      selectInput("value_mode", "How should amount be defined?", choices = c("Formula" = "function", "Variable" = "variable", "Sum of other posts" = "sum"), selected = "function"),
      uiOutput("value_unit_ui"),
      uiOutput("value_inputs_ui"),
      textAreaInput("note", "Note", value = "", rows = 2),
      selectInput("application_status", required_label("Application status"),
                  choices = c("Applied for", "Applied for elsewhere", "Not applied for", "Funded"),
                  selected = "Applied for"),
      tags$div(style = "font-size:0.8em;color:#b00020;margin-top:-4px;", "* = required"),
      fluidRow(
        column(12, uiOutput("add_or_update_button"))
      ),
      uiOutput("form_error"),
      uiOutput("success_feedback")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Post search and overview",
          fluidRow(
            column(
              3,
              selectInput(
                "period_format",
                "Display by time periods:",
                choices = c("Month" = "month", "Calendar year" = "calendar_year", "Project year" = "project_year"),
                selected = "project_year"
              )
            ),
            column(
              5,
              checkboxGroupInput(
                "squash_dims",
                "Collapse:",
                choices = c("Period", "Post name", "Site", "Category"),
                selected = character(0),
                inline = TRUE
              )
            ),
            column(
              4,
              radioButtons(
                "display_form",
                "Display form:",
                choices = c("Long (rows)" = "long", "Wide (columns)" = "wide"),
                selected = "wide",
                inline = TRUE
              )
            )
          ),
          h4("Filter by:"),
          fluidRow(
            column(3, selectizeInput("filter_name", "Post name", choices = character(0), multiple = TRUE)),
            column(3, selectizeInput("filter_center", "Site", choices = character(0), multiple = TRUE)),
            column(3, selectizeInput("filter_category", "Category", choices = character(0), multiple = TRUE)),
            column(3, selectizeInput("filter_status", "Application status", choices = c("Applied for", "Applied for elsewhere", "Not applied for", "Funded"), selected = character(0), multiple = TRUE))
          ),
          fluidRow(
            column(3, numericInput("filter_amount_min", "Min amount", value = 0)),
            column(3, numericInput("filter_amount_max", "Max amount", value = 50000000)),
            column(3, textInput("filter_text", "Free-text search", value = "", placeholder = "Search in period, post name, site, category")),
            column(3, dateRangeInput("filter_month_range", "Period (inclusive)", start = start_default, end = end_default, format = "yyyy-mm"))
          ),

          fluidRow(
            tags$div(
              style = "display: flex; justify-content: flex-end; padding: 10px 15px 0;",
              actionButton(
                "help_refresh_table",
                "Refresh table",
                icon = icon("refresh"),
                class = "btn-info",
                title = "If the budget table appears stuck, click to refresh the budget tables without reloading the page."
              )
            )
          ),
          fluidRow(
            style = "padding: 15px 0;",
            column(4, actionButton("edit_selected", "Edit post", class = "btn-primary", style = "width: 100%;")),
            column(4, actionButton("delete_selected", "Delete post", class = "btn-danger", style = "width: 100%;")),
            column(4, actionButton("make_inactive", "Make inactive", class = "btn-warning", style = "width: 100%;"))
          ),
          br(),
          uiOutput("post_table_container"),
          br(),
          htmlOutput("amendment_status"),
          uiOutput("inactive_posts_section"),
          br(),
          uiOutput("visualization_section"),
          br()
        ),
        tabPanel(
          "Salary calculations",
          fluidRow(
            column(4, textInput("salary_name", "Name of salary calculation")),
            column(2, selectInput("salary_unit", "Monthly or yearly", choices = c("month", "year"), selected = "year")),
            column(3, numericInput("salary_base", "Base salary", value = 0, min = 0, step = 100)),
            column(3, selectInput("salary_pension_mode", "Pension mode", choices = c("percentage", "numeric"), selected = "percentage"))
          ),
          fluidRow(
            column(3, numericInput("salary_pension_value", "Pension (% or amount)", value = 19.36, min = 0, step = 0.1)),
            column(3, numericInput("salary_own_pension_pct", "Own part of pension (%)", value = 33.3, min = 0, step = 0.1)),
            column(3, numericInput("salary_wage_supplement", "Wage supplements (no pension)", value = 0, min = 0, step = 100)),
            column(3, tags$div())
          ),
          fluidRow(
            column(3, numericInput("salary_holiday_rate", "Holiday allowance rate (%)", value = 12.5, min = 0, step = 0.1)),
            column(3, tags$div(style = "margin-top: 25px;", checkboxInput("salary_subtract_holiday", "Subtract holiday from total", value = TRUE))),
            column(6, tags$div())
          ),
          fluidRow(
            column(3, textOutput("salary_holiday_base")),
            column(3, textOutput("salary_holiday_allowance")),
            column(3, textOutput("salary_total_plus_holiday")),
            column(3, textOutput("salary_total"))
          ),
          fluidRow(
            column(12, uiOutput("salary_error"))
          ),
          fluidRow(
            column(12, actionButton("salary_add_or_update", "Add / Update salary", class = "btn-success", style = "width: 100%; padding: 12px 0; margin: 12px 0;"))
          ),
          tags$hr(),
          br(),
          DTOutput("salary_table"),
          br(),
          fluidRow(
            column(6, actionButton("salary_edit_selected", "Edit selected salary", class = "btn-primary", style = "width: 100%; padding: 10px 0;")),
            column(6, actionButton("salary_delete_selected", "Delete selected salary", class = "btn-danger", style = "width: 100%; padding: 10px 0;"))
          ),
          br(),
          textOutput("salary_identifier_help")
        ),
        # Issue #2: Categories and Templates settings tab
        tabPanel(
          "Categories & Templates",
          tags$div(class = "settings-tab",
          h4("Manage Sites"),
          fluidRow(
            column(12,
              DTOutput("sites_table"),
              br(),
              fluidRow(
                column(6, textInput("site_name", "Name")),
                column(6, tags$div())
              ),
              fluidRow(
                column(4, actionButton("site_add", "Add", class = "btn-success")),
                column(4, actionButton("site_edit_selected", "Edit", class = "btn-primary")),
                column(4, tags$div())
              ),
              fluidRow(
                column(4, actionButton("site_delete_selected", "Delete Site", class = "btn-danger")),
                column(4, actionButton("site_clear_all", "Clear All Sites", class = "btn-warning")),
                column(4, uiOutput("restore_sites_control"))
              ),
              uiOutput("sites_error")
            )
          ),
          br(),
          h4("Manage Categories"),
          fluidRow(
            column(12,
              DTOutput("categories_table"),
              br(),
              fluidRow(
                column(3, textInput("cat_name", "Name")),
                column(3, selectInput("cat_operator", "Operator", choices = c("", "<", "<=", "=", ">", ">=", "!="))),
                column(3, numericInput("cat_amount", "Amount", value = NA_real_)),
                column(3, selectInput("cat_per_unit", "Per", choices = c("", "month", "calendar year", "project year")))
              ),
              tags$div(style = "margin: 4px 0 10px; color: #555;", "Operators define the rule, Amount is the threshold, and Per says whether the threshold applies to each month, each calendar year, or each project year."),
              fluidRow(
                column(4, actionButton("cat_add", "Add", class = "btn-success")),
                column(4, actionButton("cat_edit_selected", "Edit", class = "btn-primary")),
                column(4, tags$div())
              ),
              fluidRow(
                column(4, actionButton("cat_delete_selected", "Delete Category", class = "btn-danger")),
                column(4, actionButton("cat_clear_all", "Clear All Categories", class = "btn-warning")),
                column(4, uiOutput("restore_categories_control"))
              ),
              uiOutput("categories_error")
            )
          ),
          br(),
          h4("Manage Templates"),
          fluidRow(
            column(12,
              DTOutput("templates_table"),
              br(),
              tags$p(tags$em("To add or edit a template, use the 'Save template' button in the Add post panel.")),
              fluidRow(
                column(4, actionButton("tpl_delete_selected", "Delete Template", class = "btn-danger")),
                column(4, actionButton("tpl_clear_all", "Clear All Templates", class = "btn-warning")),
                column(4, uiOutput("restore_templates_control"))
              ),
              uiOutput("templates_error")
            )
          )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    posts = make_empty_posts(),
    inactive_posts = make_empty_posts(),
    salaries = make_default_salaries(),
    workbook_name = "Budget",
    workbook_name_error = NULL,
    site_registry = make_default_sites(),
    category_registry = make_default_categories(),
    template_registry = make_default_templates(),
    next_site_id = max(make_default_sites()$id, na.rm = TRUE) + 1L,
    next_category_id = max(make_default_categories()$id, na.rm = TRUE) + 1L,
    next_template_id = 5L,
    next_id = 1L,
    next_salary_id = 3L,
    editing_id = NA_integer_,
    editing_salary_id = NA_integer_,
    editing_site_id = NA_integer_,
    editing_category_id = NA_integer_,
    editing_template_id = NA_integer_,
    variable_defaults = numeric(),
    constant_expr_draft = "0",
    function_expr_draft = character(),
    post_name_draft = "",
    center_draft = "",
    category_draft = "",
    start_date_draft = as.Date(NA),
    end_date_draft = as.Date(NA),
    fte_draft = NA_real_,
    value_mode_draft = "function",
    value_unit_draft = "month",
    application_status_draft = "Applied for",
    note_draft = "",
    value_inputs_refresh = 0L,
    form_error_text = NULL,
    form_error_at = NULL,
    salary_error_text = NULL,
    budget_error_text = NULL,
    export_error_text = NULL,
    export_error_at = NULL,
    sites_error_text = NULL,
    categories_error_text = NULL,
    templates_error_text = NULL,
    settings_error_text = NULL,
    pending_post_row = NULL,
    amend_fields = character(),
    table_refresh_nonce = 0L,
    success_text = NULL,
    success_at = NULL,
    post_import_issues = tibble(id = integer(), import_issues = character()),
    current_post_amendment_reason = NULL
  )

  show_error_modal <- function(msg) {
    showModal(modalDialog(
      title = "Error",
      msg,
      easyClose = FALSE,
      footer = modalButton("Dismiss")
    ))
  }

  set_success <- function(msg) {
    rv$success_text <- msg
    rv$success_at <- Sys.time()
  }

  observeEvent(input$help_refresh_table, {
    rv$table_refresh_nonce <- rv$table_refresh_nonce + 1L
    set_success("Budget table refresh triggered.")
  })

  is_amended_field <- function(field_id) {
    field_id %in% rv$amend_fields
  }

  refresh_post_field_labels <- function() {
    updateTextInput(session, "post_name", label = amend_label("Post name", show_amend = is_amended_field("post_name"), required = TRUE))
    updateSelectInput(session, "center", label = amend_label("Site", show_amend = is_amended_field("center"), required = TRUE))
    updateSelectInput(session, "category", label = amend_label("Category", show_amend = is_amended_field("category"), required = TRUE))
    updateDateRangeInput(session, "post_date_range", label = amend_label("Date range", show_amend = is_amended_field("post_date_range"), required = TRUE))
    updateTextInput(session, "function_expr", label = amend_label("Formula expression", show_amend = is_amended_field("function_expr")))
  }

  reset_form <- function() {
    rv$editing_id <- NA_integer_
    rv$variable_defaults <- numeric()
    rv$constant_expr_draft <- "0"
    rv$function_expr_draft <- ""
    rv$post_name_draft <- ""
    rv$center_draft <- ""
    rv$category_draft <- ""
    rv$start_date_draft <- as.Date(NA)
    rv$end_date_draft <- as.Date(NA)
    rv$fte_draft <- NA_real_
    rv$value_mode_draft <- "function"
    rv$value_unit_draft <- "month"
    rv$application_status_draft <- "Applied for"
    rv$note_draft <- ""
    rv$sum_multiplier_draft <- NULL
    rv$sum_sites_draft <- NULL
    rv$sum_statuses_draft <- NULL
    rv$sum_posts_draft <- NULL
    rv$amend_fields <- character(0)
    # Clear all form inputs to blank/default state
    updateSelectizeInput(session, "template_name", selected = "")
    updateTextInput(session, "post_name", value = "")
    updateSelectInput(session, "center", selected = "")
    updateSelectInput(session, "category", selected = "")
    updateDateRangeInput(session, "post_date_range", start = isolate(input$budget_start %||% NA), end = isolate(input$budget_end %||% NA))
    updateNumericInput(session, "fte", value = NA_real_)
    updateSelectInput(session, "value_mode", selected = "function")
    updateRadioButtons(session, "value_unit", selected = "month")
    updateTextInput(session, "function_expr", value = "")
    updateTextInput(session, "sum_multiplier", value = "")
    updateSelectizeInput(session, "sum_sites", selected = character())
    updateSelectizeInput(session, "sum_statuses", selected = character())
    updateSelectizeInput(session, "sum_posts", selected = character())
    updateTextAreaInput(session, "note", value = "")
    updateSelectInput(session, "application_status", selected = "Applied for")
    rv$value_inputs_refresh <- rv$value_inputs_refresh + 1L
    refresh_post_field_labels()
  }

  output$budget_period_error <- renderUI({
    if (is.null(rv$budget_error_text)) return(NULL)
    tags$div(class = "inline-error", rv$budget_error_text)
  })

  # Initialize form inputs once on page load (fires once when value_mode first becomes non-NULL)
  observeEvent(input$value_mode, {
    reset_form()
  }, once = TRUE, ignoreInit = FALSE)

  # Render button with dynamic text based on edit mode
  output$add_or_update_button <- renderUI({
    button_text <- if (is.na(rv$editing_id)) "Add budget post" else "Save edit"
    actionButton("add_or_update", button_text, class = "btn-success", style = "width: 100%;")
  })

  output$workbook_name_error <- renderUI({
    if (is.null(rv$workbook_name_error)) return(NULL)
    tags$div(class = "inline-error", rv$workbook_name_error)
  })

  output$form_error <- renderUI({
    if (is.null(rv$form_error_text)) return(NULL)
    tags$div(class = "flash-msg flash-error", rv$form_error_text)
  })

  observeEvent(rv$form_error_text, {
    if (!is.null(rv$form_error_text)) {
      rv$form_error_at <- Sys.time()
      stamp <- rv$form_error_at
      later::later(function() {
        if (!is.null(rv$form_error_at) && identical(rv$form_error_at, stamp)) {
          rv$form_error_text <- NULL
          rv$form_error_at <- NULL
        }
      }, delay = 4)
    }
  }, ignoreInit = TRUE)

  output$export_error <- renderUI({
    if (is.null(rv$export_error_text)) return(NULL)
    tags$div(class = "flash-msg flash-error", rv$export_error_text)
  })

  observeEvent(rv$export_error_text, {
    if (!is.null(rv$export_error_text)) {
      rv$export_error_at <- Sys.time()
      stamp <- rv$export_error_at
      later::later(function() {
        if (!is.null(rv$export_error_at) && identical(rv$export_error_at, stamp)) {
          rv$export_error_text <- NULL
          rv$export_error_at <- NULL
        }
      }, delay = 4)
    }
  }, ignoreInit = TRUE)

  output$salary_error <- renderUI({
    if (is.null(rv$salary_error_text)) return(NULL)
    tags$div(class = "inline-error", rv$salary_error_text)
  })

  output$categories_error <- renderUI({
    if (is.null(rv$categories_error_text)) return(NULL)
    tags$div(class = "inline-error", rv$categories_error_text)
  })

  output$sites_error <- renderUI({
    if (is.null(rv$sites_error_text)) return(NULL)
    tags$div(class = "inline-error", rv$sites_error_text)
  })

  output$templates_error <- renderUI({
    if (is.null(rv$templates_error_text)) return(NULL)
    tags$div(class = "inline-error", rv$templates_error_text)
  })

  output$sites_table <- renderDT({
    site_tbl <- rv$site_registry
    needed <- c("id", "name", "is_default", "is_locked", "is_deleted")
    if (!all(needed %in% names(site_tbl))) {
      site_tbl <- make_default_sites()
    }
    site_tbl <- site_tbl %>%
      mutate(
        id = suppressWarnings(as.integer(id)),
        name = as.character(name),
        is_default = as.logical(is_default),
        is_locked = as.logical(is_locked),
        is_deleted = as.logical(is_deleted)
      )

    datatable(
      site_tbl %>%
        filter(!is_deleted) %>%
        select(name),
      colnames = c("Name"),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })

  output$categories_table <- renderDT({
    cat_tbl <- rv$category_registry
    needed <- c("id", "name", "operator", "amount", "per_unit", "is_default", "is_locked", "is_deleted")
    if (!all(needed %in% names(cat_tbl))) {
      cat_tbl <- make_default_categories()
    }
    cat_tbl <- cat_tbl %>%
      mutate(
        id = suppressWarnings(as.integer(id)),
        name = as.character(name),
        operator = as.character(operator),
        amount = suppressWarnings(as.numeric(amount)),
        per_unit = as.character(per_unit),
        is_default = as.logical(is_default),
        is_locked = as.logical(is_locked),
        is_deleted = as.logical(is_deleted)
      )

    datatable(
      cat_tbl %>%
        filter(!is_deleted) %>%
        select(name, operator, amount, per_unit),
      colnames = c("Name", "Operator", "Amount", "Per"),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })

  output$templates_table <- renderDT({
    tpl_tbl <- rv$template_registry
    needed <- c("id", "name", "category", "mode", "unit", "is_default", "is_deleted")
    if (!all(needed %in% names(tpl_tbl))) {
      tpl_tbl <- make_default_templates()
    }
    tpl_tbl <- tpl_tbl %>%
      mutate(
        id = suppressWarnings(as.integer(id)),
        name = as.character(name),
        category = as.character(category),
        mode = as.character(mode),
        unit = as.character(unit),
        constant_expr = if ("constant_expr" %in% names(.)) as.character(constant_expr) else "0",
        function_expr = if ("function_expr" %in% names(.)) as.character(function_expr) else "rep(0, n)",
        is_default = as.logical(is_default),
        is_deleted = as.logical(is_deleted)
      ) %>%
      mutate(
        Amount = case_when(
          mode == "function" ~ function_expr,
          TRUE ~ "variable"
        )
      )

    datatable(
      tpl_tbl %>%
        filter(!is_deleted) %>%
        select(name, category, Amount, unit),
      colnames = c("Name", "Category", "Amount", "Unit"),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })

  salary_calc_preview <- reactive({
    tryCatch(
      calc_salary_fields(
        base_salary = input$salary_base,
        unit = input$salary_unit,
        pension_mode = input$salary_pension_mode,
        pension_value = input$salary_pension_value,
        own_pension_pct = input$salary_own_pension_pct,
        wage_supplement = input$salary_wage_supplement,
        holiday_rate_pct = input$salary_holiday_rate,
        subtract_holiday = isTRUE(input$salary_subtract_holiday)
      ),
      error = function(e) NULL
    )
  })

  output$salary_holiday_base <- renderText({
    calc <- salary_calc_preview()
    if (is.null(calc)) return("Holiday base: -")
    unit <- input$salary_unit
    val <- if (identical(unit, "year")) calc$holiday_allowance_total_yearly else calc$holiday_allowance_total_monthly
    suffix <- if (identical(unit, "year")) " (yearly)" else " (monthly)"
    paste0("Holiday base", suffix, ": ", format(round(val, 2), nsmall = 2))
  })

  output$salary_holiday_allowance <- renderText({
    calc <- salary_calc_preview()
    if (is.null(calc)) return("Holiday allowance: -")
    unit <- input$salary_unit
    val <- if (identical(unit, "year")) calc$holiday_allowance_yearly else calc$holiday_allowance_monthly
    suffix <- if (identical(unit, "year")) " (yearly)" else " (monthly)"
    paste0("Holiday allowance", suffix, ": ", format(round(val, 2), nsmall = 2))
  })

  output$salary_total_plus_holiday <- renderText({
    calc <- salary_calc_preview()
    if (is.null(calc)) return("Total incl. holiday: -")
    unit <- input$salary_unit
    val <- if (identical(unit, "year")) calc$total_plus_holiday_salary_yearly else calc$total_plus_holiday_salary_monthly
    suffix <- if (identical(unit, "year")) " (yearly)" else " (monthly)"
    paste0("Total incl. holiday", suffix, ": ", format(round(val, 2), nsmall = 2))
  })

  output$salary_total <- renderText({
    calc <- salary_calc_preview()
    if (is.null(calc)) return("Total salary: -")
    unit <- input$salary_unit
    val <- if (identical(unit, "year")) calc$total_salary_yearly else calc$total_salary_monthly
    suffix <- if (identical(unit, "year")) " (yearly)" else " (monthly)"
    paste0("Total salary", suffix, ": ", format(round(val, 2), nsmall = 2))
  })

  output$salary_table <- renderDT({
    datatable(
      rv$salaries %>%
        select(
          identifier, name,
          base_salary_monthly,
          pension_amount_monthly,
          own_pension_amount_monthly,
          holiday_allowance_total_monthly,
          holiday_allowance_monthly,
          total_plus_holiday_salary_monthly, total_plus_holiday_salary_yearly,
          total_salary_monthly, total_salary_yearly
        ),
      colnames = c(
        "ID", "Name",
        "Base (base_m)",
        "Pension (pension_m)",
        "Own pens (own_pension_m)",
        "Hol base (holiday_base_m)",
        "Holiday (holiday_m)",
        "Total+hol (total_plus_holiday_m)", "Total+hol (total_plus_holiday_y)",
        "Total (total_m)", "Total (total_y)"
      ),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatRound(
        columns = c(
          "base_salary_monthly",
          "pension_amount_monthly",
          "own_pension_amount_monthly",
          "holiday_allowance_total_monthly",
          "holiday_allowance_monthly",
          "total_plus_holiday_salary_monthly", "total_plus_holiday_salary_yearly",
          "total_salary_monthly", "total_salary_yearly"
        ),
        digits = 2
      )
  })

  output$salary_identifier_help <- renderText({
    if (!nrow(rv$salaries)) {
      return("Formula helper: after you add salaries, reference them like phd_student$total_m.")
    }
    ids <- paste(rv$salaries$identifier, collapse = ", ")
    first_id <- rv$salaries$identifier[[1]]
    paste0("Formula helper: identifiers are ", ids, ". Example: apply_inflation_month(", first_id, "$total_m)")
  })

  selected_salary_id <- reactive({
    sel <- input$salary_table_rows_selected
    if (!length(sel) || !nrow(rv$salaries)) return(NA_integer_)
    rv$salaries$id[sel]
  })

  observeEvent(input$salary_add_or_update, {
    rv$salary_error_text <- NULL

    if (is.null(input$salary_name) || !nzchar(trimws(input$salary_name))) {
      rv$salary_error_text <- "Salary calculation name is required."
      return()
    }

    calc <- tryCatch(
      calc_salary_fields(
        base_salary = input$salary_base,
        unit = input$salary_unit,
        pension_mode = input$salary_pension_mode,
        pension_value = input$salary_pension_value,
        own_pension_pct = input$salary_own_pension_pct
      ),
      error = function(e) {
        rv$salary_error_text <- e$message
        NULL
      }
    )
    if (is.null(calc)) return()

    is_new <- is.na(rv$editing_salary_id)
    sid <- if (is_new) rv$next_salary_id else rv$editing_salary_id
    if (is_new) {
      existing_ids <- rv$salaries %>% pull(identifier)
      identifier <- generate_salary_identifier(input$salary_name, existing_ids)
    } else {
      identifier <- rv$salaries %>% filter(id == sid) %>% pull(identifier) %>% .[1]
    }

    row <- tibble(
      id = sid,
      identifier = identifier,
      name = input$salary_name,
      unit = input$salary_unit,
      base_salary = as.numeric(input$salary_base),
      pension_mode = input$salary_pension_mode,
      pension_value = as.numeric(input$salary_pension_value),
      own_pension_pct = as.numeric(input$salary_own_pension_pct),
      wage_supplement = as.numeric(input$salary_wage_supplement),
      holiday_rate = as.numeric(input$salary_holiday_rate),
      subtract_holiday = isTRUE(input$salary_subtract_holiday),
      base_salary_monthly = calc$base_salary_monthly,
      wage_supplement_monthly = calc$wage_supplement_monthly,
      pension_amount_monthly = calc$pension_amount_monthly,
      own_pension_amount_monthly = calc$own_pension_amount_monthly,
      holiday_allowance_total_monthly = calc$holiday_allowance_total_monthly,
      holiday_allowance_monthly = calc$holiday_allowance_monthly,
      total_plus_holiday_salary_monthly = calc$total_plus_holiday_salary_monthly,
      total_salary_monthly = calc$total_salary_monthly,
      base_salary_yearly = calc$base_salary_yearly,
      wage_supplement_yearly = calc$wage_supplement_yearly,
      pension_amount_yearly = calc$pension_amount_yearly,
      own_pension_amount_yearly = calc$own_pension_amount_yearly,
      holiday_allowance_total_yearly = calc$holiday_allowance_total_yearly,
      holiday_allowance_yearly = calc$holiday_allowance_yearly,
      total_plus_holiday_salary_yearly = calc$total_plus_holiday_salary_yearly,
      total_salary_yearly = calc$total_salary_yearly
    )

    if (is_new) {
      rv$salaries <- bind_rows(rv$salaries, row)
      rv$next_salary_id <- rv$next_salary_id + 1L
      set_success(paste("Salary calculation added:", row$name))
    } else {
      rv$salaries <- rv$salaries %>% filter(id != sid) %>% bind_rows(row)
      rv$editing_salary_id <- NA_integer_
      set_success(paste("Salary calculation updated:", row$name))
    }
  })

  observeEvent(input$salary_edit_selected, {
    sid <- selected_salary_id()
    req(!is.na(sid))
    row <- rv$salaries %>% filter(id == sid)
    req(nrow(row) == 1)

    rv$editing_salary_id <- sid
    updateTextInput(session, "salary_name", value = row$name)
    updateSelectInput(session, "salary_unit", selected = row$unit)
    updateNumericInput(session, "salary_base", value = row$base_salary)
    updateSelectInput(session, "salary_pension_mode", selected = row$pension_mode)
    updateNumericInput(session, "salary_pension_value", value = row$pension_value)
    updateNumericInput(session, "salary_own_pension_pct", value = row$own_pension_pct)
    updateNumericInput(session, "salary_wage_supplement", value = row$wage_supplement)
    updateNumericInput(session, "salary_holiday_rate", value = row$holiday_rate)
    updateCheckboxInput(session, "salary_subtract_holiday", value = isTRUE(row$subtract_holiday))
    set_success("Salary calculation loaded for editing.")
  })

  observeEvent(input$salary_delete_selected, {
    sid <- selected_salary_id()
    req(!is.na(sid))
    rv$salaries <- rv$salaries %>% filter(id != sid)
    if (!is.na(rv$editing_salary_id) && rv$editing_salary_id == sid) {
      rv$editing_salary_id <- NA_integer_
    }
    set_success("Salary calculation deleted.")
  })

  output$success_feedback <- renderUI({
    if (is.null(rv$success_text)) return(NULL)
    tags$div(class = "flash-msg flash-success", rv$success_text)
  })

  observeEvent(rv$success_at, {
    if (!is.null(rv$success_at)) {
      stamp <- rv$success_at
      later::later(function() {
        if (!is.null(rv$success_at) && identical(rv$success_at, stamp)) {
          rv$success_text <- NULL
          rv$success_at <- NULL
        }
      }, delay = 4)
    }
  }, ignoreInit = TRUE)

  observeEvent(TRUE, {
    updateDateRangeInput(session, "post_date_range", start = input$budget_start, end = input$budget_end)
  }, once = TRUE)

  observeEvent(input$budget_range, {
    req(!is.null(input$budget_range), length(input$budget_range) == 2)
    range_start <- as.Date(input$budget_range[1])
    range_end <- as.Date(input$budget_range[2])
    if (!is.na(range_start) && !identical(as.Date(input$budget_start), range_start)) {
      updateDateInput(session, "budget_start", value = range_start)
    }
    if (!is.na(range_end) && !identical(as.Date(input$budget_end), range_end)) {
      updateDateInput(session, "budget_end", value = range_end)
    }
  }, ignoreInit = FALSE)

  observeEvent(c(input$budget_start, input$budget_end), {
    req(input$budget_start, input$budget_end)

    if (input$budget_end < input$budget_start) {
      rv$budget_error_text <- "Budget end must be on or after budget start."
      return()
    }

    rv$budget_error_text <- NULL
    rv$export_error_text <- NULL
    updateDateRangeInput(session, "budget_range", start = input$budget_start, end = input$budget_end)
    updateDateRangeInput(session, "filter_month_range", start = input$budget_start, end = input$budget_end)

    rv$posts <- flag_posts(rv$posts, input$budget_start, input$budget_end, preserve_existing = TRUE, post_import_issues = rv$post_import_issues)
  }, ignoreInit = FALSE)

  # Issue #3: Workbook name validation and live updates while typing
  observeEvent(c(input$workbook_name_live, input$workbook_name), {
    name <- if (!is.null(input$workbook_name_live)) input$workbook_name_live else input$workbook_name
    error <- validate_workbook_name(name)
    rv$workbook_name_error <- error
    if (is.null(error)) {
      rv$workbook_name <- sanitize_workbook_name(name)
    }
  }, ignoreInit = TRUE)

  workbook_name_effective <- reactive({
    live_name <- if (!is.null(input$workbook_name_live)) input$workbook_name_live else ""
    typed_name <- if (!is.null(input$workbook_name)) input$workbook_name else ""
    preferred <- if (nzchar(trimws(live_name))) live_name else typed_name
    if (!nzchar(trimws(preferred))) preferred <- rv$workbook_name
    sanitize_workbook_name(preferred)
  })

  # Keep site/category/template selectors synchronized with registries
  observe({
    active_sites <- rv$site_registry %>% filter(!is_deleted) %>% pull(name)
    current_site <- isolate(if (!is.null(input$center)) trimws(input$center) else "")
    selected_site <- if (nzchar(current_site) && current_site %in% active_sites) current_site else ""
    updateSelectInput(session, "center", choices = c("", active_sites), selected = selected_site)
  })

  observe({
    active_categories <- rv$category_registry %>% filter(!is_deleted) %>% pull(name)
    updateSelectInput(session, "category", choices = c("", active_categories), selected = isolate(input$category))
    updateSelectInput(session, "tpl_category", choices = c("", active_categories), selected = isolate(input$tpl_category))
  })

  observeEvent(rv$template_registry, {
    active_templates <- rv$template_registry %>% filter(!is_deleted) %>% pull(name)
    current_name <- isolate(if (!is.null(input$template_name)) trimws(input$template_name) else "")
    selected <- if (nzchar(current_name) && current_name %in% active_templates) current_name else character(0)
    updateSelectizeInput(session, "template_name", choices = active_templates, selected = selected, server = FALSE)
  }, ignoreInit = TRUE)

  output$restore_categories_control <- renderUI({
    has_deleted_defaults <- nrow(rv$category_registry) > 0 && any(rv$category_registry$is_default & !rv$category_registry$is_locked & rv$category_registry$is_deleted)
    if (!has_deleted_defaults) return(NULL)
    actionButton("restore_default_categories", "Restore Default Categories", class = "btn-success")
  })

  output$restore_sites_control <- renderUI({
    has_deleted_defaults <- nrow(rv$site_registry) > 0 && any(rv$site_registry$is_default & rv$site_registry$is_deleted)
    if (!has_deleted_defaults) return(NULL)
    actionButton("restore_default_sites", "Restore Default Sites", class = "btn-success")
  })

  output$restore_templates_control <- renderUI({
    all_deleted <- nrow(rv$template_registry) > 0 && all(rv$template_registry$is_deleted)
    if (!all_deleted) return(NULL)
    actionButton("restore_default_templates", "Restore Default Templates", class = "btn-success")
  })

  selected_site_id <- reactive({
    sel <- input$sites_table_rows_selected
    visible_sites <- rv$site_registry %>% filter(!is_deleted)
    if (!length(sel) || !nrow(visible_sites) || sel > nrow(visible_sites)) return(NA_integer_)
    visible_sites$id[sel]
  })

  observeEvent(input$site_edit_selected, {
    site_id <- selected_site_id()
    req(!is.na(site_id))
    row <- rv$site_registry %>% filter(id == site_id)
    req(nrow(row) == 1)
    rv$editing_site_id <- site_id
    updateTextInput(session, "site_name", value = row$name)
    set_success("Site loaded into fields for editing.")
  })

  observeEvent(input$site_add, {
    rv$sites_error_text <- NULL
    nm <- trimws(input$site_name)
    if (!nzchar(nm)) {
      rv$sites_error_text <- "Site name is required."
      return()
    }

    is_new <- is.na(rv$editing_site_id)
    
    # If adding new site, check if a deleted site with this name exists - reuse its ID
    if (is_new) {
      deleted_with_name <- rv$site_registry %>%
        filter(is_deleted, tolower(name) == tolower(nm))
      if (nrow(deleted_with_name) == 1) {
        site_id <- deleted_with_name$id[1]
        is_new <- FALSE
      } else {
        site_id <- rv$next_site_id
      }
    } else {
      site_id <- rv$editing_site_id
    }
    
    prev <- rv$site_registry %>% filter(id == site_id)
    prev_default <- if (nrow(prev) == 1) isTRUE(prev$is_default) else FALSE
    prev_locked <- if (nrow(prev) == 1) isTRUE(prev$is_locked) else FALSE

    dup <- rv$site_registry %>%
      filter(!is_deleted, id != site_id, tolower(name) == tolower(nm))
    if (nrow(dup) > 0) {
      rv$sites_error_text <- "Site name already exists."
      return()
    }

    new_site <- tibble(
      id = as.integer(site_id),
      name = nm,
      is_default = prev_default,
      is_locked = prev_locked,
      is_deleted = FALSE
    )

    old_name <- if (nrow(prev) == 1) prev$name[[1]] else NULL
    rv$site_registry <- rv$site_registry %>% filter(id != site_id) %>% bind_rows(new_site)
    # Only increment next_site_id if we actually used it for a new site
    if (is_new && site_id == rv$next_site_id) rv$next_site_id <- rv$next_site_id + 1L

    if (!is.null(old_name) && nzchar(old_name) && old_name != nm) {
      rv$posts <- rv$posts %>% mutate(center = if_else(center == old_name, nm, center))
    }

    # Auto-resolve amendment errors for posts that had this site marked as invalid
    if (nrow(rv$post_import_issues) > 0) {
      site_error_pattern <- paste0("Site not in registry: ", nm)
      posts_with_site_issue <- rv$post_import_issues %>%
        filter(grepl(site_error_pattern, import_issues, fixed = TRUE))
      
      if (nrow(posts_with_site_issue) > 0) {
        for (i in seq_len(nrow(posts_with_site_issue))) {
          issue_id <- posts_with_site_issue$id[i]
          current_issues <- posts_with_site_issue$import_issues[i]
          
          # Remove only the site error from this post's issues
          issue_list <- strsplit(current_issues, "; ", fixed = TRUE)[[1]]
          new_issues <- issue_list[!grepl(site_error_pattern, issue_list, fixed = TRUE)]
          new_issues_text <- paste(new_issues, collapse = "; ")
          
          if (nzchar(new_issues_text)) {
            # Still has other issues - update the issues text
            rv$post_import_issues <- rv$post_import_issues %>%
              mutate(import_issues = if_else(id == issue_id, new_issues_text, import_issues))
          } else {
            # No more issues - remove from post_import_issues and clear amendment flag
            rv$post_import_issues <- rv$post_import_issues %>% filter(id != issue_id)
            rv$posts <- rv$posts %>%
              mutate(needs_amendment = if_else(id == issue_id, FALSE, needs_amendment))
          }
        }
      }
    }

    # Also handle posts that were marked for amendment when this site was deleted
    # These are posts with needs_amendment=TRUE, center == site name, and no import issues
    if (nrow(rv$posts) > 0) {
      posts_to_revalidate <- rv$posts %>%
        filter(needs_amendment, center == nm) %>%
        anti_join(rv$post_import_issues, by = "id")
      
      if (nrow(posts_to_revalidate) > 0) {
        for (i in seq_len(nrow(posts_to_revalidate))) {
          post_row <- posts_to_revalidate[i, ]
          
          # Re-validate this post
          revalidation <- check_import_post_issues(
            post_row = post_row,
            existing_posts = rv$posts %>% filter(id != post_row$id),
            salaries_lookup = make_salary_lookup(rv$salaries),
            inflation_pct = input$inflation_pct,
            salaries_tbl = rv$salaries,
            site_registry = rv$site_registry,
            category_registry = rv$category_registry
          )
          
          if (!revalidation$has_issues) {
            # No more issues - clear amendment flag
            rv$posts <- rv$posts %>%
              mutate(needs_amendment = if_else(id == post_row$id, FALSE, needs_amendment))
            
            # If this post is currently being edited, refresh the form labels to remove !!!
            if (!is.na(rv$editing_id) && rv$editing_id == post_row$id) {
              amend_fields <- get_post_amendment_fields(
                post_row = rv$posts %>% filter(id == post_row$id) %>% slice(1),
                budget_start = as.Date(input$budget_start),
                budget_end = as.Date(input$budget_end),
                category_registry = rv$category_registry,
                site_registry = rv$site_registry,
                post_import_issues = rv$post_import_issues
              )
              rv$amend_fields <- amend_fields
              refresh_post_field_labels()
            }
          }
        }
      }
    }

    rv$editing_site_id <- NA_integer_
    updateTextInput(session, "site_name", value = "")
    set_success(if (is_new) paste("Site added:", nm) else paste("Site updated:", nm))
  })

  observeEvent(input$site_delete_selected, {
    site_id <- selected_site_id()
    req(!is.na(site_id))
    row <- rv$site_registry %>% filter(id == site_id)
    req(nrow(row) == 1)
    deleted_site_name <- row$name[[1]]
    if (isTRUE(row$is_locked[[1]])) {
      rv$sites_error_text <- "The default site cannot be deleted."
      return()
    }
    rv$site_registry <- rv$site_registry %>% mutate(is_deleted = if_else(id == site_id, TRUE, is_deleted))
    rv$posts <- rv$posts %>% mutate(needs_amendment = if_else(center == deleted_site_name, TRUE, needs_amendment))
    rv$editing_site_id <- NA_integer_
    set_success("Site deleted and posts marked for amendment.")
  })

  observeEvent(input$site_clear_all, {
    showModal(modalDialog(
      title = "Confirm Clear All Sites",
      "Are you sure? This will delete all sites except Main.",
      footer = tagList(actionButton("site_clear_confirm", "Confirm", class = "btn-danger"), modalButton("Cancel"))
    ))
  })

  observeEvent(input$site_clear_confirm, {
    active_locked_sites <- rv$site_registry %>% filter(!is_deleted, is_locked) %>% pull(name)
    rv$site_registry <- rv$site_registry %>% mutate(is_deleted = if_else(is_locked, FALSE, TRUE))
    rv$posts <- rv$posts %>% mutate(needs_amendment = if_else(!(center %in% active_locked_sites), TRUE, needs_amendment))
    rv$editing_site_id <- NA_integer_
    removeModal()
    set_success("All non-default sites cleared.")
  })

  observeEvent(input$restore_default_sites, {
    defaults <- make_default_sites()
    rv$site_registry <- defaults
    rv$next_site_id <- max(defaults$id, na.rm = TRUE) + 1L
    set_success("Default sites restored.")
  })

  selected_category_id <- reactive({
    sel <- input$categories_table_rows_selected
    visible_cats <- rv$category_registry %>% filter(!is_deleted)
    if (!length(sel) || !nrow(visible_cats) || sel > nrow(visible_cats)) return(NA_integer_)
    visible_cats$id[sel]
  })

  observeEvent(input$cat_edit_selected, {
    cat_id <- selected_category_id()
    req(!is.na(cat_id))
    row <- rv$category_registry %>% filter(id == cat_id)
    req(nrow(row) == 1)
    if (isTRUE(row$is_locked[[1]])) {
      rv$editing_category_id <- NA_integer_
      rv$categories_error_text <- "Uncategorized cannot be edited."
      return()
    }
    rv$editing_category_id <- cat_id
    updateTextInput(session, "cat_name", value = row$name)
    updateSelectInput(session, "cat_operator", selected = row$operator)
    updateNumericInput(session, "cat_amount", value = row$amount)
    updateSelectInput(session, "cat_per_unit", selected = row$per_unit)
    set_success("Category loaded into fields for editing.")
  })

  observeEvent(input$cat_add, {
    rv$categories_error_text <- NULL
    nm <- trimws(input$cat_name)
    if (!nzchar(nm)) {
      rv$categories_error_text <- "Category name is required."
      return()
    }

    is_new <- is.na(rv$editing_category_id)
    cat_id <- if (is_new) rv$next_category_id else rv$editing_category_id
    prev <- rv$category_registry %>% filter(id == cat_id)
    prev_default <- if (nrow(prev) == 1) isTRUE(prev$is_default) else FALSE
    prev_locked <- if (nrow(prev) == 1) isTRUE(prev$is_locked) else FALSE

    if (prev_locked) {
      rv$categories_error_text <- "Uncategorized cannot be edited or deleted."
      return()
    }

    dup <- rv$category_registry %>%
      filter(!is_deleted, id != cat_id, tolower(name) == tolower(nm))
    if (nrow(dup) > 0) {
      rv$categories_error_text <- "Category name already exists."
      return()
    }

    new_cat <- tibble(
      id = as.integer(cat_id),
      name = nm,
      operator = ifelse(is.null(input$cat_operator), "", input$cat_operator),
      amount = suppressWarnings(as.numeric(input$cat_amount)),
      per_unit = ifelse(is.null(input$cat_per_unit), "", input$cat_per_unit),
      is_default = prev_default,
      is_locked = FALSE,
      is_deleted = FALSE
    )

    old_name <- if (nrow(prev) == 1) prev$name[[1]] else NULL
    rv$category_registry <- rv$category_registry %>% filter(id != cat_id) %>% bind_rows(new_cat)
    if (is_new) rv$next_category_id <- rv$next_category_id + 1L

    if (!is.null(old_name) && nzchar(old_name) && old_name != nm) {
      rv$posts <- rv$posts %>% mutate(category = if_else(category == old_name, nm, category))
    }

    rv$editing_category_id <- NA_integer_
    updateTextInput(session, "cat_name", value = "")
    updateSelectInput(session, "cat_operator", selected = "")
    updateNumericInput(session, "cat_amount", value = NA_real_)
    updateSelectInput(session, "cat_per_unit", selected = "")
    set_success(if (is_new) paste("Category added:", nm) else paste("Category updated:", nm))
  })

  observeEvent(input$cat_delete_selected, {
    cat_id <- selected_category_id()
    req(!is.na(cat_id))
    deleted_cat_name <- rv$category_registry %>% filter(id == cat_id) %>% pull(name)
    if (deleted_cat_name == "Uncategorized") {
      rv$categories_error_text <- "Uncategorized cannot be deleted."
      return()
    }
    rv$category_registry <- rv$category_registry %>% mutate(is_deleted = if_else(id == cat_id, TRUE, is_deleted))
    rv$posts <- rv$posts %>% mutate(needs_amendment = if_else(category == deleted_cat_name, TRUE, needs_amendment))
    rv$editing_category_id <- NA_integer_
    set_success("Category deleted and posts marked for amendment.")
  })

  observeEvent(input$cat_clear_all, {
    showModal(modalDialog(
      title = "Confirm Clear All Categories",
      "Are you sure? This will delete all categories (including defaults).",
      footer = tagList(actionButton("cat_clear_confirm", "Confirm", class = "btn-danger"), modalButton("Cancel"))
    ))
  })

  observeEvent(input$cat_clear_confirm, {
    rv$category_registry <- rv$category_registry %>% mutate(is_deleted = if_else(is_locked, FALSE, TRUE))
    rv$posts <- rv$posts %>% mutate(needs_amendment = TRUE)
    rv$editing_category_id <- NA_integer_
    removeModal()
    set_success("All categories cleared.")
  })

  observeEvent(input$restore_default_categories, {
    defaults <- make_default_categories()
    rv$category_registry <- defaults
    rv$next_category_id <- max(defaults$id, na.rm = TRUE) + 1L
    set_success("Default categories restored.")
  })

  selected_template_id <- reactive({
    sel <- input$templates_table_rows_selected
    visible_tpls <- rv$template_registry %>% filter(!is_deleted)
    if (!length(sel) || !nrow(visible_tpls) || sel > nrow(visible_tpls)) return(NA_integer_)
    visible_tpls$id[sel]
  })

  observeEvent(input$tpl_delete_selected, {
    tpl_id <- selected_template_id()
    req(!is.na(tpl_id))
    rv$template_registry <- rv$template_registry %>% mutate(is_deleted = if_else(id == tpl_id, TRUE, is_deleted))
    rv$editing_template_id <- NA_integer_
    set_success("Template deleted.")
  })

  observeEvent(input$tpl_clear_all, {
    showModal(modalDialog(
      title = "Confirm Clear All Templates",
      "Are you sure? This will delete all templates (including defaults).",
      footer = tagList(actionButton("tpl_clear_confirm", "Confirm", class = "btn-danger"), modalButton("Cancel"))
    ))
  })

  observeEvent(input$tpl_clear_confirm, {
    rv$template_registry <- rv$template_registry %>% mutate(is_deleted = TRUE)
    rv$editing_template_id <- NA_integer_
    removeModal()
    set_success("All templates cleared.")
  })

  observeEvent(input$restore_default_templates, {
    defaults <- make_default_templates()
    rv$template_registry <- defaults
    rv$next_template_id <- max(defaults$id, na.rm = TRUE) + 1L
    set_success("Default templates restored.")
  })

  observeEvent(input$template_name, {
    selected_name <- if (!is.null(input$template_name)) trimws(input$template_name) else ""

    if (!nzchar(selected_name)) {
      rv$editing_id <- NA_integer_
      rv$variable_defaults <- numeric()
      rv$constant_expr_draft <- "0"
      rv$function_expr_draft <- ""
      rv$amend_fields <- character(0)
      updateTextInput(session, "post_name", value = "")
      updateSelectInput(session, "center", selected = "")
      updateSelectInput(session, "category", selected = "")
      updateDateRangeInput(session, "post_date_range", start = input$budget_start, end = input$budget_end)
      updateNumericInput(session, "fte", value = NA_real_)
      updateTextInput(session, "function_expr", value = "")
      updateTextAreaInput(session, "note", value = "")
      rv$value_inputs_refresh <- rv$value_inputs_refresh + 1L
      refresh_post_field_labels()
      return()
    }

    tpl <- rv$template_registry %>% filter(!is_deleted, name == selected_name) %>% slice(1)

    if (nrow(tpl) == 0) {
      # Brand-new name typed by user — auto-save as a new template with current form values
      dur_years <- tryCatch({
        if (!is.null(input$post_date_range) && length(input$post_date_range) == 2 && all(!is.na(input$post_date_range))) {
          as.numeric(difftime(as.Date(input$post_date_range[2]), as.Date(input$post_date_range[1]), units = "days")) / 365.25
        } else { NA_real_ }
      }, error = function(e) NA_real_)
      n_vals <- isolate(required_value_count())
      var_vals <- lapply(seq_len(n_vals), function(i) {
        v <- input[[paste0("var_value_", i)]]
        if (is.null(v) || is.na(v)) 0 else as.numeric(v)
      })
      new_id <- rv$next_template_id
      new_tpl <- tibble(
        id             = as.integer(new_id),
        name           = selected_name,
        category       = if (!is.null(input$category)) input$category else "",
        center         = if (!is.null(input$center)) input$center else "",
        mode           = if (!is.null(input$value_mode)) input$value_mode else "function",
        unit           = if (!is.null(input$value_unit)) input$value_unit else "month",
        constant_expr  = rv$constant_expr_draft,
        function_expr  = rv$function_expr_draft,
        fte            = if (!is.null(input$fte)) as.numeric(input$fte) else NA_real_,
        note           = if (!is.null(input$note)) input$note else "",
        values         = list(unlist(var_vals)),
        duration_years = dur_years,
        is_default     = FALSE,
        is_deleted     = FALSE,
        application_status = if (!is.null(input$application_status)) input$application_status else "Applied for"
      )
      rv$template_registry <- bind_rows(rv$template_registry, new_tpl)
      rv$next_template_id <- rv$next_template_id + 1L
      rv$editing_template_id <- new_id
      set_success(paste("Template saved:", selected_name, "— modify fields and click 'Save template' to update."))
      return()
    }

    rv$editing_template_id <- tpl$id
    rv$constant_expr_draft <- tpl$constant_expr
    rv$function_expr_draft <- tpl$function_expr

    updateSelectInput(session, "center", selected = tpl$center)
    updateSelectInput(session, "category", selected = tpl$category)
    selected_mode <- if (identical(tpl$mode, "variable")) "variable" else "function"
    updateSelectInput(session, "value_mode", selected = selected_mode)
    updateRadioButtons(session, "value_unit", selected = tpl$unit)
    updateTextInput(session, "function_expr", value = tpl$function_expr)
    updateNumericInput(session, "fte", value = tpl$fte)
    updateTextAreaInput(session, "note", value = tpl$note)
    updateSelectInput(session, "application_status",
      selected = if (!is.null(tpl$application_status) && nzchar(tpl$application_status)) tpl$application_status else "Applied for")

    if (!is.na(tpl$duration_years)) {
      updateDateRangeInput(session, "post_date_range", start = input$budget_start, end = input$budget_start %m+% years(as.integer(tpl$duration_years)) - days(1))
    } else {
      updateDateRangeInput(session, "post_date_range", start = input$budget_start, end = input$budget_end)
    }

    vals <- tpl$values[[1]]
    rv$variable_defaults <- if (is.null(vals)) numeric() else as.numeric(vals)
    rv$amend_fields <- character(0)
    rv$value_inputs_refresh <- rv$value_inputs_refresh + 1L
    refresh_post_field_labels()
    set_success(paste("Template applied:", selected_name, "— modify fields and click 'Save template' to update."))
  }, ignoreInit = TRUE)



  observeEvent(input$save_as_template_btn, {
    rv$templates_error_text <- NULL
    nm <- trimws(if (!is.null(input$template_name)) input$template_name else "")
    if (!nzchar(nm)) {
      nm <- trimws(if (!is.null(input$post_name)) input$post_name else "")
    }
    if (!nzchar(nm)) {
      rv$templates_error_text <- "Template name is required. Select an existing template name or type a new one in the Template field."
      return()
    }

    is_new <- is.na(rv$editing_template_id)
    tpl_id <- if (is_new) rv$next_template_id else rv$editing_template_id
    prev <- rv$template_registry %>% filter(id == tpl_id)
    prev_default <- if (nrow(prev) == 1) isTRUE(prev$is_default) else FALSE

    dup <- rv$template_registry %>% filter(!is_deleted, id != tpl_id, tolower(name) == tolower(nm))
    if (nrow(dup) > 0) {
      rv$templates_error_text <- "A template with that name already exists."
      return()
    }

    dur_years <- tryCatch({
      if (!is.null(input$post_date_range) && length(input$post_date_range) == 2 && all(!is.na(input$post_date_range))) {
        as.numeric(difftime(as.Date(input$post_date_range[2]), as.Date(input$post_date_range[1]), units = "days")) / 365.25
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)

    n_vals <- isolate(required_value_count())
    var_vals <- lapply(seq_len(n_vals), function(i) {
      v <- input[[paste0("var_value_", i)]]
      if (is.null(v) || is.na(v)) 0 else as.numeric(v)
    })

    new_tpl <- tibble(
      id             = as.integer(tpl_id),
      name           = nm,
      category       = if (!is.null(input$category)) input$category else "",
      center         = if (!is.null(input$center)) input$center else "",
      mode           = if (!is.null(input$value_mode)) input$value_mode else "function",
      unit           = if (!is.null(input$value_unit)) input$value_unit else "month",
      constant_expr  = rv$constant_expr_draft,
      function_expr  = rv$function_expr_draft,
      fte            = if (!is.null(input$fte)) as.numeric(input$fte) else NA_real_,
      note           = if (!is.null(input$note)) input$note else "",
      values         = list(unlist(var_vals)),
      duration_years = dur_years,
      is_default     = prev_default,
      is_deleted     = FALSE,
      application_status = if (!is.null(input$application_status)) input$application_status else "Applied for"
    )

    rv$template_registry <- rv$template_registry %>% filter(id != tpl_id) %>% bind_rows(new_tpl)
    if (is_new) rv$next_template_id <- rv$next_template_id + 1L
    rv$editing_template_id <- NA_integer_
    set_success(if (is_new) paste("Template saved:", nm) else paste("Template updated:", nm))
  })

  required_value_count <- reactive({
    req(input$post_date_range)
    req(length(input$post_date_range) == 2)
    req(all(!is.na(input$post_date_range)))
    range_start <- as.Date(input$post_date_range[1])
    range_end <- as.Date(input$post_date_range[2])
    if (range_end < range_start) return(0L)
    if (input$value_unit == "year") {
      n_years_between(range_start, range_end)
    } else {
      n_months_between(range_start, range_end)
    }
  })

  # Increment value_inputs_ui refresh counter when user changes mode/unit/dates directly
  observeEvent(input$value_mode,       { rv$value_inputs_refresh <- rv$value_inputs_refresh + 1L }, ignoreInit = TRUE)
  observeEvent(input$value_unit,       { rv$value_inputs_refresh <- rv$value_inputs_refresh + 1L }, ignoreInit = TRUE)
  observeEvent(input$post_date_range,  { rv$value_inputs_refresh <- rv$value_inputs_refresh + 1L }, ignoreInit = TRUE)

  # Render unit radio buttons - only allow "Month" for formula and sum modes
  output$value_unit_ui <- renderUI({
    mode <- input$value_mode
    if (mode == "function" || mode == "sum") {
      # Formula and sum modes: only allow monthly
      radioButtons("value_unit", "Input amount per", choices = c("Month" = "month"), selected = "month", inline = TRUE)
    } else {
      # Variable mode: allow both
      radioButtons("value_unit", "Input amount per", choices = c("Month" = "month", "Year" = "year"), selected = "month", inline = TRUE)
    }
  })

  # When mode changes to "function" or "sum", ensure value_unit is set to "month"
  observeEvent(input$value_mode, {
    if (input$value_mode == "function" || input$value_mode == "sum") {
      updateRadioButtons(session, "value_unit", selected = "month")
    }
  }, ignoreInit = TRUE)

  # Reactive for dynamically filtering posts based on sum mode criteria
  # Empty selections mean "show all" (no filter applied)
  sum_mode_filtered_posts <- reactive({
    selected_sites <- input$sum_sites
    selected_statuses <- input$sum_statuses
    
    # If both are empty, show all posts (no filters applied)
    if ((is.null(selected_sites) || length(selected_sites) == 0) && 
        (is.null(selected_statuses) || length(selected_statuses) == 0)) {
      if (nrow(rv$posts) > 0) {
        rv$posts %>%
          filter(value_mode != "sum") %>%
          mutate(label = paste0(post_name, " (", center, ")")) %>%
          pull(label)
      } else {
        character(0)
      }
    } else {
      # At least one filter is set - apply both (OR logic: at least one must be true)
      # But if only sites set, show posts from those sites
      # If only statuses set, show posts with those statuses
      if (is.null(selected_sites) || length(selected_sites) == 0) {
        selected_sites <- unique(rv$site_registry$name[!rv$site_registry$is_deleted])
      }
      if (is.null(selected_statuses) || length(selected_statuses) == 0) {
        selected_statuses <- c("Applied for", "Applied for elsewhere", "Not applied for", "Funded")
      }
      
      if (nrow(rv$posts) > 0) {
        rv$posts %>%
          filter(
            value_mode != "sum",
            center %in% selected_sites,
            application_status %in% selected_statuses
          ) %>%
          mutate(label = paste0(post_name, " (", center, ")")) %>%
          pull(label)
      } else {
        character(0)
      }
    }
  })
  
  # Observer to auto-sync post selection when sites/statuses filters change
  # Keeps selected posts in sync: keeps valid posts, adds new matching posts
  observeEvent(c(input$sum_sites, input$sum_statuses), {
    available <- sum_mode_filtered_posts()
    current_selection <- isolate(input$sum_posts %||% character(0))
    
    # Keep posts that are still available, add all newly available posts
    still_valid <- intersect(current_selection, available)
    posts_to_add <- setdiff(available, current_selection)
    new_selection <- union(still_valid, posts_to_add)
    
    # Only update if selection actually changed
    if (!identical(sort(new_selection), sort(current_selection))) {
      updateSelectizeInput(session, "sum_posts",
                          choices = available,
                          selected = new_selection)
    }
  }, ignoreInit = TRUE, priority = 100)
  
  output$value_inputs_ui <- renderUI({
    rv$value_inputs_refresh  # single reactive dependency

    mode <- isolate(input$value_mode)
    if (is.null(mode) || !nzchar(mode)) {
      return(NULL)
    }

    if (mode == "function") {
      tagList(
        textInput("function_expr", amend_label("Formula expression", show_amend = is_amended_field("function_expr")), value = isolate(rv$function_expr_draft), placeholder = "Expression using n, FTE, fte(site), salary_identifier$total_m and apply_inflation() helper"),
        actionButton("show_formula_help", "Help: available formula variables")
      )
    } else if (mode == "sum") {
      # Sum of other posts mode - show 4 fields
      # Read from draft RVs (not inputs) to handle edit flow correctly
      # Draft RVs are updated synchronously during edit, while inputs update asynchronously
      available_sites <- sort(unique(rv$site_registry$name[!rv$site_registry$is_deleted]))
      available_statuses <- c("Applied for", "Applied for elsewhere", "Not applied for", "Funded")
      
      # Get dynamically filtered posts - updates as sites/statuses change
      filtered_posts_list <- sum_mode_filtered_posts()
      
      # Get current selections from draft RVs (updated during edit) and inputs (updated during user interaction)
      current_multiplier <- isolate(rv$sum_multiplier_draft %||% input$sum_multiplier %||% "1")
      current_sites <- isolate(rv$sum_sites_draft %||% input$sum_sites %||% character())
      current_statuses <- isolate(rv$sum_statuses_draft %||% input$sum_statuses %||% character())
      current_posts <- isolate(rv$sum_posts_draft %||% input$sum_posts %||% character())
      
      tagList(
        textInput("sum_multiplier", "Multiplier formula", value = current_multiplier, placeholder = "e.g., 1, 0.5, FTE*2, etc."),
        selectizeInput("sum_sites", "Sites to include", 
                       choices = available_sites, 
                       selected = current_sites,
                       multiple = TRUE,
                       options = list(plugins = list('remove_button'))),
        selectizeInput("sum_statuses", "Application statuses to include",
                       choices = available_statuses,
                       selected = current_statuses,
                       multiple = TRUE,
                       options = list(plugins = list('remove_button'))),
        selectizeInput("sum_posts", "Posts to include",
                       choices = filtered_posts_list,
                       selected = current_posts,
                       multiple = TRUE,
                       options = list(plugins = list('remove_button')))
      )
    } else {
      # Variable mode
      n_vals <- isolate(required_value_count())
      defaults <- isolate(rv$variable_defaults)
      if (length(defaults) < n_vals) defaults <- c(defaults, rep(0, n_vals - length(defaults)))

      tagList(
        helpText(paste("Provide", n_vals, ifelse(input$value_unit == "year", "yearly", "monthly"), "amounts.")),
        lapply(seq_len(n_vals), function(i) {
          unit_label <- ifelse(input$value_unit == "year", "Year", "Month")
          numericInput(
            inputId = paste0("var_value_", i),
            label = paste(unit_label, i),
            value = defaults[i],
            step = 100
          )
        })
      )
    }
  })

  post_summary <- reactive({
    if (!nrow(rv$posts)) return(tibble())

    salaries_lookup <- make_salary_lookup(rv$salaries)

    map_dfr(seq_len(nrow(rv$posts)), function(i) {
      row <- rv$posts[i, ]
      total <- tryCatch(post_total(row, all_posts_tbl = rv$posts, salaries_lookup = salaries_lookup, inflation_pct = input$inflation_pct, salaries_tbl = rv$salaries, budget_start = as.Date(input$budget_start)), error = function(e) NA_real_)
      tibble(
        id = row$id,
        center = row$center,
        post_name = row$post_name,
        category = row$category,
        start_date = row$start_date,
        end_date = row$end_date,
        fte = row$fte,
        value_mode = row$value_mode,
        value_unit = row$value_unit,
        total_value = total,
        needs_amendment = row$needs_amendment
      )
    })
  })

  observe({
    s <- post_summary()
    site_choices <- sort(unique(rv$site_registry$name[!rv$site_registry$is_deleted]))
    category_choices <- sort(unique(rv$category_registry$name[!rv$category_registry$is_deleted]))
    updateSelectizeInput(session, "filter_name", choices = sort(unique(s$post_name)), selected = character(0), server = TRUE)
    updateSelectizeInput(session, "filter_center", choices = site_choices, selected = character(0), server = TRUE)
    updateSelectizeInput(session, "filter_category", choices = category_choices, selected = character(0), server = TRUE)
  })

  filtered_posts <- reactive({
    rv$table_refresh_nonce

    build_display_table <- function(period_choice, apply_filters = TRUE) {
      if (!nrow(rv$posts)) {
        return(tibble(
          id = integer(),
          Period = character(),
          `Post name` = character(),
          Site = character(),
          Category = character(),
          Note = character(),
          `Start Date` = as.Date(character()),
          `End date` = as.Date(character()),
          FTE = numeric(),
          Amount = numeric(),
          needs_amendment = logical()
        ))
      }

      salaries_lookup <- make_salary_lookup(rv$salaries)
      long <- build_long_budget(rv$posts, input$budget_start, input$budget_end, salaries_lookup = salaries_lookup, inflation_pct = input$inflation_pct, salaries_tbl = rv$salaries)
      if (!nrow(long)) {
        return(tibble(
          id = integer(),
          Period = character(),
          `Post name` = character(),
          Site = character(),
          Category = character(),
          Note = character(),
          `Start Date` = as.Date(character()),
          `End date` = as.Date(character()),
          FTE = numeric(),
          Amount = numeric(),
          needs_amendment = logical()
        ))
      }

      if (apply_filters && !is.null(input$filter_month_range) && all(!is.na(input$filter_month_range))) {
        mstart <- as.Date(input$filter_month_range[1])
        mend <- as.Date(input$filter_month_range[2])
        long <- long %>% filter(month >= mstart, month <= mend)
      }

      meta <- rv$posts %>%
        select(id, start_date, end_date, note, needs_amendment, application_status)

      out <- long %>%
        left_join(meta, by = "id") %>%
        mutate(
          Period = case_when(
            period_choice == "month" ~ period_month,
            period_choice == "calendar_year" ~ as.character(calendar_year),
            TRUE ~ paste0("Year ", project_year)
          ),
          `Post name` = post_name,
          Site = center,
          Category = category,
          Note = note,
          `Start Date` = start_date,
          `End date` = end_date,
          FTE = fte,
          Value = value,
          needs_amendment = needs_amendment
        ) %>%
        select(id, Period, `Post name`, Site, Category, Note, `Start Date`, `End date`, FTE, Value, needs_amendment, application_status)

      if (apply_filters) {
        if (!is.null(input$filter_name) && length(input$filter_name) > 0) {
          out <- out %>% filter(`Post name` %in% input$filter_name)
        }
        if (!is.null(input$filter_center) && length(input$filter_center) > 0) {
          out <- out %>% filter(Site %in% input$filter_center)
        }
        if (!is.null(input$filter_category) && length(input$filter_category) > 0) {
          out <- out %>% filter(Category %in% input$filter_category)
        }
        if (!is.null(input$filter_status) && length(input$filter_status) > 0) {
          out <- out %>% filter(application_status %in% input$filter_status)
        }
      }

      squash <- input$squash_dims
      if (is.null(squash)) squash <- character(0)

      if ("Period" %in% squash) out$Period <- "All periods"
      if ("Post name" %in% squash) out$`Post name` <- "All posts"
      if ("Site" %in% squash) out$Site <- "All sites"
      if ("Category" %in% squash) out$Category <- "All categories"
      if ("Post name" %in% squash) out$Note <- "Mixed notes"

      group_cols <- c()
      if (!("Period" %in% squash)) group_cols <- c(group_cols, "Period")
      if (!("Post name" %in% squash)) group_cols <- c(group_cols, "Post name")
      if (!("Site" %in% squash)) group_cols <- c(group_cols, "Site")
      if (!("Category" %in% squash)) group_cols <- c(group_cols, "Category")
      if (length(group_cols) == 0) group_cols <- "Period"

      out <- out %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(
          Period = if ("Period" %in% group_cols) first(Period) else "All periods",
          `Post name` = if ("Post name" %in% group_cols) first(`Post name`) else "All posts",
          Site = if ("Site" %in% group_cols) first(Site) else "All sites",
          Category = if ("Category" %in% group_cols) first(Category) else "All categories",
          Note = if (n_distinct(Note) == 1) first(Note) else "Mixed notes",
          `Start Date` = if (n_distinct(`Start Date`) == 1) first(`Start Date`) else as.Date(NA),
          `End date` = if (n_distinct(`End date`) == 1) first(`End date`) else as.Date(NA),
          FTE = if (n_distinct(FTE) == 1) first(FTE) else NA_real_,
          Value = sum(Value),
          needs_amendment = any(needs_amendment, na.rm = TRUE),
          id = min(id),
          application_status = first(application_status),
          .groups = "drop"
        )

      if (apply_filters) {
        if (!is.null(input$filter_amount_min) && !is.na(input$filter_amount_min)) {
          out <- out %>% filter(is.na(Value) | Value >= input$filter_amount_min)
        }
        if (!is.null(input$filter_amount_max) && !is.na(input$filter_amount_max)) {
          out <- out %>% filter(is.na(Value) | Value <= input$filter_amount_max)
        }
        if (!is.null(input$filter_text) && nzchar(trimws(input$filter_text))) {
          txt <- tolower(trimws(input$filter_text))
          out <- out %>%
            filter(
              grepl(txt, tolower(Period), fixed = TRUE) |
                grepl(txt, tolower(`Post name`), fixed = TRUE) |
                grepl(txt, tolower(Site), fixed = TRUE) |
                grepl(txt, tolower(Category), fixed = TRUE) |
                grepl(txt, tolower(Note), fixed = TRUE)
            )
        }
      }

      out %>%
        transmute(id, Period, `Post name`, Site, Category, Note, `Start Date`, `End date`, FTE, Amount = Value, needs_amendment, application_status) %>%
        arrange(Site, `Post name`, Period)
    }

    build_display_table(input$period_format, apply_filters = TRUE)
  })

  output$posts_table <- renderDT({
    req(input$display_form == "long")
    tbl <- filtered_posts()
    datatable(
      tbl,
      selection = "single",
      rownames = FALSE,
      filter = "none",
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        stateSave = TRUE,
        searching = FALSE,
        scrollX = TRUE,
        columnDefs = list(list(targets = c(0, 10, 11), visible = FALSE)),
        rowCallback = JS(
          "function(row, data) {",
          "  var flagged = data[10];",
          "  if (flagged === true || flagged === 'TRUE' || flagged === 'true') {",
          "    $(row).css('background-color', 'rgba(255, 0, 0, 0.2)');",
          "  }",
          "  var status = data[11];",
          "  if (status && status !== 'Applied for') {",
          "    $(row).css({'font-style': 'italic', 'color': '#999'});",
          "  }",
          "}"
        )
      )
    ) %>%
      formatRound("Amount", digits = 2, interval = 3, mark = ",")
  })

  output$visualization_section <- renderUI({
    if (nrow(rv$posts) == 0) return(NULL)
    tagList(
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: baseline;",
        h4("Visualization"),
        if (isTRUE(input$display_form == "wide"))
          actionButton("save_pdf_btn", "Save PDF", class = "btn-default btn-sm", icon = icon("file-pdf"))
      ),
      plotlyOutput("visualization_plot", height = "600px", width = "100%")
    )
  })

  output$inactive_posts_section <- renderUI({
    if (nrow(rv$inactive_posts) == 0) return(NULL)
    tagList(
      br(),
      h4("Inactive posts"),
      DTOutput("inactive_posts_table")
    )
  })

  output$inactive_posts_table <- renderDT({
    df <- rv$inactive_posts
    if (nrow(df) == 0) return(NULL)
    display_df <- df %>%
      select(id, post_name, center, import_issues) %>%
      mutate(
        `Post name` = post_name,
        Site = center,
        Action = sprintf(
          '<button class="btn btn-sm btn-success activate-post-btn" data-post-id="%d">Activate post</button>',
          id
        )
      ) %>%
      select(id, import_issues, `Post name`, Site, Action)
    
    tbl <- datatable(
      display_df %>% select(-id, -import_issues),
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE
      ),
      callback = JS(
        "table.on('click', '.activate-post-btn', function() {",
        "  var id = parseInt($(this).data('post-id'));",
        "  Shiny.setInputValue('activate_post_id', id, {priority: 'event'});",
        "});"
      )
    )
    
    tbl
  }, server = FALSE)

  output$post_table_container <- renderUI({
    if (isTRUE(input$display_form == "wide")) {
      tagList(
        uiOutput("wide_total_checkboxes"),
        DTOutput("wide_form_table")
      )
    } else {
      DTOutput("posts_table")
    }
  })

  include_site_totals_now <- function() {
    squash <- input$squash_dims
    if (is.null(squash)) squash <- character(0)
    !any(c("Site", "Post name") %in% squash)
  }

  wide_column_mode_now <- function() {
    squash <- input$squash_dims
    if (is.null(squash)) squash <- character(0)
    if ("Category" %in% squash) "category" else "post"
  }

  build_wide_from_long <- function(long_df, include_site_totals = TRUE, column_mode = c("post", "category")) {
    column_mode <- match.arg(column_mode)
    if (!nrow(long_df)) {
      return(list(
        data = tibble(Period = character()),
        sites = character(),
        posts_by_site = list(),
        spacer_cols = character(),
        numeric_cols = character(),
        flagged_post_cols = character(),
        column_mode = column_mode,
        post_col_status = character(0),
        status_total_labels = character(0),
        grand_total_label = "TOTAL"
      ))
    }

    # Remove summary row before pivoting wide columns.
    data_rows <- long_df %>% filter(Period != "TOTAL")
    if (!nrow(data_rows)) {
      return(list(
        data = tibble(Period = character()),
        sites = character(),
        posts_by_site = list(),
        spacer_cols = character(),
        numeric_cols = character(),
        flagged_post_cols = character(),
        column_mode = column_mode,
        post_col_status = character(0),
        status_total_labels = character(0),
        grand_total_label = "TOTAL"
      ))
    }

    if (identical(column_mode, "category")) {
      post_category_lookup <- rv$posts %>%
        transmute(Site = center, `Post name` = post_name, Category_lookup = category) %>%
        distinct()

      data_rows <- data_rows %>%
        left_join(post_category_lookup, by = c("Site", "Post name")) %>%
        mutate(Category_resolved = if_else(!is.na(Category_lookup) & nzchar(Category_lookup), Category_lookup, Category))
    } else {
      data_rows <- data_rows %>% mutate(Category_resolved = Category)
    }

    amount_col <- if ("Amount" %in% names(data_rows)) "Amount" else if ("Value" %in% names(data_rows)) "Value" else NULL
    if (is.null(amount_col)) stop("No amount column found in table data.")

    periods_vec <- sort(unique(data_rows$Period))
    sites_vec <- unique(data_rows$Site)
    posts_by_site <- lapply(sites_vec, function(site) {
      if (identical(column_mode, "category")) {
        unique(data_rows$Category_resolved[data_rows$Site == site])
      } else {
        unique(data_rows$`Post name`[data_rows$Site == site])
      }
    })
    names(posts_by_site) <- sites_vec

    wide_df <- tibble(Period = periods_vec)
    spacer_cols <- character(0)
    numeric_cols <- character(0)
    flagged_post_cols <- character(0)
    has_status <- "application_status" %in% names(data_rows)
    post_col_status <- character(0)   # named: col_name -> application_status
    site_post_cols <- list()          # named: site -> vector of post col names
    site_total_col_map <- character(0) # named: site -> site-total col name

    for (s_idx in seq_along(sites_vec)) {
      site <- sites_vec[s_idx]
      post_cols <- character(0)

      for (post in posts_by_site[[site]]) {
        col_name <- paste(site, post, sep = " > ")
        wide_df[[col_name]] <- vapply(periods_vec, function(p) {
          vals <- if (identical(column_mode, "category")) {
            data_rows %>%
              filter(Period == p, Site == site, Category_resolved == post) %>%
              pull(all_of(amount_col))
          } else {
            data_rows %>%
              filter(Period == p, Site == site, `Post name` == post) %>%
              pull(all_of(amount_col))
          }
          if (!length(vals)) return(NA_real_)
          sum(as.numeric(vals), na.rm = TRUE)
        }, numeric(1))
        post_cols <- c(post_cols, col_name)

        # Track application status for this column (most common among its data rows)
        if (has_status) {
          if (identical(column_mode, "post")) {
            status_vals <- data_rows$application_status[data_rows$Site == site & data_rows$`Post name` == post]
          } else {
            status_vals <- data_rows$application_status[data_rows$Site == site & data_rows$Category_resolved == post]
          }
          dominant <- if (length(status_vals) > 0) {
            names(sort(table(status_vals), decreasing = TRUE))[1]
          } else {
            "Applied for"
          }
          post_col_status[col_name] <- dominant
        }

        # Mark post columns that contain at least one amendment-required post.
        if (identical(column_mode, "post") && any(data_rows$Site == site & data_rows$`Post name` == post & data_rows$needs_amendment, na.rm = TRUE)) {
          flagged_post_cols <- c(flagged_post_cols, col_name)
        }
      }

      numeric_cols <- c(numeric_cols, post_cols)
      site_post_cols[[site]] <- post_cols

      if (isTRUE(include_site_totals)) {
        total_col <- paste(site, "Site total", sep = " > ")
        post_mat <- as.matrix(wide_df[, post_cols, drop = FALSE])
        post_counts <- rowSums(!is.na(post_mat))
        post_sums <- rowSums(post_mat, na.rm = TRUE)
        wide_df[[total_col]] <- ifelse(post_counts == 0, NA_real_, post_sums)
        numeric_cols <- c(numeric_cols, total_col)
        site_total_col_map[site] <- total_col
      }

      if (s_idx < length(sites_vec)) {
        spacer_col <- strrep(" ", s_idx)
        wide_df[[spacer_col]] <- ""
        spacer_cols <- c(spacer_cols, spacer_col)
      }
    }

    if (isTRUE(include_site_totals)) {
      grand_source_cols <- grep(" > Site total$", names(wide_df), value = TRUE)
    } else {
      grand_source_cols <- setdiff(numeric_cols, "All sites > Total")
    }
    grand_spacer <- strrep(" ", length(sites_vec) + 1)
    while (grand_spacer %in% names(wide_df)) {
      grand_spacer <- paste0(grand_spacer, " ")
    }
    wide_df[[grand_spacer]] <- ""
    spacer_cols <- c(spacer_cols, grand_spacer)

    grand_total_col <- "All sites > Total"
    if (length(grand_source_cols) > 0) {
      wide_df[[grand_total_col]] <- rowSums(as.matrix(wide_df[, grand_source_cols, drop = FALSE]), na.rm = TRUE)
    } else {
      wide_df[[grand_total_col]] <- NA_real_
    }
    numeric_cols <- c(numeric_cols, grand_total_col)

    # Build grand total row (sum across period rows only)
    total_row <- as.list(rep(NA, ncol(wide_df)))
    names(total_row) <- names(wide_df)
    total_row$Period <- "TOTAL"
    for (nm in numeric_cols) {
      total_row[[nm]] <- sum(as.numeric(wide_df[[nm]]), na.rm = TRUE)
    }
    for (nm in spacer_cols) {
      total_row[[nm]] <- ""
    }

    # Build per-application-status total rows (only for statuses present in data)
    status_total_labels <- character(0)
    grand_total_label <- "TOTAL"
    if (has_status && length(post_col_status) > 0) {
      status_order <- c("Applied for", "Applied for elsewhere", "Not applied for", "Funded")
      statuses_present <- intersect(status_order, unique(post_col_status))

      if (length(statuses_present) == 1) {
        # Single status: just use one "Total" row, no separator
        total_row$Period <- "Total"
        grand_total_label <- "Total"
      } else if (length(statuses_present) > 1) {
        # Multiple statuses: separator row + per-status rows
        # Insert a blank separator row before the totals block
        sep_row <- as.list(rep(NA, ncol(wide_df)))
        names(sep_row) <- names(wide_df)
        sep_row$Period <- "Totals"
        for (nm in spacer_cols) sep_row[[nm]] <- ""
        wide_df <- bind_rows(wide_df, as_tibble(sep_row))
        status_total_labels <- c(status_total_labels, "Totals")
      }

      for (status in if (length(statuses_present) > 1) statuses_present else character(0)) {
        status_cols <- names(post_col_status)[post_col_status == status]
        st_row <- as.list(rep(NA, ncol(wide_df)))
        names(st_row) <- names(wide_df)
        label <- status
        st_row$Period <- label
        status_total_labels <- c(status_total_labels, label)

        for (nm in spacer_cols) st_row[[nm]] <- ""

        # Per-site: sum post cols of this status
        for (site in sites_vec) {
          site_cols_for_status <- intersect(site_post_cols[[site]], status_cols)
          if (length(site_cols_for_status) > 0) {
            site_data <- data_rows %>% filter(
              Site == site,
              application_status == status
            )
            for (cn in site_cols_for_status) {
              post_label <- sub(paste0("^", site, " > "), "", cn)
              if (identical(column_mode, "post")) {
                v <- sum(site_data[[amount_col]][site_data$`Post name` == post_label], na.rm = TRUE)
              } else {
                v <- sum(site_data[[amount_col]][site_data$Category_resolved == post_label], na.rm = TRUE)
              }
              st_row[[cn]] <- v
            }
            # Other post cols in this site: leave NA
          }
          if (isTRUE(include_site_totals) && site %in% names(site_total_col_map)) {
            site_vals <- vapply(site_cols_for_status, function(cn) {
              v <- st_row[[cn]]; if (is.null(v) || is.na(v)) 0 else v
            }, numeric(1))
            st_row[[site_total_col_map[site]]] <- if (length(site_vals) > 0) sum(site_vals) else NA_real_
          }
        }

        # Grand total for this status
        if (isTRUE(include_site_totals)) {
          site_tot_vals <- vapply(sites_vec, function(s) {
            v <- st_row[[site_total_col_map[s]]]; if (is.null(v) || is.na(v)) 0 else v
          }, numeric(1))
          st_row[[grand_total_col]] <- sum(site_tot_vals)
        } else {
          all_vals <- vapply(status_cols, function(cn) {
            v <- st_row[[cn]]; if (is.null(v) || is.na(v)) 0 else v
          }, numeric(1))
          st_row[[grand_total_col]] <- sum(all_vals)
        }

        wide_df <- bind_rows(wide_df, as_tibble(st_row))
      }
    }

    # Append grand total row last
    wide_df <- bind_rows(wide_df, as_tibble(total_row))

    list(
      data = wide_df,
      sites = sites_vec,
      posts_by_site = posts_by_site,
      spacer_cols = spacer_cols,
      numeric_cols = numeric_cols,
      flagged_post_cols = unique(flagged_post_cols),
      column_mode = column_mode,
      post_col_status = post_col_status,
      status_total_labels = status_total_labels,
      grand_total_label = grand_total_label
    )
  }

  output$wide_total_checkboxes <- renderUI({
    req(input$display_form == "wide")
    long_data <- filtered_posts()
    if (!nrow(long_data)) return(NULL)
    if (!"application_status" %in% names(long_data)) return(NULL)

    status_order <- c("Applied for", "Applied for elsewhere", "Not applied for", "Funded")
    statuses_present <- intersect(status_order, unique(long_data$application_status))
    if (length(statuses_present) <= 1) return(NULL)

    status_labels <- statuses_present
    all_labels <- status_labels
    choices_vec <- setNames(all_labels, statuses_present)

    tags$div(
      style = "padding: 4px 0 6px;",
      checkboxGroupInput(
        "wide_totals_visible",
        "Show total rows:",
        choices = choices_vec,
        selected = choices_vec,
        inline = TRUE
      )
    )
  })

  output$wide_form_table <- renderDT({
    req(input$display_form == "wide")
    long_data <- filtered_posts()

    if (!nrow(long_data)) {
      return(
        datatable(
          tibble(Message = "No posts to display. Add posts or adjust filters."),
          colnames = "",
          rownames = FALSE,
          options = list(dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE)
        )
      )
    }

    include_site_totals <- include_site_totals_now()
    wide_mode <- wide_column_mode_now()

    wide_obj <- build_wide_from_long(long_data, include_site_totals = include_site_totals, column_mode = wide_mode)
    wide_df <- wide_obj$data
    numeric_cols <- wide_obj$numeric_cols
    flagged_post_cols <- wide_obj$flagged_post_cols

    # Filter total rows based on checkbox selection
    grand_total_label <- wide_obj$grand_total_label
    all_total_labels <- c(wide_obj$status_total_labels, grand_total_label)
    vis_totals <- if (is.null(input$wide_totals_visible)) all_total_labels else input$wide_totals_visible
    # "Totals" separator and grand total are never user-toggleable
    toggleable_labels <- setdiff(all_total_labels, c("Totals", grand_total_label))
    hide_labels <- setdiff(toggleable_labels, vis_totals)
    # Also hide "Totals" separator when all status rows are hidden
    status_data_labels <- setdiff(wide_obj$status_total_labels, "Totals")
    if (length(status_data_labels) > 0 && !any(status_data_labels %in% vis_totals)) {
      hide_labels <- union(hide_labels, "Totals")
    }
    if (length(hide_labels) > 0) {
      wide_df <- wide_df %>% filter(!(Period %in% hide_labels))
    }

    site_counts <- tibble(
      Site = wide_obj$sites,
      n_posts = map_int(wide_obj$posts_by_site, length)
    )
    header_top <- tags$tr(
      tags$th(rowspan = 2, "Period"),
      lapply(seq_len(nrow(site_counts)), function(i) {
        tagList(
          tags$th(colspan = site_counts$n_posts[i] + ifelse(include_site_totals, 1, 0), site_counts$Site[i], class = "wide-header-site"),
          if (i < nrow(site_counts)) tags$th(rowspan = 2, "", class = "wide-header-spacer")
        )
      }),
      tags$th(rowspan = 2, "", class = "wide-header-spacer"),
      tags$th(colspan = 1, "All sites", class = "wide-header-site")
    )
    header_bottom <- tags$tr(
      unlist(lapply(seq_len(nrow(site_counts)), function(i) {
        site <- site_counts$Site[i]
        posts <- wide_obj$posts_by_site[[site]]
        cells <- lapply(posts, function(p) tags$th(p))
        if (isTRUE(include_site_totals)) {
          cells <- c(cells, list(tags$th("Site total", class = "wide-header-total")))
        }
        cells
      })
      , recursive = FALSE),
      tags$th("Total", class = "wide-header-total")
    )
    table_header <- withTags(table(class = "display", thead(header_top, header_bottom)))

    dt <- datatable(
      wide_df,
      rownames = FALSE,
      container = table_header,
      selection = "none",
      options = list(
        pageLength = 25,
        lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "All")),
        stateSave = TRUE,
        searching = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        initComplete = JS(
          "function() {",
          "  var api = this.api();",
          "  var tableNode = api.table().node();",
          "  var $table = $(tableNode);",
          "  Shiny.setInputValue('wide_col_selected', -1, {priority: 'event'});",
          "  $table.on('click', 'tbody td', function() {",
          "    var idx = api.cell(this).index();",
          "    if (!idx) return;",
          "    var col = idx.column;",
          "    var prev = tableNode.dataset.selectedCol;",
          "    if (prev !== undefined && prev !== '' && parseInt(prev, 10) === col) {",
          "      delete tableNode.dataset.selectedCol;",
          "      $(api.cells().nodes()).removeClass('wide-col-selected');",
          "      Shiny.setInputValue('wide_col_selected', -1, {priority: 'event'});",
          "      return;",
          "    }",
          "    tableNode.dataset.selectedCol = col;",
          "    $(api.cells().nodes()).removeClass('wide-col-selected');",
          "    $(api.cells(null, col).nodes()).addClass('wide-col-selected');",
          "    Shiny.setInputValue('wide_col_selected', col + 1, {priority: 'event'});",
          "  });",
          "}"
        ),
        drawCallback = JS(
          "function() {",
          "  var api = this.api();",
          "  var tableNode = api.table().node();",
          "  var col = tableNode.dataset.selectedCol;",
          "  $(api.cells().nodes()).removeClass('wide-col-selected');",
          "  if (col !== undefined && col !== '') {",
          "    $(api.cells(null, parseInt(col, 10)).nodes()).addClass('wide-col-selected');",
          "  }",
          "}"
        ),
        rowCallback = JS(
          "function(row, data) {",
          "  var p = data[0];",
          "  var statusRows = ['Applied for', 'Applied for elsewhere', 'Not applied for', 'Funded'];",
          "  var isStatusRow = statusRows.indexOf(p) !== -1;",
          "  var isTotalRow  = (p === 'TOTAL' || p === 'Total');",
          "  var isSepRow    = (p === 'Totals');",
          "  if (isTotalRow) {",
          "    $(row).css({'font-weight':'700','background-color':'#f1f1f1'});",
          "  } else if (isSepRow) {",
          "    $(row).css({'font-weight':'700','background-color':'#ececec','color':'#555'});",
          "    $('td', row).each(function(i) { if (i > 0) $(this).text(''); });",
          "  } else if (p === 'Applied for') {",
          "    $(row).css({'font-weight':'700','background-color':'#f1f1f1'});",
          "  } else if (isStatusRow) {",
          "    $(row).css({'font-weight':'700','font-style':'italic','color':'#888','background-color':'#f8f8f8'});",
          "  }",
          "}"
        )
      )
    )

    if (length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(columns = numeric_cols, digits = 2, interval = 3, mark = ",")
    }

    if (length(flagged_post_cols) > 0) {
      dt <- dt %>% formatStyle(
        columns = flagged_post_cols,
        backgroundColor = "rgba(255, 0, 0, 0.2)"
      )
    }

    # Grey/italic formatting for non-"Applied for" post columns in wide view
    # Applied only to data rows (not total/separator rows) via rowCallback cell override.
    post_col_status <- wide_obj$post_col_status
    if (length(post_col_status) > 0) {
      non_applied_cols <- names(post_col_status)[post_col_status != "Applied for"]
      non_applied_cols <- intersect(non_applied_cols, names(wide_df))
      if (length(non_applied_cols) > 0) {
        # Get 0-based column indices for non-applied columns
        non_applied_idx <- which(names(wide_df) %in% non_applied_cols) - 1L
        idx_json <- paste0("[", paste(non_applied_idx, collapse = ","), "]")
        dt <- dt %>% htmlwidgets::onRender(paste0(
          "function(el, x) {",
          "  var api = $(el).find('table').DataTable();",
          "  var grayIdx = ", idx_json, ";",
          "  var statusRows = ['Applied for', 'Applied for elsewhere', 'Not applied for', 'Funded', 'Totals', 'TOTAL', 'Total'];",
          "  api.on('draw', function() {",
          "    api.rows().every(function() {",
          "      var d = this.data();",
          "      var p = d[0];",
          "      var isTotalRow = statusRows.indexOf(p) !== -1;",
          "      if (!isTotalRow) {",
          "        var rowNode = this.node();",
          "        grayIdx.forEach(function(ci) {",
          "          var cell = api.cell(rowNode, ci).node();",
          "          if (cell) $(cell).css({'color':'#999','font-style':'italic'});",
          "        });",
          "      }",
          "    });",
          "  }).draw();",
          "}"
        ))
      }
    }

    dt
  })

  selected_post_id <- reactive({
    if (identical(input$display_form, "wide")) {
      col_guard <- input$wide_col_selected
      if (is.null(col_guard) || is.na(col_guard) || as.integer(col_guard) < 1L) return(NA_integer_)

      long_tbl <- filtered_posts()
      if (!nrow(long_tbl)) return(NA_integer_)
      col_idx <- as.integer(col_guard)

      include_site_totals <- include_site_totals_now()
      wide_mode <- wide_column_mode_now()

      wide_obj <- build_wide_from_long(long_tbl, include_site_totals = include_site_totals, column_mode = wide_mode)
      wide_cols <- names(wide_obj$data)
      if (col_idx < 1 || col_idx > length(wide_cols)) return(NA_integer_)

      col_name <- wide_cols[[col_idx]]
      if (identical(col_name, "Period") || col_name %in% wide_obj$spacer_cols || grepl("Site total$", col_name) || identical(col_name, "All sites > Total")) {
        return(NA_integer_)
      }

      if (identical(wide_mode, "category")) return(NA_integer_)

      parts <- strsplit(col_name, " > ", fixed = TRUE)[[1]]
      if (length(parts) < 2) return(NA_integer_)
      site_name <- parts[[1]]
      post_label <- paste(parts[-1], collapse = " > ")

      ids <- rv$posts %>%
        filter(center == site_name, post_name == post_label) %>%
        pull(id) %>%
        unique()

      if (length(ids) != 1) return(NA_integer_)
      return(ids[[1]])
    }

    sel <- input$posts_table_rows_selected
    tbl <- filtered_posts()
    if (!length(sel) || !nrow(tbl)) return(NA_integer_)
    tbl$id[sel]
  })

  observeEvent(input$function_expr, {
    rv$function_expr_draft <- input$function_expr
  }, ignoreInit = TRUE)

  observeEvent(input$post_name, {
    rv$post_name_draft <- input$post_name
  }, ignoreInit = TRUE)

  observeEvent(input$center, {
    rv$center_draft <- trimws(input$center %||% "")
  }, ignoreInit = TRUE)

  observeEvent(input$category, {
    rv$category_draft <- input$category %||% ""
  }, ignoreInit = TRUE)

  observeEvent(input$post_date_range, {
    if (!is.null(input$post_date_range) && length(input$post_date_range) == 2) {
      rv$start_date_draft <- as.Date(input$post_date_range[1])
      rv$end_date_draft   <- as.Date(input$post_date_range[2])
    }
  }, ignoreInit = TRUE)

  observeEvent(input$fte, {
    rv$fte_draft <- suppressWarnings(as.numeric(input$fte))
  }, ignoreInit = TRUE)

  observeEvent(input$value_mode, {
    rv$value_mode_draft <- input$value_mode
  }, ignoreInit = TRUE)

  observeEvent(input$value_unit, {
    rv$value_unit_draft <- input$value_unit
  }, ignoreInit = TRUE)

  observeEvent(input$application_status, {
    rv$application_status_draft <- input$application_status %||% ""
  }, ignoreInit = TRUE)

  observeEvent(input$note, {
    rv$note_draft <- input$note %||% ""
  }, ignoreInit = TRUE)

  observeEvent(input$sum_multiplier, {
    rv$sum_multiplier_draft <- input$sum_multiplier
  }, ignoreInit = TRUE)

  observeEvent(input$sum_sites, {
    rv$sum_sites_draft <- input$sum_sites
  }, ignoreInit = TRUE)

  observeEvent(input$sum_statuses, {
    rv$sum_statuses_draft <- input$sum_statuses
  }, ignoreInit = TRUE)

  observeEvent(input$sum_posts, {
    rv$sum_posts_draft <- input$sum_posts
  }, ignoreInit = TRUE)

  observeEvent(input$edit_selected, {
    sid <- selected_post_id()
    if (is.na(sid)) {
      if (identical(input$display_form, "wide")) {
        wide_mode <- wide_column_mode_now()
        rv$form_error_text <- NULL
        rv$form_error_at <- NULL
        later::later(function() {
          if (identical(wide_mode, "category")) {
            rv$form_error_text <- "Category columns in Wide view are aggregated and cannot be edited. Uncheck 'Collapse: category' and select a specific post column."
          } else {
            rv$form_error_text <- "Select a specific post column in Wide view to edit. Site/grand total columns are not editable."
          }
        }, delay = 0)
      }
      return()
    }

    row <- rv$posts %>% filter(id == sid)
    req(nrow(row) == 1)

    rv$editing_id <- sid
    rv$variable_defaults <- unlist(row$value_vector[[1]], use.names = FALSE)
    rv$constant_expr_draft <- as.character(row$constant_expr[[1]] %||% "0")
    rv$function_expr_draft <- as.character(row$function_expr[[1]] %||% "")
    rv$post_name_draft <- as.character(row$post_name[[1]] %||% "")
    rv$center_draft <- as.character(row$center[[1]] %||% "")
    rv$category_draft <- as.character(row$category[[1]] %||% "")
    rv$start_date_draft <- as.Date(row$start_date[[1]])
    rv$end_date_draft <- as.Date(row$end_date[[1]])
    rv$fte_draft <- as.numeric(row$fte[[1]] %||% NA_real_)
    rv$value_mode_draft <- as.character(row$value_mode[[1]] %||% "function")
    rv$value_unit_draft <- as.character(row$value_unit[[1]] %||% "month")
    rv$application_status_draft <- as.character(row$application_status[[1]] %||% "Applied for")
    rv$note_draft <- as.character(row$note[[1]] %||% "")
    
    # Load sum mode fields
    rv$sum_multiplier_draft <- if (!is.null(row$sum_multiplier[[1]]) && nzchar(as.character(row$sum_multiplier[[1]] %||% ""))) {
      as.character(row$sum_multiplier[[1]])
    } else {
      "1"
    }
    
    rv$sum_sites_draft <- if (!is.null(row$sum_sites[[1]]) && nzchar(as.character(row$sum_sites[[1]] %||% ""))) {
      strsplit(as.character(row$sum_sites[[1]]), "||", fixed = TRUE)[[1]]
    } else {
      character()
    }
    
    rv$sum_statuses_draft <- if (!is.null(row$sum_statuses[[1]]) && nzchar(as.character(row$sum_statuses[[1]] %||% ""))) {
      strsplit(as.character(row$sum_statuses[[1]]), "||", fixed = TRUE)[[1]]
    } else {
      character()
    }
    
    rv$sum_posts_draft <- if (!is.null(row$sum_posts[[1]]) && nzchar(as.character(row$sum_posts[[1]] %||% ""))) {
      strsplit(as.character(row$sum_posts[[1]]), "||", fixed = TRUE)[[1]]
    } else {
      character()
    }

    updateTextInput(session, "post_name", value = row$post_name[[1]])
    updateSelectInput(session, "center", selected = row$center[[1]])
    updateSelectInput(session, "category", selected = row$category[[1]])
    updateDateRangeInput(session, "post_date_range", start = row$start_date[[1]], end = row$end_date[[1]])
    updateNumericInput(session, "fte", value = row$fte[[1]])
    selected_mode <- if (identical(row$value_mode[[1]], "variable")) {
      "variable"
    } else if (identical(row$value_mode[[1]], "sum")) {
      "sum"
    } else {
      "function"
    }
    updateSelectInput(session, "value_mode", selected = selected_mode)
    updateRadioButtons(session, "value_unit", selected = row$value_unit[[1]])
    updateTextInput(session, "function_expr", value = row$function_expr[[1]])
    updateTextInput(session, "sum_multiplier", value = rv$sum_multiplier_draft)
    updateSelectizeInput(session, "sum_sites", selected = rv$sum_sites_draft)
    updateSelectizeInput(session, "sum_statuses", selected = rv$sum_statuses_draft)
    updateSelectizeInput(session, "sum_posts", selected = rv$sum_posts_draft)
    updateTextAreaInput(session, "note", value = row$note[[1]])
    updateSelectInput(session, "application_status",
      selected = if (!is.null(row$application_status[[1]]) && nzchar(row$application_status[[1]])) row$application_status[[1]] else "Applied for")

    # Store amendment reasons for display
    rv$current_post_amendment_reason <- NULL
    if (isTRUE(row$needs_amendment)) {
      # Check for import issues first
      import_issue_row <- rv$post_import_issues %>% filter(id == sid)
      if (nrow(import_issue_row) > 0) {
        rv$current_post_amendment_reason <- import_issue_row$import_issues[[1]]
      }
    }

    amend_fields <- get_post_amendment_fields(
      post_row = row,
      budget_start = as.Date(input$budget_start),
      budget_end = as.Date(input$budget_end),
      category_registry = rv$category_registry,
      site_registry = rv$site_registry,
      post_import_issues = rv$post_import_issues
    )
    rv$amend_fields <- amend_fields
    rv$value_inputs_refresh <- rv$value_inputs_refresh + 1L
    refresh_post_field_labels()

    set_success("Post loaded into form for editing.")
  })

  observeEvent(input$delete_selected, {
    sid <- selected_post_id()
    if (is.na(sid)) {
      if (identical(input$display_form, "wide")) {
        wide_mode <- wide_column_mode_now()
        rv$form_error_text <- NULL
        rv$form_error_at <- NULL
        later::later(function() {
          if (identical(wide_mode, "category")) {
            rv$form_error_text <- "Category columns in Wide view are aggregated and cannot be deleted. Uncheck 'Collapse: category' and select a specific post column."
          } else {
            rv$form_error_text <- "Select a specific post column in Wide view to delete. Site/grand total columns are not deletable."
          }
        }, delay = 0)
      }
      return()
    }
    rv$posts <- rv$posts %>% filter(id != sid)
    set_success("Post deleted.")
  })

  observeEvent(input$make_inactive, {
    sid <- selected_post_id()
    if (is.na(sid)) {
      if (identical(input$display_form, "wide")) {
        wide_mode <- wide_column_mode_now()
        rv$form_error_text <- NULL
        rv$form_error_at <- NULL
        later::later(function() {
          if (identical(wide_mode, "category")) {
            rv$form_error_text <- "Category columns in Wide view are aggregated and cannot be made inactive. Uncheck 'Collapse: category' and select a specific post column."
          } else {
            rv$form_error_text <- "Select a specific post column in Wide view to make inactive. Site/grand total columns are not selectable."
          }
        }, delay = 0)
      }
      return()
    }
    row_to_deactivate <- rv$posts %>% filter(id == sid)
    rv$posts <- rv$posts %>% filter(id != sid)
    rv$inactive_posts <- bind_rows(rv$inactive_posts, row_to_deactivate)
    set_success("Post made inactive.")
  })

  observeEvent(input$activate_post_id, {
    sid <- input$activate_post_id
    if (is.null(sid) || is.na(sid)) return()
    row_to_activate <- rv$inactive_posts %>% filter(id == sid)
    req(nrow(row_to_activate) == 1)
    
    # Normal activation for user-inactivated posts
    rv$inactive_posts <- rv$inactive_posts %>% filter(id != sid)
    rv$posts <- bind_rows(rv$posts, row_to_activate)
    set_success("Post reactivated.")
  })

  observeEvent(input$add_or_update, {
    rv$form_error_text <- NULL
    rv$export_error_text <- NULL
    rv$pending_post_row <- NULL

    is_editing <- !is.na(rv$editing_id)

    # When editing, always use draft RVs (synchronously set) instead of async form inputs.
    # Form inputs lag behind because reset_form() + updateXxx() are async; drafts are set synchronously.
    val_post_name   <- if (is_editing) rv$post_name_draft          else input$post_name
    val_center      <- if (is_editing) rv$center_draft             else trimws(input$center %||% "")
    val_category    <- if (is_editing) rv$category_draft           else input$category
    val_app_status  <- if (is_editing) rv$application_status_draft else (input$application_status %||% "")
    val_start_date  <- if (is_editing) rv$start_date_draft         else {
      if (!is.null(input$post_date_range) && length(input$post_date_range) == 2) as.Date(input$post_date_range[1]) else as.Date(NA)
    }
    val_end_date    <- if (is_editing) rv$end_date_draft           else {
      if (!is.null(input$post_date_range) && length(input$post_date_range) == 2) as.Date(input$post_date_range[2]) else as.Date(NA)
    }

    # Keep range_start/end for backwards-compat use when adding (new posts)
    range_start <- val_start_date
    range_end   <- val_end_date

    missing_fields <- c()
    if (is.null(val_post_name)  || !nzchar(trimws(val_post_name)))  missing_fields <- c(missing_fields, "Post name")
    if (is.null(val_center)     || !nzchar(trimws(val_center)))     missing_fields <- c(missing_fields, "Site")
    if (is.null(val_category)   || !nzchar(trimws(val_category)))   missing_fields <- c(missing_fields, "Category")
    if (is.null(val_app_status) || !nzchar(trimws(val_app_status))) missing_fields <- c(missing_fields, "Application status")
    if (is.na(range_start) || is.na(range_end)) missing_fields <- c(missing_fields, "Date range")

    if (length(missing_fields) > 0) {
      rv$form_error_text <- paste("Required fields missing:", paste(missing_fields, collapse = ", "))
      return()
    }

    if (range_end < range_start) {
      rv$form_error_text <- "Date range end must be on or after start."
      return()
    }

    vec <- numeric()
    if (isTRUE(if (is_editing) rv$value_mode_draft == "variable" else input$value_mode == "variable")) {
      n_vals <- required_value_count()
      vec <- map_dbl(seq_len(n_vals), function(i) {
        as.numeric(input[[paste0("var_value_", i)]])
      })
    }

    new_row <- tibble(
      id = if (is_editing) rv$editing_id else rv$next_id,
      center = if (is_editing) val_center else trimws(input$center),
      post_name = if (is_editing) val_post_name else input$post_name,
      category = if (is_editing) val_category else input$category,
      start_date = if (is_editing) val_start_date else range_start,
      end_date = if (is_editing) val_end_date else range_end,
      fte = if (is_editing) rv$fte_draft else suppressWarnings(as.numeric(input$fte)),
      value_mode = if (is_editing) rv$value_mode_draft else input$value_mode,
      value_unit = if (is_editing) rv$value_unit_draft else input$value_unit,
      constant_expr = rv$constant_expr_draft,
      function_expr = ifelse(is.null(input$function_expr) || !nzchar(input$function_expr), rv$function_expr_draft, input$function_expr),
      value_vector = list(vec),
      sum_multiplier = if (!is.null(input$sum_multiplier) && nzchar(input$sum_multiplier)) input$sum_multiplier else (rv$sum_multiplier_draft %||% "1"),
      sum_sites = if (!is.null(input$sum_sites) && length(input$sum_sites) > 0) paste(input$sum_sites, collapse = "||") else if (length(rv$sum_sites_draft) > 0) paste(rv$sum_sites_draft, collapse = "||") else "",
      sum_statuses = if (!is.null(input$sum_statuses) && length(input$sum_statuses) > 0) paste(input$sum_statuses, collapse = "||") else if (length(rv$sum_statuses_draft) > 0) paste(rv$sum_statuses_draft, collapse = "||") else "",
      sum_posts = if (!is.null(input$sum_posts) && length(input$sum_posts) > 0) paste(input$sum_posts, collapse = "||") else if (length(rv$sum_posts_draft) > 0) paste(rv$sum_posts_draft, collapse = "||") else "",
      note = if (is_editing) rv$note_draft else input$note,
      needs_amendment = FALSE,
      application_status = if (is_editing) val_app_status else (if (!is.null(input$application_status)) input$application_status else "Applied for"),
      import_issues = ""
    )

    dup <- rv$posts %>%
      filter(
        tolower(center) == tolower(new_row$center),
        tolower(post_name) == tolower(new_row$post_name),
        id != new_row$id
      )

    if (nrow(dup) > 0) {
      rv$form_error_text <- "A post with the same name already exists in this site. Please rename it or edit the existing post."
      return()
    }

    category_row <- rv$category_registry %>%
      filter(!is_deleted, name == val_category) %>%
      slice(1)
    if (nrow(category_row) != 1) {
      rv$form_error_text <- "Selected category is unavailable. Please choose another category."
      return()
    }

    site_row <- rv$site_registry %>%
      filter(!is_deleted, name == val_center) %>%
      slice(1)
    if (nrow(site_row) != 1) {
      rv$form_error_text <- "Selected site is unavailable. Please choose another site."
      return()
    }

    valid <- tryCatch({
      candidate_posts <- rv$posts %>% filter(id != new_row$id) %>% bind_rows(new_row)
      row_months <- month_sequence(new_row$start_date, new_row$end_date)
      row_fte_monthly_total <- map_dbl(row_months, function(m) {
        sum(candidate_posts$fte[candidate_posts$start_date <= m & candidate_posts$end_date >= m], na.rm = TRUE)
      })
      resolved_for_validation <- resolve_post_values(
        new_row,
        salaries_lookup = make_salary_lookup(rv$salaries),
        inflation_pct = input$inflation_pct,
        fte_monthly_total = row_fte_monthly_total,
        salaries_tbl = rv$salaries,
        budget_start = as.Date(input$budget_start),
        all_posts_tbl = candidate_posts
      )
      category_msg <- validate_category_rule(
        resolved_values = resolved_for_validation,
        category_row = category_row,
        budget_start = as.Date(input$budget_start)
      )
      if (!is.null(category_msg)) category_msg else TRUE
    }, error = function(e) {
      rv$form_error_text <- e$message
      FALSE
    })

    commit_post <- function(row_to_save) {
      if (is.na(rv$editing_id)) {
        rv$posts <- bind_rows(rv$posts, row_to_save)
        rv$next_id <- rv$next_id + 1L
        set_success(paste("Post added:", row_to_save$post_name, "in", row_to_save$center))
      } else {
        rv$posts <- rv$posts %>%
          filter(id != rv$editing_id) %>%
          bind_rows(row_to_save)
        # Remove from inactive_posts if editing a post that was there
        rv$inactive_posts <- rv$inactive_posts %>% filter(id != rv$editing_id)
        # Clear any import issue tracking for this post since it's been re-edited
        rv$post_import_issues <- rv$post_import_issues %>% filter(id != rv$editing_id)
        set_success(paste("Post updated:", row_to_save$post_name, "in", row_to_save$center))
      }

      rv$posts <- flag_posts(rv$posts, input$budget_start, input$budget_end, preserve_existing = TRUE, post_import_issues = rv$post_import_issues)
      
      # Re-validate the saved post against current registries and check if it still needs amendment
      saved_post <- rv$posts %>% filter(id == row_to_save$id)
      if (nrow(saved_post) > 0) {
        # Re-check for import issues
        revalidation <- check_import_post_issues(
          post_row = saved_post[1, ],
          existing_posts = rv$posts %>% filter(id != row_to_save$id),
          salaries_lookup = make_salary_lookup(rv$salaries),
          inflation_pct = input$inflation_pct,
          salaries_tbl = rv$salaries,
          site_registry = rv$site_registry,
          category_registry = rv$category_registry
        )
        
        if (revalidation$has_issues) {
          # Still has issues - add to post_import_issues
          rv$post_import_issues <- rv$post_import_issues %>%
            filter(id != row_to_save$id) %>%
            bind_rows(tibble(id = row_to_save$id, import_issues = revalidation$issues_text))
          rv$posts <- rv$posts %>%
            mutate(needs_amendment = if_else(id == row_to_save$id, TRUE, needs_amendment))
        } else {
          # No import issues - but keep needs_amendment if it was set by flag_posts() due to date range
          rv$post_import_issues <- rv$post_import_issues %>% filter(id != row_to_save$id)
        }
      }
      
      reset_form()
    }

    if (is.character(valid)) {
      rv$pending_post_row <- new_row
      showModal(modalDialog(
        title = "Category restriction exceeded",
        tags$p(valid),
        tags$p("Do you want to return to the form or override this restriction for this post?"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Return"),
          actionButton("confirm_override_category_rule", "Override and Save", class = "btn-success")
        )
      ))
      return()
    }

    if (!isTRUE(valid)) return()

    commit_post(new_row)
  })

  observeEvent(input$confirm_override_category_rule, {
    req(!is.null(rv$pending_post_row))
    row_to_save <- rv$pending_post_row
    removeModal()

    if (is.na(rv$editing_id)) {
      rv$posts <- bind_rows(rv$posts, row_to_save)
      rv$next_id <- rv$next_id + 1L
      set_success(paste("Post added with override:", row_to_save$post_name, "in", row_to_save$center))
    } else {
      rv$posts <- rv$posts %>%
        filter(id != rv$editing_id) %>%
        bind_rows(row_to_save)
      # Clear any import issue tracking for this post
      rv$post_import_issues <- rv$post_import_issues %>% filter(id != rv$editing_id)
      set_success(paste("Post updated with override:", row_to_save$post_name, "in", row_to_save$center))
    }

    rv$posts <- flag_posts(rv$posts, input$budget_start, input$budget_end, preserve_existing = TRUE, post_import_issues = rv$post_import_issues)
    rv$pending_post_row <- NULL
    reset_form()
  })

  output$amendment_status <- renderUI({
    if (nrow(rv$posts) == 0) return(NULL)

    n_flagged <- sum(rv$posts$needs_amendment, na.rm = TRUE)
    edit_hint <- if (identical(input$display_form, "wide")) {
      "Click on a column to make edits to a post."
    } else {
      "Click on a row to make edits to a post."
    }

    if (n_flagged == 0) {
      tags$span(style = "font-weight: 600;", edit_hint)
    } else {
      amend_text <- if (n_flagged == 1) {
        "1 post requires amendment before finalisation."
      } else {
        paste(n_flagged, "posts require amendment before finalisation.")
      }
      tags$span(style = "color: #a94442; font-weight: 600;", paste(amend_text, edit_hint, "Note: the input(s) requiring amendment will be marked with '!!!'"))
    }
  })

  # Visualization data: aggregate by month, site, and category for stacked area chart
  visualization_data <- reactive({
    long_df <- build_long_budget(rv$posts, input$budget_start, input$budget_end, salaries_lookup = make_salary_lookup(rv$salaries), inflation_pct = input$inflation_pct, salaries_tbl = rv$salaries)
    
    if (!nrow(long_df)) {
      return(tibble(
        month = as.Date(character()),
        center = character(),
        category = character(),
        value = numeric(),
        facet = character()
      ))
    }

    # Apply same filters as filtered_posts() to respect table viewer filtering
    if (!is.null(input$filter_month_range) && all(!is.na(input$filter_month_range))) {
      mstart <- as.Date(input$filter_month_range[1])
      mend <- as.Date(input$filter_month_range[2])
      long_df <- long_df %>% filter(month >= mstart, month <= mend)
    }

    if (!is.null(input$filter_center) && length(input$filter_center) > 0) {
      long_df <- long_df %>% filter(center %in% input$filter_center)
    }

    if (!is.null(input$filter_category) && length(input$filter_category) > 0) {
      long_df <- long_df %>% filter(category %in% input$filter_category)
    }

    if (!is.null(input$filter_status) && length(input$filter_status) > 0) {
      status_ids <- rv$posts %>% filter(application_status %in% input$filter_status) %>% pull(id)
      long_df <- long_df %>% filter(id %in% status_ids)
    }

    if (!nrow(long_df)) {
      return(tibble(
        month = as.Date(character()),
        center = character(),
        category = character(),
        value = numeric(),
        facet = character()
      ))
    }

    # Aggregate by month, site (center), and category (monthly values)
    agg_monthly <- long_df %>%
      mutate(month = as.Date(paste0(format(month, "%Y-%m"), "-01"))) %>%
      group_by(month, center, category) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
    
    # Ensure all month-site-category combinations exist (fill missing with 0)
    all_months <- unique(agg_monthly$month)
    all_centers <- unique(agg_monthly$center)
    all_categories <- unique(agg_monthly$category)
    
    complete_grid <- expand_grid(
      month = all_months,
      center = all_centers,
      category = all_categories
    )
    
    agg_data <- complete_grid %>%
      left_join(agg_monthly, by = c("month", "center", "category")) %>%
      replace_na(list(value = 0)) %>%
      arrange(center, category, month) %>%
      group_by(center, category) %>%
      mutate(value = cumsum(value)) %>%
      ungroup() %>%
      mutate(facet = center)

    # Create "All sites" aggregate: sum monthly values across sites, then cumulative sum once.
    all_sites_data <- complete_grid %>%
      left_join(agg_monthly, by = c("month", "center", "category")) %>%
      replace_na(list(value = 0)) %>%
      group_by(month, category) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(category, month) %>%
      group_by(category) %>%
      mutate(value = cumsum(value)) %>%
      ungroup() %>%
      mutate(center = "ALL_SITES_TOTAL", facet = "All sites")

    # Combine both
    bind_rows(agg_data, all_sites_data) %>%
      mutate(facet = factor(facet, levels = c(sort(unique(agg_data$center)), "All sites")))
  })

  output$visualization_plot <- renderPlotly({
    viz_data <- visualization_data()
    
    if (!nrow(viz_data)) {
      p <- ggplot() +
        geom_blank() +
        labs(title = "No data to display. Add posts to view budget visualization.") +
        theme_minimal()
      return(ggplotly(p, tooltip = "none"))
    }

    # Color palette for categories - using RColorBrewer Set2
    # Order categories by their total cumulative sum (largest at bottom)
    # Use the "All sites" facet to get true totals, or sum across sites if it doesn't exist
    all_sites_data <- viz_data %>% filter(facet == "All sites")
    if (nrow(all_sites_data) > 0) {
      # Use all_sites aggregate - get final cumulative value for each category
      cat_totals <- all_sites_data %>%
        group_by(category) %>%
        summarise(total_value = max(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total_value))
    } else {
      # Fall back to summing across all data
      cat_totals <- viz_data %>%
        group_by(category) %>%
        summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total_value))
    }
    unique_cats <- cat_totals$category
    n_cats <- length(unique_cats)
    # Set2 has colors: #66C2A5, #FC8D62, #8DA0CB, #E78AC3, #A6D854, #FFD92F, #E5C494, #B3B3B3
    set2_colors <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")
    color_palette <- if (n_cats <= 8) {
      setNames(set2_colors[1:n_cats], unique_cats)
    } else {
      setNames(colorRampPalette(set2_colors)(n_cats), unique_cats)
    }

    facets <- levels(viz_data$facet)

    # Show each category in the legend exactly once, in the first facet where it appears with value > 0.
    cat_legend_facet <- setNames(rep(NA_character_, length(unique_cats)), unique_cats)
    for (cat_name in unique_cats) {
      for (f in facets) {
        has_positive <- viz_data %>%
          filter(facet == f, category == cat_name) %>%
          summarise(any_pos = any(value > 0, na.rm = TRUE)) %>%
          pull(any_pos)
        if (isTRUE(has_positive)) {
          cat_legend_facet[[cat_name]] <- f
          break
        }
      }
    }
    
    # Calculate max stacked total across non-"All sites" facets for aligned y-axes.
    # Use per-month sums across categories (stack height), not max of individual categories.
    non_all_data <- viz_data %>% filter(facet != "All sites")
    y_max_non_all <- if (nrow(non_all_data) > 0) {
      non_all_data %>%
        group_by(facet, month) %>%
        summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        summarise(max_total = max(total_value, na.rm = TRUE)) %>%
        pull(max_total) * 1.05
    } else {
      0
    }
    
    plots <- lapply(facets, function(f) {
      facet_data <- viz_data %>% filter(facet == f)
      if (!nrow(facet_data)) {
        return(plot_ly() %>% add_trace(y = numeric(), name = f))
      }
      
      # Only include categories present in this facet, in the ordered sequence
      present_cats <- unique_cats[unique_cats %in% facet_data$category]
      
      p <- plot_ly()
      for (cat_name in present_cats) {
        cat_data <- facet_data %>%
          filter(category == cat_name) %>%
          arrange(month)

        # Trim leading zero cumulative values so hover only lists categories that are present (> 0).
        first_positive_idx <- which(cat_data$value > 0)[1]
        if (is.na(first_positive_idx)) next
        cat_data <- cat_data[first_positive_idx:nrow(cat_data), , drop = FALSE]

        show_legend_trace <- identical(f, cat_legend_facet[[cat_name]])
        
        p <- p %>%
          add_trace(
            x = cat_data$month,
            y = cat_data$value,
            name = cat_name,
            type = "scatter",
            mode = "lines",
            fill = "tonexty",
            fillcolor = color_palette[cat_name],
            line = list(width = 0),
            stackgroup = "one",
            legendgroup = cat_name,
            showlegend = show_legend_trace,
            hovertemplate = paste0("%{x|%Y-%m}<br>", cat_name, "<br>%{y:,}<extra></extra>")
          )
      }
      
      # Set y-axis range: aligned for non-All sites, free for All sites
      y_axis_config <- list(title = if (f == facets[1]) "Cumulative amount" else "")
      if (f != "All sites" && y_max_non_all > 0) {
        y_axis_config$range <- c(0, y_max_non_all)
      }
      
      x_start <- format(as.Date(input$budget_start), "%Y-%m-%d")
      x_end   <- format(as.Date(input$budget_end),   "%Y-%m-%d")
      period_months <- as.integer(
        difftime(as.Date(input$budget_end), as.Date(input$budget_start), units = "days") / 30
      )
      dtick_val <- if (period_months <= 24) "M3" else if (period_months <= 60) "M6" else "M12"

      p <- p %>%
        layout(
          xaxis = list(
            title = "",
            tickformat = "%Y-%m",
            tickangle = 45,
            range = c(x_start, x_end),
            dtick = dtick_val
          ),
          yaxis = y_axis_config,
          hovermode = "x unified",
          showlegend = FALSE
        )
      
      p
    })
    
    n_facets <- length(facets)
    facet_annotations <- lapply(seq_along(facets), function(i) {
      list(
        text = facets[[i]],
        x = (i - 0.5) / n_facets,
        y = 1.06,
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 12)
      )
    })

    n_f  <- length(plots)
    gap  <- 0.05
    w_each <- (1 - gap * (n_f - 1)) / n_f
    xaxis_names <- c("xaxis", if (n_f > 1) paste0("xaxis", 2:n_f) else character(0))
    xaxis_domains <- setNames(
      lapply(seq_len(n_f), function(i) {
        start <- (i - 1) * (w_each + gap)
        list(domain = c(start, start + w_each))
      }),
      xaxis_names
    )

    base_fig <- subplot(plots, nrows = 1, shareY = FALSE, margin = gap) %>%
      config(toImageButtonOptions = list(format = "png", filename = paste0("budget_visualization_", format(Sys.Date(), "%Y%m%d"))))

    base_layout <- list(
      title = list(text = "Cumulative budget over time by category and site", x = 0.5, xanchor = "center"),
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.25, yanchor = "top"),
      hovermode = "x unified",
      margin = list(t = 110, b = 120),
      annotations = facet_annotations
    )

    do.call(layout, c(list(base_fig), base_layout, xaxis_domains))
  })

  observeEvent(input$save_pdf_btn, {
    fname <- paste0(workbook_name_effective(), "_budget_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    session$sendCustomMessage("save-pdf", list(filename = fname))
  })


  output$export_control <- renderUI({
    n_flagged <- sum(rv$posts$needs_amendment, na.rm = TRUE)
    export_name <- ifelse(nzchar(workbook_name_effective()), workbook_name_effective(), "Budget")
    if (n_flagged > 0) {
      actionButton("export_xlsx", "Export blocked: amend posts first", class = "btn-warning", width = "100%")
    } else {
      actionButton("export_xlsx", paste0("Export ", export_name, ".xlsx"), class = "btn-primary", width = "100%")
    }
  })

  observeEvent(input$show_formula_help, {
    showModal(modalDialog(
      title = "Formula variables and helpers",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$p(tags$b("Note:"), " entering a single value repeats that value for every period."),
      tags$p("Available in amount formulas (Formula mode):"),
      tags$ul(
        tags$li(tags$b("n"), " - required vector length for the selected input period (months or years)"),
        tags$li(tags$b("FTE"), " - FTE for this post, as input by user"),
        tags$li(tags$b("fte(site)"), " - function returning total number of FTEs within each chosen period (month, year). By default sums across all positions within the current post's site, but this can be changed with the site argument; e.g., fte(site = 'Main'), fte(site = all_sites)."),
        tags$li(tags$b("this_site"), " - string, the current post's site name"),
        tags$li(tags$b("all_sites"), " - vector of all site names in the budget"),
        tags$li(tags$b("inflation_factors"), " - vector of inflation multipliers for each period (month or year, depending on post's period type)"),
        tags$li(tags$b("apply_inflation(base_value)"), " - applies calendar-year inflation to a repeated value, matching the post's period type (returns month or year length vector)"),
        tags$li(tags$b("inflation_pct"), " - yearly inflation percentage"),
      ),
      tags$p("Salary references:"),
      tags$ul(
        tags$li("Each salary identifier is available as an object, e.g. phd_student$total_m"),
        tags$li("Identifiers are generated from salary names and made unique"),
        tags$li("Monthly selectors: base_m, pension_m, own_pension_m, holiday_base_m, holiday_m, total_plus_holiday_m, total_m"),
        tags$li("Yearly selectors: total_plus_holiday_y, total_y"),
        tags$li(tags$b("total_m / total_y"), " — the final effective salary (holiday-absence deducted if 'Subtract holiday' is on)"),
        tags$li(tags$b("total_plus_holiday_m / total_plus_holiday_y"), " — salary including holiday allowance but before holiday-absence deduction")
      ),
      tags$p("Examples:"),
      tags$pre("apply_inflation_month(phd_student$total_m)\nresearch_year$total_plus_holiday_m\nclinical_researcher$total_y")
    ))
  })

  build_export_sheet <- function(period_choice) {
    if (!nrow(rv$posts)) {
      return(tibble(
        Period = character(),
        `Post name` = character(),
        Site = character(),
        Category = character(),
        Note = character(),
        `Start Date` = character(),
        `End date` = character(),
        FTE = numeric(),
        Amount = numeric()
      ))
    }

    long <- build_long_budget(
      rv$posts,
      input$budget_start,
      input$budget_end,
      salaries_lookup = make_salary_lookup(rv$salaries),
      inflation_pct = input$inflation_pct,
      salaries_tbl = rv$salaries
    )
    if (!nrow(long)) {
      return(tibble(
        Period = character(),
        `Post name` = character(),
        Site = character(),
        Category = character(),
        Note = character(),
        `Start Date` = character(),
        `End date` = character(),
        FTE = numeric(),
        Amount = numeric()
      ))
    }

    meta <- rv$posts %>%
      select(id, start_date, end_date, note)

    out <- long %>%
      left_join(meta, by = "id") %>%
      mutate(
        Period = case_when(
          period_choice == "month" ~ period_month,
          period_choice == "calendar_year" ~ as.character(calendar_year),
          TRUE ~ paste0("Year ", project_year)
        ),
        `Post name` = post_name,
        Site = center,
        Category = category,
        Note = note,
        `Start Date` = start_date,
        `End date` = end_date,
        FTE = fte,
        Amount = value
      ) %>%
      select(Period, `Post name`, Site, Category, Note, `Start Date`, `End date`, FTE, Amount)

    squash <- input$squash_dims
    if (is.null(squash)) squash <- character(0)

    if ("Period" %in% squash) out$Period <- "All periods"
    if ("Post name" %in% squash) out$`Post name` <- "All posts"
    if ("Site" %in% squash) out$Site <- "All sites"
    if ("Category" %in% squash) out$Category <- "All categories"
    if ("Post name" %in% squash) out$Note <- "Mixed notes"

    group_cols <- c()
    if (!("Period" %in% squash)) group_cols <- c(group_cols, "Period")
    if (!("Post name" %in% squash)) group_cols <- c(group_cols, "Post name")
    if (!("Site" %in% squash)) group_cols <- c(group_cols, "Site")
    if (!("Category" %in% squash)) group_cols <- c(group_cols, "Category")
    if (length(group_cols) == 0) group_cols <- "Period"

    out <- out %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(
        Period = if ("Period" %in% group_cols) first(Period) else "All periods",
        `Post name` = if ("Post name" %in% group_cols) first(`Post name`) else "All posts",
        Site = if ("Site" %in% group_cols) first(Site) else "All sites",
        Category = if ("Category" %in% group_cols) first(Category) else "All categories",
        Note = if (n_distinct(Note) == 1) first(Note) else "Mixed notes",
        `Start Date` = if (n_distinct(`Start Date`) == 1) first(`Start Date`) else as.Date(NA),
        `End date` = if (n_distinct(`End date`) == 1) first(`End date`) else as.Date(NA),
        FTE = if (n_distinct(FTE) == 1) first(FTE) else NA_real_,
        Amount = sum(Amount),
        .groups = "drop"
      ) %>%
      mutate(
        `Start Date` = as.character(`Start Date`),
        `End date` = as.character(`End date`)
      ) %>%
      arrange(Site, `Post name`, Period)

    total_row <- tibble(
      Period = "TOTAL",
      `Post name` = "",
      Site = "",
      Category = "",
      Note = "",
      `Start Date` = NA_character_,
      `End date` = NA_character_,
      FTE = NA_real_,
      Amount = sum(out$Amount, na.rm = TRUE)
    )
    bind_rows(out, total_row)
  }

  build_export_wide_sheet <- function(period_choice) {
    long_df <- build_export_sheet(period_choice)
    if (!nrow(long_df)) return(tibble(Period = character()))

    # Strip the TOTAL summary row before pivoting – build_wide_from_long adds
    # Site=="" / Post name=="" which turns into spurious empty-site columns.
    data_rows <- dplyr::filter(long_df, Period != "TOTAL")
    if (!nrow(data_rows)) return(tibble(Period = character()))

    # Pivot each post to its own column.
    wide <- tidyr::pivot_wider(
      dplyr::select(data_rows, Period, Site, `Post name`, Amount),
      names_from  = c(Site, `Post name`),
      values_from = Amount,
      names_sep   = " > ",
      values_fn   = sum,
      values_fill = NA_real_
    )

    # Add a site-total column immediately after each site's post columns.
    ordered_cols <- "Period"
    site_total_cols <- character(0)
    for (site in unique(data_rows$Site)) {
      prefix  <- paste0(site, " > ")
      pcols   <- names(wide)[startsWith(names(wide), prefix)]
      if (length(pcols) == 0) next
      tcol <- paste0(site, " > Site total")
      wide[[tcol]] <- rowSums(as.matrix(wide[, pcols, drop = FALSE]), na.rm = TRUE)
      site_total_cols <- c(site_total_cols, tcol)
      ordered_cols <- c(ordered_cols, pcols, tcol)
    }

    grand_spacer <- strrep(" ", length(unique(data_rows$Site)) + 1)
    while (grand_spacer %in% names(wide)) {
      grand_spacer <- paste0(grand_spacer, " ")
    }
    wide[[grand_spacer]] <- ""
    ordered_cols <- c(ordered_cols, grand_spacer)

    grand_total_col <- "All sites > Total"
    if (length(site_total_cols) > 0) {
      wide[[grand_total_col]] <- rowSums(as.matrix(wide[, site_total_cols, drop = FALSE]), na.rm = TRUE)
    } else {
      wide[[grand_total_col]] <- NA_real_
    }
    ordered_cols <- c(ordered_cols, grand_total_col)

    wide <- wide[, ordered_cols, drop = FALSE]

    # Append a grand TOTAL row.
    num_cols <- names(wide)[vapply(wide, is.numeric, logical(1))]
    total_row <- as.list(rep(NA, ncol(wide)))
    names(total_row) <- names(wide)
    total_row$Period <- "TOTAL"
    for (col in num_cols) {
      total_row[[col]] <- sum(wide[[col]], na.rm = TRUE)
    }
    for (col in names(wide)[vapply(wide, is.character, logical(1))]) {
      if (grepl("^\\s+$", col)) total_row[[col]] <- ""
    }
    dplyr::bind_rows(wide, tibble::as_tibble(total_row))
  }

  to_js_sheet <- function(df, has_total_row = FALSE) {
    list(
      # as.list() forces a JSON array even when there is only one column name,
      # preventing Shiny from serialising it as a bare scalar string.
      columns = as.list(names(df)),
      rows = if (!nrow(df)) {
        list()
      } else {
        lapply(seq_len(nrow(df)), function(i) unname(as.list(df[i, , drop = FALSE])))
      },
      meta = list(hasTotalRow = has_total_row)
    )
  }

  do_export_now <- function() {
    export_base_name <- workbook_name_effective()
    name_error <- validate_workbook_name(export_base_name)
    if (!is.null(name_error)) {
      rv$workbook_name_error <- name_error
      rv$export_error_text <- "Workbook name must be valid before export."
      return()
    }
    rv$workbook_name <- export_base_name
    rv$workbook_name_error <- NULL

    # Issue #3: Use workbook name for filename
    export_filename <- paste0(export_base_name, ".xlsx")
    
    # Issue #4: Generate salary identifiers for existing salaries
    salary_identifiers <- character()
    if (nrow(rv$salaries) > 0) {
      for (i in seq_len(nrow(rv$salaries))) {
        if (is.na(rv$salaries$identifier[i]) || !nzchar(rv$salaries$identifier[i])) {
          salary_identifiers[i] <- generate_salary_identifier(rv$salaries$name[i], salary_identifiers)
        } else {
          salary_identifiers[i] <- rv$salaries$identifier[i]
        }
      }
      rv$salaries$identifier <- salary_identifiers
    }

    sheets <- list(
      by_project_year_wide  = to_js_sheet(build_export_wide_sheet("project_year"),  has_total_row = TRUE),
      by_project_year_long  = to_js_sheet(build_export_sheet("project_year"),       has_total_row = TRUE),
      by_calendar_year_wide = to_js_sheet(build_export_wide_sheet("calendar_year"), has_total_row = TRUE),
      by_calendar_year_long = to_js_sheet(build_export_sheet("calendar_year"),      has_total_row = TRUE),
      by_month_wide         = to_js_sheet(build_export_wide_sheet("month"),         has_total_row = TRUE),
      by_month_long         = to_js_sheet(build_export_sheet("month"),              has_total_row = TRUE),
      posts                 = to_js_sheet(serialize_posts(rv$posts)),
      inactive_posts        = to_js_sheet(serialize_inactive_posts(rv$inactive_posts)),
      salaries              = to_js_sheet(serialize_salaries(rv$salaries)),
      sites                 = to_js_sheet(serialize_sites(rv$site_registry)),
      categories            = to_js_sheet(serialize_categories(rv$category_registry)),
      templates             = to_js_sheet(serialize_templates(rv$template_registry)),
      meta                  = to_js_sheet(tibble(
        key = c("budget_start", "budget_end", "inflation_pct", "workbook_name", "schema_version"),
        value = c(as.character(input$budget_start), as.character(input$budget_end), as.character(input$inflation_pct), export_base_name, "2.0")
      ))
    )

    session$sendCustomMessage("download-xlsx", list(
      filename = export_filename,
      sheets = sheets
    ))

    set_success(paste("Export started:", export_filename))
  }

  observeEvent(input$export_confirm_yes, {
    removeModal()
    do_export_now()
  })

  observeEvent(input$export_xlsx, {
    rv$export_error_text <- NULL

    n_flagged <- sum(rv$posts$needs_amendment, na.rm = TRUE)
    if (n_flagged > 0) {
      rv$export_error_text <- paste(n_flagged, "post(s) must be amended before export.")
      return()
    }
    
    # Check for posts with import issues
    n_import_errors <- sum(!is.na(rv$inactive_posts$import_issues) & nzchar(rv$inactive_posts$import_issues), na.rm = TRUE)
    if (n_import_errors > 0) {
      showModal(modalDialog(
        title = "Confirm Export",
        tagList(
          tags$p(tags$strong(paste(n_import_errors, "post(s) have import errors and are inactive."))),
          tags$p("These posts will not be included in the export. Do you want to continue?")
        ),
        footer = tagList(
          actionButton("export_confirm_yes", "Continue Export", class = "btn-primary"),
          modalButton("Cancel")
        )
      ))
      return()
    }

    do_export_now()
  })

  observeEvent(input$import_file, {
    req(input$import_file)

    if (!requireNamespace("readxl", quietly = TRUE)) {
      show_error_modal("Package 'readxl' is required for XLSX import.")
      return()
    }

    path <- input$import_file$datapath

    ok <- tryCatch({
      sheets <- readxl::excel_sheets(path)
      required_sheets <- c("posts", "meta")

      if (!all(required_sheets %in% sheets)) {
        stop("Import file must contain 'posts' and 'meta' sheets.")
      }

      imported_posts <- readxl::read_excel(path, sheet = "posts", col_types = "text")
      imported_meta <- readxl::read_excel(path, sheet = "meta", col_types = "text")

      if (!all(c("key", "value") %in% names(imported_meta))) {
        stop("Meta sheet must have columns 'key' and 'value'.")
      }

      start_val <- imported_meta$value[imported_meta$key == "budget_start"]
      end_val <- imported_meta$value[imported_meta$key == "budget_end"]
      inflation_val <- imported_meta$value[imported_meta$key == "inflation_pct"]
      workbook_name_val <- imported_meta$value[imported_meta$key == "workbook_name"]

      if (length(start_val) != 1 || length(end_val) != 1) {
        stop("Meta sheet must contain one budget_start and one budget_end.")
      }

      posts_parsed <- parse_posts(as_tibble(imported_posts))

      salaries_parsed <- make_empty_salaries()
      if ("salaries" %in% sheets) {
        imported_salaries <- readxl::read_excel(path, sheet = "salaries", col_types = "text")
        salaries_parsed <- parse_salaries(as_tibble(imported_salaries))
      }

      # Issue #3: Restore workbook name
      restored_workbook_name <- workbook_name_from_filename(input$import_file$name)
      if (!nzchar(restored_workbook_name) && length(workbook_name_val) == 1 && !is.na(workbook_name_val) && nzchar(workbook_name_val)) {
        restored_workbook_name <- workbook_name_val
      }

      # Issue #2: Restore categories and templates
      site_registry_restored <- make_default_sites()
      if ("sites" %in% sheets) {
        imported_sites <- readxl::read_excel(path, sheet = "sites", col_types = "text")
        site_registry_restored <- parse_sites(as_tibble(imported_sites))
      }

      category_registry_restored <- make_default_categories()
      if ("categories" %in% sheets) {
        imported_categories <- readxl::read_excel(path, sheet = "categories", col_types = "text")
        category_registry_restored <- parse_categories(as_tibble(imported_categories))
      }

      template_registry_restored <- make_default_templates()
      if ("templates" %in% sheets) {
        imported_templates <- readxl::read_excel(path, sheet = "templates", col_types = "text")
        template_registry_restored <- parse_templates(as_tibble(imported_templates))
      }

      updateDateInput(session, "budget_start", value = as.Date(start_val))
      updateDateInput(session, "budget_end", value = as.Date(end_val))
      updateTextInput(session, "workbook_name", value = restored_workbook_name)
      if (length(inflation_val) == 1 && !is.na(suppressWarnings(as.numeric(inflation_val)))) {
        updateNumericInput(session, "inflation_pct", value = as.numeric(inflation_val))
      }

      rv$workbook_name <- restored_workbook_name
      rv$next_id <- ifelse(nrow(posts_parsed), max(posts_parsed$id, na.rm = TRUE) + 1L, 1L)
      
      # Validate imported posts for issues
      posts_with_validation <- map_dfr(seq_len(nrow(posts_parsed)), function(i) {
        post <- posts_parsed[i, ]
        validation <- check_import_post_issues(
          post, 
          posts_parsed[-i, ], 
          make_salary_lookup(salaries_parsed), 
          as.numeric(inflation_val),
          salaries_tbl = salaries_parsed,
          site_registry = site_registry_restored,
          category_registry = category_registry_restored
        )
        post$import_issues <- if (validation$has_issues) validation$issues_text else NA_character_
        post$needs_amendment <- validation$has_issues
        post
      })
      
      # Posts with import issues stay active but marked for amendment
      all_imported_posts <- posts_with_validation %>% select(-import_issues)
      
      # Store import issues in reactive value for display
      rv$post_import_issues <- posts_with_validation %>%
        filter(!is.na(import_issues)) %>%
        select(id, import_issues)
      
      # Import previously inactive posts from the export file (for user-inactivated posts)
      previously_inactive <- make_empty_posts() %>% slice(0)
      if ("inactive_posts" %in% sheets) {
        imported_inactive <- readxl::read_excel(path, sheet = "inactive_posts", col_types = "text")
        if (nrow(imported_inactive) > 0) {
          previously_inactive <- parse_inactive_posts(as_tibble(imported_inactive))
        }
      }
      
      rv$inactive_posts <- previously_inactive
      rv$posts <- all_imported_posts
      rv$posts <- flag_posts(rv$posts, as.Date(start_val), as.Date(end_val), preserve_existing = TRUE, post_import_issues = rv$post_import_issues)
      rv$salaries <- salaries_parsed
      rv$next_salary_id <- ifelse(nrow(salaries_parsed), max(salaries_parsed$id, na.rm = TRUE) + 1L, 1L)
      
      # Issue #2, #4: Restore settings
      rv$site_registry <- site_registry_restored
      max_site_id <- suppressWarnings(max(as.integer(site_registry_restored$id), na.rm = TRUE))
      if (!is.finite(max_site_id)) max_site_id <- 1L
      rv$next_site_id <- as.integer(max_site_id) + 1L

      rv$category_registry <- category_registry_restored
      max_cat_id <- suppressWarnings(max(as.integer(category_registry_restored$id), na.rm = TRUE))
      if (!is.finite(max_cat_id)) max_cat_id <- 18L
      rv$next_category_id <- as.integer(max_cat_id) + 1L
      rv$template_registry <- template_registry_restored
      max_tpl_id <- suppressWarnings(max(as.integer(template_registry_restored$id), na.rm = TRUE))
      if (!is.finite(max_tpl_id)) max_tpl_id <- 4L
      rv$next_template_id <- as.integer(max_tpl_id) + 1L
      
      TRUE
    }, error = function(e) {
      show_error_modal(paste("Import failed:", e$message))
      FALSE
    })

    if (isTRUE(ok)) {
      set_success("Import completed.")
    }
  })
}

shinyApp(ui, server)
