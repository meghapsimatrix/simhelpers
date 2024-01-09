
bundle_sim <- function(
    f_generate,                 # data generation function
    f_analyze,                  # data analysis function
    f_summarize = NULL,         # optional performance summary function
    reps_name = "reps",         # argument name for number of replications
    seed_name = "seed",               # name for seed argument
    summarize_opt_name = "summarize"  # name for optional summarize argument
) {

  # Get component arguments and argument names
  gen_args <- formals(f_generate)
  gen_arg_names <- names(gen_args)
  ana_args <- formals(f_analyze)
  ana_arg_names <- names(ana_args)
  all_names <- union(gen_arg_names, ana_arg_names[-1])
  arg_list <- c(reps_name = reps_name, seed_name = seed_name)
  function_list_str <- "`f_generate` or `f_analyze`"

  if (!is.null(f_summarize)) {
    sum_args <- formals(f_summarize)
    sum_arg_names <- names(sum_args)
    all_names <- union(all_names, sum_arg_names[-1])
    function_list_str <- "`f_generate`, `f_analyze`, or `f_summarize`"
    arg_list <- c(reps_name = reps_name, seed_name = seed_name, summarize_opt_name = summarize_opt_name)
  }

  # Check that all extra arguments of f_analyze have defaults
  if (any(sapply(ana_args[-1], \(x) identical(x, substitute())))) {
    stop("All arguments of `f_analyze` except for the first must have default values.")
  }

  # Check that all extra arguments of f_summarize have defaults
  if (!is.null(f_summarize)) {
    if (any(sapply(sum_args[-1], \(x) identical(x, substitute())))) {
      stop("All arguments of `f_summarize` except for the first must have default values.")
    }
  }

  # Check for argument name conflicts
  for (arg in names(arg_list)) {
    if (arg_list[arg] %in% all_names) {
      stop(paste0(
        arg_list[arg],
        " cannot be used as an argument name in ",
        function_list_str,
        ". Consider renaming arguments or set a different `",
        arg,
        "`."
      ))
    }
  }

  # Check for conflicting default arguments
  common_args <- intersect(gen_arg_names, ana_arg_names[-1])
  if (length(common_args) > 0L) {
    defaults_match <- identical(gen_args[common_args], ana_args[common_args])
    if (!defaults_match) stop("Default arguments of `f_analyze` do not match default arguments of `f_generate`. Consider renaming arguments to avoid ambiguity.")
  }
  if (!is.null(f_summarize)) {
    common_gen_args <- intersect(gen_arg_names, sum_arg_names)
    if (length(common_gen_args) > 0L) {
      defaults_match <- identical(gen_args[common_gen_args], sum_args[common_gen_args])
      if (!defaults_match) stop("Default arguments of `f_summarize` do not match default arguments of `f_generate`. Consider renaming arguments to avoid ambiguity.")
    }
    common_ana_args <- intersect(ana_arg_names[-1], sum_arg_names)
    if (length(common_ana_args) > 0L) {
      defaults_match <- identical(ana_args[common_ana_args], sum_args[common_ana_args])
      if (!defaults_match) stop("Default arguments of `f_summarize` do not match default arguments of `f_analyze`. Consider renaming arguments to avoid ambiguity.")
    }
  }

  # compile all arguments
  full_args <- setNames(alist(1L), reps_name)
  for (i in gen_arg_names) {
    full_args[[i]] <- gen_args[[i]]
  }
  extra_ana_args <- setdiff(ana_arg_names[-1], gen_arg_names)
  for (i in extra_ana_args) {
    full_args[[i]] <- ana_args[[i]]
  }
  if (!is.null(f_summarize)) {
    extra_sum_args <- setdiff(sum_arg_names[-1], names(full_args))
    for (i in extra_sum_args) {
      full_args[[i]] <- sum_args[[i]]
    }
  }
  if (!is.null(seed_name)) {
    full_args[[seed_name]] <- NA_real_
  }
  if (!is.null(f_summarize) && !is.null(summarize_opt_name)) {
    full_args[[summarize_opt_name]] <- TRUE
  }

  # Build iteration function

  bundled_sim <- function(reps, seed, summarize) {
    cl <- match.call()
    gen_cl <- cl[c(1L, match(gen_arg_names, names(cl), 0L))]
    gen_cl[[1L]] <- quote(f_generate)
    ana_cl <- cl[c(1L, match(ana_arg_names[-1], names(cl), 0L))]
    ana_cl[[1L]] <- quote(f_analyze)

    if (!is.na(seed)) {
      set.seed(seed)
    }

    res <- lapply(1:reps, function(x) {
      dat <- eval(gen_cl)
      ana_cl[[ana_arg_names[1]]] <- as.symbol("dat")
      eval(ana_cl)
    })

    res <- do.call(rbind, res)

    if (summarize) {
      sum_cl <- cl[c(1L, match(sum_arg_names[-1], names(cl), 0L))]
      sum_cl[[1L]] <- quote(f_summarize)
      sum_cl[[sum_arg_names[1]]] <- res
      res <- eval(sum_cl)
    }

    return(res)
  }

  formals(bundled_sim) <- full_args

  # adjust reps_name
  body(bundled_sim)[[8]][[3]][[2]][[3]] <- as.symbol(reps_name)

  # adjust summarize_opt_name
  if (is.null(f_summarize)) {
    body(bundled_sim)[[10]] <- NULL
  } else if (is.null(summarize_opt_name)) {
    body(bundled_sim)[[10]] <- body(bundled_sim)[[8]][[3]]
  } else {
    body(bundled_sim)[[10]][[2]] <- as.symbol(summarize_opt_name)
  }

  # adjust seed_name
  if (is.null(seed_name)) {
    body(bundled_sim)[[7]] <- NULL
  } else {
    body(bundled_sim)[[7]][[2]][[2]][[2]] <- as.symbol(seed_name)
    body(bundled_sim)[[7]][[3]][[2]][[2]] <- as.symbol(seed_name)
  }

  return(bundled_sim)
}
