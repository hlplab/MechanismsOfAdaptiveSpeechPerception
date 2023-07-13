require(tidyverse)

mycall <- function(fn, args) suppressWarnings(exec(fn, !!! args))

get_plot_limits <- function(plot) {
  gb = ggplot_build(plot)
  xmin = gb$layout$panel_params[[1]]$x.range[1]
  xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

get_plot_breaks <- function(plot) {
  gb = ggplot_build(plot)
  xbreaks = gb$layout$panel_params[[1]]$x.sec$breaks
  ybreaks = gb$layout$panel_params[[1]]$y.sec$breaks
  list(xbreaks = xbreaks, ybreaks = ybreaks)
}

myGplot.defaults = function(
  type = c("paper","poster","slides")[1],
  base_size = if (type == "paper") { 10 } else if (type == "slides") { 32 } else if (type == "poster") { 36 } else { 10 },
  margin = c("t" = 0.6, "r" = 0.5, "b" = 0.5, "l" = 0.3),
  set_theme = T
)
{
  require(ggplot2)
  t <-
    theme_bw(base_size=base_size) +
    theme(
      axis.text.x = element_text(size=base_size-1, vjust=1),
      axis.text.y = element_text(size=base_size-1, hjust=1, vjust=.5),
      axis.title.x = element_text(size=base_size , vjust=1, hjust=0.5, face = "bold"),
      axis.title.y = element_text(size=base_size, hjust= 0.5, vjust=0.5, face = "bold"),
      strip.text = element_text(size=base_size, color = "white"),
      strip.background = element_rect(fill = "black", color = "black"),
      legend.title = element_text(size=base_size, face = "bold", hjust= 0),
      legend.text = element_text(size=base_size),
      plot.margin = unit(margin, "lines"))

  if (set_theme) theme_set(t) else return(t)
}

# Align a list of plots with shared x-axis but potentially multiple rows, each with different y-axis
# Each column is also assumed to have the same titles, which are then plotted only above the top row.
my_plot_grid <- function(
  plotlist,
  labels.top.row = c(
    # example of atop
    # bquote(atop("Prior" ~ kappa[0 ~","~ c] == .(weak), "     " ~ nu[0 ~","~ c] == .(weak))),
    bquote(~" Prior" ~ { kappa[c*","*0] == nu[c*","*0] } == .(weak)),
    bquote(~" Prior" ~ kappa[c*","*0] == .(weak) ~ ", " ~ nu[c*","*0] == .(strong)),
    bquote(~" Prior" ~ kappa[c*","*0] == .(strong) ~ ", " ~ nu[c*","*0] == .(weak)),
    bquote(~" Prior" ~ { kappa[c*","*0] == nu[c*","*0] } == .(strong))),
  ncols = 4,
  legend.position = "right"
) {
  l <- length(plotlist)

  labels <- map(LETTERS[1:ncols], ~ bquote(~ bold(.(.x)*")")))
  if (!is.null(labels.top.row)) labels <- paste(labels, labels.top.row)
  labels <- c(labels, rep("", ncols + l - length(labels)))

  nrows = ceiling(l / ncols)
  x.axis.label <- get_plot_component(plotlist[[1]], "xlab-l")

  # Remove all x-axis labels
  for (i in 1:length(plotlist)) {
    # Current row and column
    r <- floor((i - 1) / ncols) + 1
    c <- ((i - 1) %% ncols) + 1

    # Scrub redundant info from plots
    plotlist[[i]] <- plotlist[[i]] +
      theme(
        axis.text.y = element_text(angle = 90, hjust = .5),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        plot.title = element_blank(),
        plot.margin = margin(r = if (legend.position == "right") 0 else 8, t = if (legend.position == "top") 15 else 2))
    if ((legend.position %in% c("top", "right") & r == 1 & c == ncols) | (legend.position %in% c("left") & r == 1 & c == 1) | (legend.position %in% c("bottom") & r == nrows & c == ncols)) {
      plotlist[[i]] <- plotlist[[i]] + theme(legend.position = legend.position)
    } else {
      plotlist[[i]] <- plotlist[[i]] + theme(legend.position = "none")
    }
    if (r != nrows) plotlist[[i]] <- plotlist[[i]] + theme(axis.text.x = element_blank(), axis.title.x = element_blank())
    if (i %% ncols != 1) plotlist[[i]] <- plotlist[[i]] + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  }

  p <- plot_grid(
    plotlist = plotlist,
    axis = if (legend.position %in% c("top", "bottom")) "btlr" else "lrtb",
    align = "hv",
    nrow = nrows,
    ncol = ncols)

  p <- p +
    draw_plot_label(
      labels,
      x = 0:(ncols - 1) / ncols,
      y = .99,
      hjust = 0,
      vjust = 1,
      size = 10,
      fontface = "bold",
      parse = T)

  return(p)
}

# Error bars that only goes up
# From https://newbedev.com/error-bars-for-barplot-only-in-one-direction

GeomUperrorbar <- ggproto(
  "GeomUperrorbar", Geom,
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, width = 0.5,
                    alpha = NA),

  draw_key = draw_key_path,

  required_aes = c("x", "y", "ymax"),

  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    transform(data,
              xmin = x - width / 2, xmax = x + width / 2, width = NULL
    )
  },
  draw_panel = function(data, panel_scales, coord, width = NULL) {
    GeomPath$draw_panel(data.frame(
      x = as.vector(rbind(data$xmin, data$xmax, NA, data$x,   data$x)),
      y = as.vector(rbind(data$ymax, data$ymax, NA, data$ymax, data$y)),
      colour = rep(data$colour, each = 5),
      alpha = rep(data$alpha, each = 5),
      size = rep(data$size, each = 5),
      linetype = rep(data$linetype, each = 5),
      group = rep(1:(nrow(data)), each = 5),
      stringsAsFactors = FALSE,
      row.names = 1:(nrow(data) * 5)
    ), panel_scales, coord)
  }
)

geom_uperrorbar <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomUperrorbar,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# From https://stackoverflow.com/questions/13407236/remove-a-layer-from-a-ggplot2-chart
remove_layers <- function(ggplot2_object, geom_type) {
  # Delete layers that match the requested type.
  layers <- lapply(ggplot2_object$layers, function(x) {
    if (class(x$geom)[1] == geom_type) {
      NULL
    } else {
      x
    }
  })
  # Delete the unwanted layers.
  layers <- layers[!sapply(layers, is.null)]
  ggplot2_object$layers <- layers
  ggplot2_object
}

extract_layers <- function(ggplot2_object, geom_type) {
  # Delete layers that match the requested type.
  layers <- lapply(ggplot2_object$layers, function(x) {
    if (class(x$geom)[1] == geom_type) {
      x
    } else {
      NULL
    }
  })

  # Delete the unwanted layers.
  layers[!sapply(layers, is.null)]
}

# https://stackoverflow.com/questions/20249653/insert-layer-underneath-existing-layers-in-ggplot2-object
insert_layers <- function(ggplot2_object, after = 0, alpha = 1, ...) {
  # after  : Position where to insert new layers, relative to existing layers
  #  ...   : additional layers, separated by commas (,) instead of plus sign (+)

  if (after < 0)
    after <- after + length(ggplot2_object$layers)

  l <- list(...)
  l <- map(l, ~ (.$aes_params$alpha <- alpha))

  if (!length(ggplot2_object$layers))
    ggplot2_object$layers <- l
  else
    ggplot2_object$layers <- append(ggplot2_object$layers, l, after)

  return(ggplot2_object)
}

combine_animations_into_gif <- function(a, b, height = 300, width = 500) {
  a_gif <- animate(a, width = width, height = height)
  b_gif <- animate(b, width = width, height = height)

  a_mgif <- image_read(a_gif)
  b_mgif <- image_read(b_gif)

  new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
  for(i in 2:nrow(image_info(a_mgif))){
    combined <- image_append(c(a_mgif[i], b_mgif[i]))
    new_gif <- c(new_gif, combined)
  }

  new_gif
}


# LOAD DATA ---------------------------------------------------------------
get_ChodroffWilson_data <- function(
    database_filename,
    min.n_per_talker_and_stop = 0,
    limits.VOT = c(-Inf, Inf),
    limits.f0 = c(0, Inf),
    max.p_for_multimodality = 1
) {
  require(tidyverse)
  require(magrittr)
  require(diptest)

  d.chodroff_wilson <-
    read_csv(database_filename, show_col_types = FALSE) %>%
    rename(category = stop, VOT = vot, f0 = usef0, Talker = subj, Word = word, Trial = trial, Vowel = vowel) %>%
    mutate(
      category =
        plyr::mapvalues(
          category,
          c("B", "D", "G", "P", "T", "K"),
          c("/b/", "/d/", "/g/", "/p/", "/t/", "/k/")),
      gender = factor(
        plyr::mapvalues(
          gender,
          c("F", "M"),
          c("female", "male")),
        levels = c("male", "female")),
      poa = factor(
        plyr::mapvalues(
          poa,
          c("lab", "cor", "dor"),
          c("/b/-/p/", "/d/-/t/", "/g/-/k/")),
        levels = c("/b/-/p/", "/d/-/t/", "/g/-/k/")),
      voicing = factor(
        ifelse(category %in% c("/b/", "/d/", "/g/"), "yes", "no"),
        levels = c("yes", "no"))) %>%
    mutate(across(c(Talker, Word, gender, category), factor)) %>%
    select(Talker, Word, Trial, Vowel, gender, category, poa, voicing, VOT, f0)

  # Filter VOT and f0 for absolute values to deal with outliers
  d.chodroff_wilson %<>%
    filter(
      between(VOT, min(limits.VOT), max(limits.VOT)),
      between(f0, min(limits.f0), max(limits.f0)))

  # Keep only talkers with at last n.min observations for each stop
  # (this is done both prior to and after the multimodality test in order to avoid low N warnings)
  d.chodroff_wilson %<>%
    group_by(Talker, category) %>%
    mutate(n = length(category)) %>%
    group_by(Talker) %>%
    mutate(n = ifelse(any(is.na(n)), 0, min(n))) %>%
    ungroup() %>%
    filter(n > min.n_per_talker_and_stop)

  # Identify and remove talkers with bimodal f0 distributions
  # (indicating pitch halving/doubling)
  d.chodroff_wilson %<>%
    group_by(Talker) %>%
    mutate(f0_Mel = phonR::normMel(f0)) %>%
    group_by(Talker, category) %>%
    mutate(
      f0_Mel.multimodal = dip.test(f0_Mel)$p.value < max.p_for_multimodality) %>%
    filter(!f0_Mel.multimodal) %>%
    droplevels()

  # Keep only talkers with at last n.min observations for each stop
  d.chodroff_wilson %<>%
    group_by(Talker, category) %>%
    mutate(n = length(category)) %>%
    group_by(Talker) %>%
    mutate(n = ifelse(any(is.na(n)), 0, min(n))) %>%
    ungroup() %>%
    filter(n > min.n_per_talker_and_stop)

  # Get Mel and Semitones, then C-CuRE
  d.chodroff_wilson %<>%
    group_by(Talker) %>%
    mutate(
      f0_semitones = 12 * log(f0 / mean(f0)) / log(2)) %>%
    ungroup()
}

# NORMALIZATION -----------------------------------------------------------
apply_ccure <- function(x, data) {
  require(lme4)

  x - predict(lmer(x ~ 1 + (1 | Talker), data = data), random.only = T)
}

# MAKE IDEAL OBSERVERS ----------------------------------------------------

# Make IO out of MVG
make_stop_VOTf0_ideal_observer <- function(
    m,
    prior = rep(1 / nrow(m), nrow(m)),
    lapse_rate = 0,
    lapse_bias = rep(1 / nrow(m), nrow(m)),
    Sigma_noise = matrix(c(80, 0, 0, 878), ncol = 2, dimnames = list(names(first(m$mu)), names(first(m$mu))))
) {
  message("By default, using noise estimates from Kronrod et al. (2016). Mel noise estimates are taken from their vowel studies.")
  m %>%
    lift_MVG_to_MVG_ideal_observer(
      Sigma_noise = Sigma_noise,
      prior = prior,
      lapse_rate = lapse_rate,
      lapse_bias = lapse_bias)
}

# Capturing prior beliefs of perceiver
make_stop_VOTf0_ideal_adaptor <- function(m, kappa = 3, nu = 3) {
  assert_that(all(nu >= 3))

  m %>%
    rename(
      m = mu,
      S = Sigma) %>%
    mutate(
      kappa = kappa,
      nu = nu,
      S = get_S_from_expected_Sigma(S, nu))
}


# DECISION-MAKING ---------------------------------------------------------
make_psychometric_stat_functions <- function(intercept, slope, lambda, pi, centered_on_x = 0) {
  crossing(intercept, slope, lambda, pi) %>%
    mutate(
      fun = pmap(
        .l = list(intercept, slope, lambda, pi),
        .f = function(intercept, slope, lambda, pi)
          stat_function(
            fun = function(x) lambda * pi + (1 - lambda) * plogis(intercept + slope * (x - centered_on_x) + qlogis(pi)),
            aes(color = factor(lambda), linetype = factor(pi)),
            alpha = 1 / (length(intercept) * length(slope))))) %>%
    pull(fun)
}

# FUNCTIONS FOR CASE STUDIES ----------------------------------------------
add_subjects_to_exposure <- function(
    d,
    n.subject,
    quiet = F
) {
  assert_that(!("Subject" %in% names(d)),
              msg = "This data frame already seems to contain subjects")
  if (!quiet) message(paste("Adding", n.subject, "subjects per exposure condition."))

  d %>%
    group_by(Condition) %>%
    crossing(Subject = factor(1:n.subject)) %>%
    mutate(Subject = paste(Condition, Subject, sep = ".")) %>%
    select(Condition, Subject, Phase, ItemID, Item.Category, Item.Type, x, everything()) %>%
    ungroup()
}

add_subjects_to_test <- function(
    d,
    n.subject,
    quiet = F
) {
  assert_that(!("Subject" %in% names(d)),
              msg = "This data frame already seems to contain subjects")
  if (!quiet) message(paste("Adding", n.subject, "subjects per exposure condition."))

  d %>%
    group_by(Condition) %>%
    crossing(Subject = factor(1:n.subject)) %>%
    mutate(Subject = paste(Condition, Subject, sep = ".")) %>%
    select(Condition, Subject, Phase, ItemID, Item.Category, Item.Type, x, everything()) %>%
    ungroup()
}

add_test_tokens <- function(data, data.test) {
  data %>%
    crossing(data.test %>% distinct(x)) %>%
    nest(x = c(x))
}

add_categorization_functions <- function(data, logit = F) {
  data %>%
    mutate(
      prior.categorization = map(prior, ~ get_categorization_function_from_NIW_ideal_adaptor(.x, logit = logit)),
      posterior.categorization = map(posterior, ~ get_categorization_function_from_NIW_ideal_adaptor(.x, logit = logit)))
}

add_categorization <- function(data) {
  data %>%
    mutate(categorization = map2(
      x,
      posterior,
      ~ suppressWarnings(get_categorization_from_NIW_ideal_adaptor(
        .x$x,
        .y,
        decision_rule = "proportional",
        lapse_treatment = "marginalize",
        noise_treatment = "marginalize")))) %>%
    select(-x) %>%
    unnest(categorization)
}


# (if test is null it won't be added and it's assumed that there already is a nested
# column of test tokens called  x and another column indicating the mapping from x
# to intended categories; this is required only for the normalization updating).
add_test_and_categorize <- function(data, test = NULL) {
  assert_that(is_tibble(data))
  assert_that(
    "posterior" %in% names(data),
    msg = "data must contain a column posterior that is a nested ideal adaptor.")
  if (is.null(test)) {
    assert_that(
      all(c("x", "Item.Intended_category") %in% names(data)),
      msg = "If test is NULL, data must contains column x with nested test tokens and Item.Intended_category with mappings of x to the intended/correct category.")
    assert_that(is_list(data$x))
  }

  data %>%
    { if (!is.null(test)) add_test_tokens(., test) else select(., -Item.Intended_category) } %>%
    add_categorization() %>%
    # Keep only posterior of intended category
    { if (!is.null(test)) {
      left_join(
        .,
        test %>%
          select(x, Item.Intended_category),
        by = "x")
    } else {
      left_join(
        .,
        first(data$Item.Intended_category),
        by = "x")
    }} %>%
    filter(category == Item.Intended_category)
}

get_accuracy <- function(categorizations) {
  categorizations %>%
    pull(response) %>%
    mean()
}

# CHANGES IN REPRESENTATIONS ----------------------------------------------
plot_VOT_NIW_belief_1D <- function(
  belief,
  prior = NULL,
  xlim = VOT_range,
  ylim = NULL,
  resolution = VOT_resolution
) {
  mu_sigma <- belief %>%
    mutate(
      mu = get_expected_mu_from_m(m),
      sigma = get_expected_Sigma_from_S(S, nu))

  if (is.null(ylim)) ylim <- c(1, max(mu_sigma$sigma) * 2)
  if (!is.null(prior))
    prior.mu_sigma <- prior %>%
      mutate(
        mu = get_expected_mu_from_m(m),
        sigma = get_expected_Sigma_from_S(S, nu))

  belief %>%
    crossing(
      mu = seq_range(xlim, n = VOT_resolution),
      sigma = seq_range(sqrt(ylim), n = VOT_resolution)^2) %>%
    { if ("Subject" %in% names(.)) group_by(., Subject) else . } %>%
    # TO DO: Since mu and sigma can probably be vectors this can be made more efficient by first nesting and then unnesting
    # (see what I did for the test_plot function below). The line above this has been in added in anticipation of that
    # change (it's currently not required since the density is obtained line by line).
    mutate(l = unlist(pmap(
      .l = list(mu, m, kappa, sigma, S, nu),
      .f = dnorminvwishart))) %>%
    ggplot(aes(x = mu, y = sigma, color = category, group = category)) +
    { if (is.null(prior))
      list(
        geom_raster(
          data = ~ filter(., category == "/d/"),
          mapping = aes(fill = category, alpha = l),
          interpolate = T),
        geom_raster(
          data = ~ filter(., category == "/t/"),
          mapping = aes(fill = category, alpha = l),
          interpolate = T)) } +
    geom_contour(aes(z = l, color = category), breaks = 10^(-10:-3), size = .5) +
    { if (!is.null(prior))
      geom_contour(
        data = prior %>%
          crossing(
            mu = seq_range(xlim, n = VOT_resolution),
            sigma = seq_range(sqrt(ylim), n = VOT_resolution)^2) %>%
          mutate(l = unlist(pmap(
            .l = list(mu, m, kappa, sigma, S, nu),
            .f = dnorminvwishart))),
        aes(z = l, color = category), breaks = 10^(-10:-3), size = .5, alpha = .1) } +
    { if (is.null(prior))
      geom_point(
        data = mu_sigma,
        color = "black") } +
    { if (!is.null(prior))
      list(
        geom_point(
          data = prior.mu_sigma,
          color = "black",
          alpha = .5),
        geom_segment(
          data =
            mu_sigma %>%
            left_join(
              prior.mu_sigma %>%
                rename_at(vars(mu, sigma), ~ paste0("prior_", .x)),
              by = "category"),
          aes(x = prior_mu, y = prior_sigma, xend = mu, yend = sigma),
          arrow = arrow(angle = 15, length = unit(0.1, "inches"), ends = "last", type = "closed"),
          color = "black",
          size = .5,
          alpha = .75)) } +
    scale_x_continuous(name = bquote(mu ~ "(msec VOT)"), limits = xlim) +
    scale_y_sqrt(name = bquote(sigma^2 ~ "(" ~ msec^2 ~ ")"), limits = ylim) +
    scale_color_manual("Category", values = colors.voicing) +
    scale_fill_manual("Category", values = colors.voicing) +
    { if (is.null(prior)) scale_alpha_continuous("density", range = c(0,1), guide = "none") } +
    coord_cartesian(expand = FALSE) +
    theme(
      legend.position = "right",
      axis.text = element_text(size = 9),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank())
}


add_prior_and_get_posterior_beliefs_based_on_exposure <- function(
  data.exposure,
  prior,
  idealized = T,
  keep.update_history = FALSE,
  keep.exposure_data = FALSE
) {
  suppressWarnings(
    data.exposure %>%
      nest(data = -c(Condition, Subject)) %>%
      crossing(
        prior %>%
          nest(prior = -c(prior_kappa, prior_nu))) %>%
      group_by(Condition, Subject, prior_kappa, prior_nu) %>%
      mutate(
        posterior = map2(
          data,
          prior,
          ~ update_NIW_ideal_adaptor_incrementally(
            prior = .y,
            exposure = .x,
            exposure.category = "Item.Category",
            exposure.cues = c("VOT", "f0_Mel"),
            noise_treatment = if (idealized) "marginalize" else "sample",
            lapse_treatment = if (idealized) "marginalize" else "sample",
            method = "label-certain",
            keep.update_history = keep.update_history,
            keep.exposure_data = keep.exposure_data))))
}


# CHANGES IN RESPONSE BIASES ----------------------------------------------

# Functions for incremental bias change model
probability2logit <- function(p, refcat = 1)
  if (length(p) == 2)
    qlogis(p) else
    message("not yet defined")

logit2probability <- function(l, refcat = 1)
  if (length(l) == 2)
    plogis(l) else
    ifelse(1:length(l) == refcat, 1, exp(l)) / (1 + sum(exp(l[-refcat])))

update_NIW_response_bias_by_one_observation <- function(
  prior,
  beta,
  x,
  x_category,
  decision_rule,
  noise_treatment,
  lapse_treatment,
  verbose = F
) {
  assert_that(all(is_scalar_character(noise_treatment)), is_scalar_character(lapse_treatment))
  if (any(noise_treatment != "no_noise", lapse_treatment != "no_lapses"))
    assert_NIW_ideal_adaptor(prior, verbose = verbose) else assert_NIW_belief(prior, verbose = verbose)

  prior %>%
    left_join(
      # The response variable that is returned by the get_categorization function tells us *how often* each category response is
      # observed. The x_category argument tells what the (supervising) category label is. If the decision rule is "proportional"
      # each category will have a probability / expected proportion of occurrence (response) between 0 and 1, summing to 1. If
      # the decision rule is "sample" or "criterion" only one category will have response = 1 and all others will have 0. For
      # details, see help(get_categorization_from_NIW_ideal_adaptor).
      get_categorization_from_NIW_ideal_adaptor(x = x, model = prior, decision_rule = decision_rule, noise_treatment = noise_treatment, lapse_treatment = lapse_treatment) %>%
        # Calculate the amount of change (in log-odds) that the observed category label (x_category) causes.
        # Here we are assuming that each observation leads to change proportional to its surprisal (the
        # surprisal experienced when seeing the category label, x_category):
        mutate(delta_logodds = beta * sum(ifelse(category == x_category, -log2(response) * response, 0))),
        # If one wants to use an error signal that is either 0 or 1 (rather than the prediction error) then
        # delta_logodds would simply be the sum of all error signals multiplied by beta:
        # mutate(delta_logodds = beta * sum(ifelse(category == x_category, 0 * response, 1 * response))),
      by = "category") %>%
    # For the category that was actually observed (x_category) increase the response bias by delta_logodds.
    # Subtract that same *total* amount from the response bias of all other categories (which were not observed),
    # by decreasing the response bias of each of those categories by delta_logodds divided by the number of those
    # categories. Prior to rounding errors, this keeps the sum of the response bias across all categories at 1.
    # (finally, set prior to lapse bias since the model we're assuming here holds that the two biases are identical)
    mutate(
      lapse_bias = logit2probability(probability2logit(lapse_bias) + ifelse(category == x_category, +delta_logodds, -delta_logodds / (length(category) - 1))),
      # correct for rounding errors by re-normalizing
      lapse_bias = lapse_bias / sum(lapse_bias),
      prior = lapse_bias) %>%
    select(-c(observationID, x, response, delta_logodds)) %>%
    ungroup()
}


update_NIW_response_bias_incrementally <- function(
  prior,
  beta,
  exposure,
  exposure.category = "category",
  exposure.cues = get_cue_labels_from_model(prior),
  exposure.order = NULL,
  decision_rule,
  noise_treatment = if (is.NIW_ideal_adaptor(prior)) "sample" else "no_noise",
  lapse_treatment = if (is.NIW_ideal_adaptor(prior)) "sample" else "no_lapses",
  keep.update_history = TRUE,
  keep.exposure_data = FALSE,
  verbose = FALSE
){
  if (verbose) message("Assuming that category variable in NIW belief/ideal adaptor is called category.")
  if (lapse_treatment == "marginalize")
    warning("Using lapse_treatment == 'marginalize' can result in updating by *fractions* of observations, which might not be wellformed.", call. = FALSE)

  assert_NIW_belief(prior)
  assert_that(all(is.flag(keep.update_history), is.flag(keep.exposure_data)))
  assert_that(any(is_tibble(exposure), is.data.frame(exposure)))
  assert_that(exposure.category %in% names(exposure),
              msg = paste0("exposure.category variable not found: ", exposure.category, " must be a column in the exposure data."))
  assert_that(any(is.null(exposure.order), exposure.order %in% names(exposure)),
              msg = paste0("exposure.order variable not found: ", exposure.order, " must be a column in the exposure data."))
  assert_that(any(is.null(exposure.order), if (!is.null(exposure.order)) is.numeric(exposure[[exposure.order]]) else T),
              msg = "exposure.order variable must be numeric.")

  # Number of dimensions/cues
  D = length(exposure.cues)
  if (any(prior$nu <= D + 1))
    message(paste0("Prior for at least one category had nu smaller than allowed (is ", min(prior$nu), "; should be >", D+1, ").\n"))

  # Prepare exposure data
  exposure %<>%
    { if (!is.null(exposure.order)) arrange(., !! sym(exposure.order)) else . } %>%
    make_vector_column(exposure.cues, "cues")

  if (keep.update_history)
    prior %<>%
    mutate(observation.n = 0)

  for (i in 1:nrow(exposure)) {
    posterior = if (keep.update_history) prior %>% filter(observation.n == i - 1) else prior

    posterior =
      suppressWarnings(
        update_NIW_response_bias_by_one_observation(
          prior = posterior,
          beta = beta,
          x = matrix(unlist(exposure[i,][["cues"]]), nrow = 1),
          x_category = exposure[i,][[exposure.category]],
          decision_rule = decision_rule,
          noise_treatment = noise_treatment,
          lapse_treatment = lapse_treatment,
          verbose = verbose))

    if (keep.update_history) {
      posterior %<>%
        mutate(observation.n = i)
      prior = rbind(prior, posterior)
    } else prior = posterior
  }

  if (keep.exposure_data) {
    exposure %<>%
      { if (!is.null(exposure.order))
        rename(., observation.n = !! sym(exposure.order)) else
          mutate(., observation.n = 1:nrow(exposure)) } %>%
      rename_with(~ paste0("observation.", .x), !starts_with("observation.n"))
     prior %<>%
      left_join(exposure)
  }

  return(prior)
}

update_bias_and_categorize_test <- function(
    prior,
    lapse_rate,
    beta_pi,
    exposure,
    test,
    cues = c("VOT", "f0_Mel"),
    control = list(
      min.simulations = 5,
      max.simulations = 10,
      step.simulations = 1,
      target_accuracy_se = .005),
    add_updates = NULL,
    verbose = FALSE
) {
  u <-
    lapply(
      X = as_list(1:control[["min.simulations"]]),
      FUN = function(x)
        update_NIW_response_bias_incrementally(
          prior = prior,
          beta = beta_pi,
          # Reshuffle exposure on each run
          exposure = exposure %>% sample_frac(1, replace = F),
          exposure.category = "Item.Category",
          exposure.cues = cues,
          decision_rule = "proportional",
          noise_treatment = "marginalize",
          lapse_treatment = "marginalize",
          keep.update_history = FALSE,
          keep.exposure_data = FALSE) %>%
        mutate(lapse_rate = .env$lapse_rate) %>%
        nest(posterior = everything()) %>%
        mutate(sim = x) %>%
        add_test_and_categorize(test)) %>%
    reduce(bind_rows)

  # If existing updates were provided add them to the ones just created before
  # checking whether one of the convergence criteria is reached.
  if (!is.null(add_updates))
    u %<>%
    mutate(sim = sim + n_distinct(add_updates$sim)) %>%
    bind_rows(add_updates)

  u.test <-
    u %>%
    group_by(sim) %>%
    summarise(response.mean = mean(response)) %>%
    ungroup() %>%
    summarise(
      n.sims_so_far = n_distinct(sim),
      min.additional_simulations = .env$control[["min.simulations"]],
      max.additional_simulations = .env$control[["max.simulations"]] - min.additional_simulations,
      step.simulations = .env$control[["step.simulations"]],
      target_accuracy_se = .env$control[["target_accuracy_se"]],
      lapse_rate = first(.env$prior$lapse_rate),
      response.mean.mean = mean(response.mean),
      response.mean.se = sd(response.mean) / sqrt(n_distinct(sim)))

  if (verbose)
    u.test %>%
    print()


  # If max simulations not yet reached AND target_se not yet reached, run more
  # simulations.
  if (
    control[["min.simulations"]] < control[["max.simulations"]] &
    (u.test %>% pull(response.mean.se) %>% { . > control[["target_accuracy_se"]]})) {

    u <-
      update_bias_and_categorize_test(
        prior = prior,
        lapse_rate = lapse_rate,
        beta_pi = beta_pi,
        exposure = exposure,
        test = test,
        cues = cues,
        verbose = verbose,
        control = list(
          min.simulations = control[["step.simulations"]], # after running the minimal number of simulations (min.simulations), increase min.simulations by a few steps (step.simulations) until the max.simulations is reached or the standard error of the response mean (response.mean.se) reaches the predetermined criteria
          max.simulations = control[["max.simulations"]] - control[["min.simulations"]],
          step.simulations = control[["step.simulations"]],
          target_accuracy_se = control[["target_accuracy_se"]]),
        add_updates = u)

  }
  return(u)
}


# CHANGES IN NORMALIZATION ------------------------------------------------
add_prior_and_normalize_test_tokens_based_on_exposure <- function(data.exposure, data.test, prior.normalization, prior.categories) {
  # Get prior mean
  mu_0 <- prior.normalization$x_mean[[1]]

  # Get normalization parameters for exposure data
  exposure.normalization <- data.exposure %>%
    group_by(Condition, Subject) %>%
    summarise(
      x_N = length(x),
      x_mean = list(colMeans(reduce(x, rbind))),
      x_cov = list(cov(reduce(x, rbind))))

  data.exposure %>%
    nest(data = -c(Condition, Subject)) %>%
    crossing(
      prior_kappa.normalization = 4^(1:5),
      # prior_nu.normalization = 4^(1:5),
      prior.categories %>%
        filter(prior_kappa == max(prior_kappa) & prior_nu == max(prior_nu)) %>%
        nest(prior = everything())) %>%
    group_by(Condition, Subject, prior_kappa.normalization) %>%
    mutate(posterior = prior) %>%
    crossing(data.test %>% distinct(x)) %>%
    # Normalize test tokens
    mutate(
      # Get inferred mean
      mu_inferred = pmap(
        .l = list(Condition, Subject, prior_kappa.normalization),
        .f = function(currentCondition, currentSubject, currentKappa) {
          x_N <- exposure.normalization[exposure.normalization$Condition == currentCondition & exposure.normalization$Subject == currentSubject, "x_N"][[1]]
          x_bar <- exposure.normalization[exposure.normalization$Condition == currentCondition & exposure.normalization$Subject == currentSubject, "x_mean"][[1]][[1]]

          mu <- 1 / (currentKappa + x_N) * (currentKappa * mu_0 + x_N * x_bar)
          return(mu)
        }),
      # Adjust test tokens based on the difference between the inferred mean and prior mean
      x = map2(x, mu_inferred, ~ .x - (.y - mu_0))) %>%
    nest(x = c(x))
}

# 3D FIGURES ------------------------------------------------
save_3Dfigure <- function(figure, filename) {
  require(processx)
  orca(figure, file = file.path(get_path('../figures/plotly/'), filename))
}

# Bivariate Gaussian density function wrapper suitable for use with outer()
bivariate_density <- function(x, y, mu, Sigma) dmvnorm(cbind(x, y), mu, Sigma)

make_quadratic_effects_of_cues_monotonic <- function(data){
  data %<>%
    as.matrix() %>%
    data.table()

  data[, col_max := colnames(.SD)[max.col(.SD, ties.method = "first")]] # get column number that has the maximal value within each row
  data$max <- apply(data[,.SD, .SDcols = !c('col_max')], 1, max) # get the max value within each row

  data %<>%
    as.data.frame() %>%
    rownames_to_column('id') %>%
    pivot_longer(
      cols = starts_with("V"),
      names_to = "col_name",
      values_to = "prob") %>%
    # criteria to fix the quadratic effect: for each row, if
    #     1) the current value is smaller than the max value of the row, and
    #     2) the col_num of the current value is smaller (i.e., VOT is smaller) than the col_num of the max value,
    # then replace the current value with the max value
    mutate(
      need_to_fix = as.numeric(gsub("V", "", col_name)) < as.numeric(gsub("V", "",  col_max)),
      prob.fixed = ifelse(need_to_fix, max, prob)) %>%
    select(id, col_name, prob.fixed) %>%
    pivot_wider(names_from = col_name, values_from = prob.fixed) %>%
    select(-id) %>%
    as.matrix()

  return(data)
}

# Function to demonstrate representations in 3D plots
demonstrate_representations_3D <- function(model, n, cue1_range, cue2_range, cue_names, lambda, pi, fix_quadratic_effects = FALSE) {
  x <- seq(cue1_range[1], cue1_range[2], length = n) # generating the vector series cue 1
  y <- seq(cue2_range[1], cue2_range[2], length = n) # generating the vector series cue 2

  bivn <- list()
  prob <- list()
  ellipse_data <- list()

  for (i in 1:length(model$category)) {
    mu <- model$mu[[i]]
    Sigma <- model$Sigma[[i]]

    # calculate the density values (transpose the matrix generated by 'outer': x is the first cue, y is the 2nd cue)
    # and add the grid of the two cues (x, y) and density together into a list
    z <- t(outer(x, y, FUN = bivariate_density, mu, Sigma))
    bivn[[i]] <- list(x, y, z)
    names(bivn[[i]]) <- c(cue_names, "density")

    # add ellipse
    ellipse_data[[i]] <- mixtools::ellipse(mu, Sigma, alpha = .05, npoints = 250, newplot = F, draw = F)
    ellipse_data[[i]] <- cbind(as.data.frame(ellipse_data[[i]][,1]), as.data.frame(ellipse_data[[i]][,2]))
    colnames(ellipse_data[[i]]) <- cue_names
    ellipse_data[[i]]$z <- NULL
    ellipse_data[[i]]$density <- 0

    prob[[i]] <- z
  }

  # create categorization surface
  resp.prob <- prob[[1]] / (prob[[1]] + prob[[2]])

  # fix the quadratic effects in the categorization function that can result from unequal variances?
  if (fix_quadratic_effects) {
    resp.prob <- make_quadratic_effects_of_cues_monotonic(resp.prob)
    resp.prob <- make_quadratic_effects_of_cues_monotonic(t(resp.prob))
    resp.prob <- t(resp.prob)
  }

  # add lapse rate and response bias
  resp.prob <- (1 - lambda) * resp.prob + lambda * pi
  df.resp <- list(x, y, resp.prob)
  names(df.resp) <- c(cue_names, "proportion_d")

  color <- rep(0, length(bivn[[1]]$density))

  output <- list(bivn[[1]], bivn[[2]], ellipse_data[[1]], ellipse_data[[2]], df.resp, color)
  names(output) <- c("d.bivn", "t.bivn", "d.ellipse", "t.ellipse", "df.resp", "color")

  return(output)
}

# Function to make category distributions in 3D space (x-cue1, y-cue2, z-density)
plot_3D.density <- function(d.bivn, t.bivn, d.ellipse, t.ellipse, color, width, height){
  plot_ly(width = width, height = height) %>%
    add_surface(
      x = d.bivn$VOT, y = d.bivn$f0, z = d.bivn$density,
      opacity = 0.2,
      name = category.contrasts[1],
      colors = colors.voicing,
      colorscale = list(c(0, 1), c(colors.voicing[1], colors.voicing[1])),
      surfacecolor = color,
      cauto = F,
      cmax = 1,
      cmin = 0,
      showscale = FALSE) %>%
    add_surface(
      x = t.bivn$VOT, y = t.bivn$f0, z = t.bivn$density,
      opacity = 0.2,
      name = category.contrasts[2],
      colorscale = list(c(0, 1), c(colors.voicing[2], colors.voicing[2])),
      surfacecolor = color,
      cauto = F,
      cmax = 1,
      cmin = 0,
      showscale = FALSE) %>%
    # this layer is just used to create the desired legend marker size
    add_trace(
      x = d.ellipse$VOT, y = d.ellipse$f0, z = t(d.ellipse$density),
      type="scatter3d", mode="markers",
      marker = list(size = 10), opacity = 0,
      opacity = 1, color = colors.voicing[1], size = 0.01, showlegend = FALSE) %>%
    add_trace(
      x = d.ellipse$VOT, y = d.ellipse$f0, z = t(d.ellipse$density),
      type="scatter3d", mode="markers",
      marker = list(size = 2),
      opacity = 1, color = colors.voicing[1], size = 0.01, name=category.contrasts[1]) %>%
    add_trace(
      x = t.ellipse$VOT, y = t.ellipse$f0, z = t(t.ellipse$density),
      type="scatter3d", mode="markers",
      marker = list(size = 2),
      opacity = 1, color = colors.voicing[2], size = 0.01, name=category.contrasts[2]) %>%
    layout(
      legend = list(
        itemsizing = "constant",
        font = list(size = 22),
        orientation = "h",   # show entries horizontally
        xanchor = "center",  # use center of legend as anchor
        x = 0.5, y = 0.9),   # specify legend position: x, y values between -2 and 3
      scene = list(
        aspectratio = list(x = 1.2, y = 1.2, z = 1), # zooming in on the plot, values larger than 0
        camera = list(eye = list(x = -0.5, y = -2.5, z = 0.5),
                      up =  list(x = 0, y = 0, z = 1)),
        zaxis = list(title = "Density", titlefont = list(size = 22)),
        yaxis = list(title = "f0 (Mel)", titlefont = list(size = 22)),
        xaxis = list(title = "VOT (ms)", titlefont = list(size = 22))))
}

# Function to make categorization surface in 3D space (x-cue1, y-cue2, z-prop of category1)
plot_3D.categorization <- function(df.resp, width, height){
  # plot categorization surface
  plot_ly(width = width, height = height) %>%
    add_surface(
      x = df.resp$VOT, y = df.resp$f0, z = df.resp$proportion_d,
      opacity = 0.5,
      showscale = FALSE,
      colorscale = list(c(0, 1), c(colors.voicing[2], colors.voicing[1])),
      cmin = 0,
      cmid = 0.5,
      cmax = 1) %>%
    colorbar(
      title = paste0('Posterior probability\nof ', category.contrasts[1]),
      len = .75,
      titlefont = list(size = 22),
      x = 0.9, y = 0.9) %>%
    layout(
      scene = list(
        aspectratio = list(x = 1.2, y = 1.2, z = 1), # zooming in on the plot, values larger than 0
        camera = list(
          eye = list(x = -0.5, y = -2.5, z = 0.5), # perspective good for showing categorization curve
          up =  list(x = 0, y = 0, z = 1)),
        zaxis = list(title = paste0('Posterior probability of ', category.contrasts[1]), titlefont = list(size = 22), range = c(0,1), tickvals = c(0, 0.2 ,0.4, 0.6, 0.8, 1)),
        yaxis = list(title = "f0 (Mel)", titlefont = list(size = 22)),
        xaxis = list(title = "VOT (ms)", titlefont = list(size = 22))))
}

# Function to show differences in categorization surface in 3D log-odds space (x-cue1, y-cue2, z-prop of category1)
plot_3D.categorization.diff <- function(df.resp, width, height){
  plot_ly(width = width, height = height) %>%
    add_surface(
      x = df.resp$VOT, y = df.resp$f0, z = df.resp$difference_in_logodds_d,
      opacity = 0.5,
      showscale = FALSE,
      colorscale = list(c(0, 0.5, 1), c("red", "lightgrey","blue")),
      cmin = -15,
      cmid = 0,
      cmax = 15) %>%
    colorbar(
      title = paste0('Posterior probability\nof ', category.contrasts[1]),
      len = .75,titlefont = list(size = 22),
      x = 0.9, y = 0.9) %>%
    layout(
      scene = list(
        # zooming in on the plot, values larger than 0
        aspectratio = list(x = 1.2, y = 1.2, z = 1),
        # perspective good for showing categorization curve
        camera = list(
          eye = list(x = -0.5, y = -2.5, z = 0.5),
          up =  list(x = 0, y = 0, z = 1)),
        zaxis = list(title = "Diff. in posterior<br>log-odds of /d/", titlefont = list(size = 22), range = c(-5,10)),
        yaxis = list(title = "f0 (Mel)",titlefont = list(size = 22)),
        xaxis = list(title = "VOT (ms)", titlefont = list(size = 22))))
}

# Function to make categorization surface in 3D space based on the modeling results from the three change mechanisms
prepare_3D.categorization_from_results <- function(data, cue1_range, cue2_range, n, fix_quadratic_effects = FALSE) {

  x<-seq(cue1_range[1], cue1_range[2],length=n)  # generating the vector sequence for cue 1
  y<-seq(cue2_range[1], cue2_range[2],length=n)  # generating the vector sequence for cue 2

  d.input = expand.grid(x, y)
  colnames(d.input) = c("VOT", "f0")
  temp2 = as.data.frame(d.input) %>% # create a grid of cue 1 and cue 2
    mutate(x = map2(VOT, f0, ~ c(.x, .y)))

  output <- vector(mode = "list", length = length(conditions.AA))
  for (i in 1:length(conditions.AA)){

    ##----------------
    #convert cue values based on the mu_inferred for the current parameter of prior_kappa.normalization
    if("Normalization" %in% levels(factor(data$model))) {
      d.AA.normalization.step0 <- d.AA.normalization %>%
        filter(prior_kappa.normalization  == prior_kappa.normalization.selected) %>%
        filter(Condition == conditions.AA[i]) %>%
        droplevels() %>%
        mutate(mu_inferred = map2(x_unnormalized, x, ~ .x - (.y - prior_marginal_VOT_f0_stats$x_mean[[1]])))
      temp2 %<>%
        mutate(mu_inferred = d.AA.normalization.step0$mu_inferred[1],
               x = map2(x, mu_inferred, ~ .x - (.y - prior_marginal_VOT_f0_stats$x_mean[[1]])))
    }

    ##----------------
    d.output.step1 <- data %>%
      crossing(temp2 %>% distinct(x)) %>%
      nest(x = c(x)) %>%
      add_categorization() %>%
      filter(Condition == conditions.AA[i] & category == "/d/")

    ##----------------
    # For the bias model, get the average response predictions from multiple simulations
    if("Decision_making" %in% levels(factor(data$model))){
      d.output.step1 %<>%
      group_by(!!! syms(setdiff(names(.), c("sim", "posterior", "response")))) %>%
      summarise(
        response.n_sims = n_distinct(sim),
        response.se = sd(response) / sqrt(response.n_sims),
        response = mean(response)) %>%
      relocate(
        !!! syms(setdiff(names(.), c("response", "response.se", "response.n_sims"))),
        response, response.se, response.n_sims)
    }

    d.output <- d.output.step1 %>%
      group_by(Condition) %>%
      unnest(x) %>%
      mutate(nrow = row_number(),
             cue_name = ifelse((nrow %% 2) == 0, "f0", "VOT")) %>%
      select(-nrow) %>%
      pivot_wider(values_from = x, names_from = cue_name)

    df.resp <- list()
    df.resp$VOT <- x
    df.resp$f0 <- y

    # create categorization surface
    resp.prob <- t(matrix(d.output$response, ncol=n))

    # fix the quadratic effects in the categorization function that can result from unequal variances?
    if (fix_quadratic_effects) {
      resp.prob <- make_quadratic_effects_of_cues_monotonic(resp.prob)
      resp.prob <- make_quadratic_effects_of_cues_monotonic(t(resp.prob))
      resp.prob <- t(resp.prob)
    }
    df.resp$proportion_d <- resp.prob

    output[[i]] = df.resp
  }

  return(output)
}
