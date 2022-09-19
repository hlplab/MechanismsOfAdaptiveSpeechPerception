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
  margin=c("t" = 0.6, "r" = 0.5, "b" = 0.5, "l" = 0.3),
  set_theme = T
)
{
  require(ggplot2)

  if (set_theme) {
    theme_set(theme_bw(base_size=base_size))
    theme_update(
      axis.text.x = element_text(size=base_size, vjust=1),
      axis.text.y = element_text(size=base_size, hjust=1, vjust=.5),
      axis.title.x = element_text(size=base_size , vjust=0, hjust=0.5, face = "bold"),
      axis.title.y = element_text(size=base_size, hjust= 0.5, vjust=0.5, face = "bold"),
      strip.text = element_text(size=base_size, color = "white"),
      strip.background = element_rect(fill = "black", color = "black"),
      legend.title = element_text(size=base_size, face = "bold", hjust= 0),
      legend.text = element_text(size=base_size),
      plot.margin = unit(margin, "lines")
    )
  } else {
    return(
      theme(
        axis.text.x = element_text(size=base_size, vjust=1),
        axis.text.y = element_text(size=base_size, hjust=1, vjust=.5),
        axis.title.x = element_text(size=base_size , vjust=0, hjust=0.5, face = "bold"),
        axis.title.y = element_text(size=base_size, hjust= 0.5, vjust=0.5, face = "bold"),
        strip.text = element_text(size=base_size, color = "white"),
        strip.background = element_rect(fill = "black", color = "black"),
        legend.title = element_text(size=base_size, face = "bold", hjust= 0),
        legend.text = element_text(size=base_size),
        plot.margin = unit(margin, "lines")))
  }
}

# Align a list of plots with shared x-axis but potentially multiple rows, each with different y-axis
# Each column is also assumed to have the same titles, which are then plotted only above the top row.
my_plot_grid <- function(
  plotlist,
  labels.top.row = c(
    # example of atop
    # bquote(atop("Prior" ~ kappa[0 ~","~ c] == .(weak), "     " ~ nu[0 ~","~ c] == .(weak))),
    bquote(~" Prior" ~ { kappa[0*","*c] == nu[0*","*c] } == .(weak)),
    bquote(~" Prior" ~ kappa[0*","*c] == .(weak) ~ ", " ~ nu[0*","*c] == .(strong)),
    bquote(~" Prior" ~ kappa[0*","*c] == .(strong) ~ ", " ~ nu[0*","*c] == .(weak)),
    bquote(~" Prior" ~ { kappa[0*","*c] == nu[0*","*c] } == .(strong))),
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
      size = 11,
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


# NORMALIZATION -----------------------------------------------------------
apply_ccure <- function(x, data) {
  x - predict(lmer(x ~ 1 + (1 | Talker), data = data), random.only = T)
}

# MAKE IDEAL OBSERVERS ----------------------------------------------------

# Make IO out of MVG
make_stop_VOTf0_ideal_observer <- function(
    m,
    prior = rep(1 / nrow(m), nrow(m)),
    lapse_rate = 0,
    lapse_bias = rep(1 / nrow(m), nrow(m)),
    Sigma_noise = matrix(c(80, 0, 0, 878), ncol = 2, dimnames = list(c("VOT", "f0_Mel"), c("VOT", "f0_Mel")))
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
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
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


add_prior_and_posterior_with_changed_response_biases_based_on_exposure <- function(
  data.exposure,
  prior,
  idealized = T,
  decision_rule = if (idealized) "proportional" else "sample",
  keep.update_history = FALSE,
  keep.exposure_data = FALSE
) {
  suppressWarnings(data.exposure %>%
    nest(data = -c(Condition, Subject)) %>%
    crossing(
      posterior.lapse_rate = c(.0005, .005, .05, .5, 1),
      beta_pi = c(0, .01, .05, .1, .2, .8)) %>%
    crossing(
      prior %>%
        filter(prior_kappa == max(prior_kappa), prior_nu == max(prior_nu)) %>%
        nest(prior = everything())) %>%
    group_by(Condition, Subject, posterior.lapse_rate, beta_pi) %>%
    mutate(
      posterior = pmap(
        .l = list(data, prior, posterior.lapse_rate, beta_pi),
        .f = function(.data, .prior, .posterior.lapse_rate, .beta_pi)
          update_NIW_response_bias_incrementally(
            prior = .prior,
            exposure = .data,
            exposure.category = "Item.Category",
            exposure.cues = c("VOT", "f0_Mel"),
            beta = .beta_pi,
            decision_rule = decision_rule,
            noise_treatment = if (idealized) "marginalize" else "sample",
            lapse_treatment = if (idealized) "marginalize" else "sample",
            keep.update_history = keep.update_history,
            keep.exposure_data = keep.exposure_data) %>%
        mutate(lapse_rate = .posterior.lapse_rate))))
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



