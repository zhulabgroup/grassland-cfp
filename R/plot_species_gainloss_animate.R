#' @export
plot_species_gainloss_animate <- function(dat_community, dat_gainloss, dat_niche, outdir = "alldata/output/figures/animation/") {
  p_obs <- plot_species_gainloss_animate_obs(dat_comm = dat_community$obs, dat_gl = dat_gainloss$obs, dat_niche, nrow = 3, outdir = outdir)

  p_obs_site <- vector(mode = "list")
  for (siteoi in dat_community$obs$site %>%
    unique() %>%
    sort()) {
    p_obs_site[[siteoi]] <- plot_species_gainloss_animate_obs(dat_comm = dat_community$obs, dat_gl = dat_gainloss$obs, dat_niche, nrow = 1, outdir = outdir, onesite = siteoi)
  }

  p_exp <- plot_species_gainloss_animate_exp(dat_comm = dat_community$exp, dat_gl = dat_gainloss$exp, dat_niche, outdir = outdir)

  out <- list(
    obs = p_obs,
    obs_site = p_obs_site,
    exp = p_exp
  )
  return(out)
}

#' @export
plot_species_gainloss_animate_obs <- function(dat_comm, dat_gl, dat_niche, nrow = 3, outdir = "alldata/output/figures/", onesite = NULL) {
  if (!is.null(onesite)) {
    dat_comm <- dat_comm %>%
      filter(site == onesite)
  }

  dat_rel_abun <- dat_comm %>%
    filter(guild != "DUMMY") %>%
    group_by(site, year, species) %>%
    summarise(abund = sum(abund)) %>%
    ungroup() %>%
    group_by(site, year) %>%
    mutate(rel_abun = abund / sum(abund)) %>%
    ungroup() %>%
    select(-abund) %>%
    # spread(key = "species", value = "rel_abun", fill = 0) %>% # so that species with 0 rel abun do not disappear
    # gather(key = "species", value = "rel_abun",-site, -year) %>%
    inner_join(dat_gl %>% select(site, species, change, complete_change, tmp, ppt), by = c("site", "species"))

  gg_gainloss_obs <-
    ggplot() +
    geom_point(
      data = dat_niche,
      aes(
        x = tmp_occ_median,
        y = ppt_occ_median
      ), color = "gray", alpha = 0
    ) +
    geom_point(
      data = dat_rel_abun,
      aes(
        x = tmp,
        y = ppt,
        size = rel_abun,
        color = change,
        group = species
      ), alpha = 1, pch = 21, fill = NA
    )

  if (dat_rel_abun %>% filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)) %>% nrow() > 0) {
    gg_gainloss_obs <- gg_gainloss_obs +
      geom_point(
        data = dat_rel_abun %>% filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)),
        aes(
          x = tmp,
          y = ppt,
          size = rel_abun,
          color = change,
          fill = complete_change,
          group = species
        ), alpha = 0.75, pch = 21
      )
  }

  gg_gainloss_obs <- gg_gainloss_obs +
    scale_color_manual(values = c(increase = "dark green", `no clear change` = "lightgray", decrease = "dark orange")) +
    scale_fill_manual(values = c(new = "dark green", lost = "dark orange")) +
    labs(
      x = "Mean annual temperature (°C)",
      y = "Annual precipitation (mm)",
      title = "Year: {closest_state}"
    ) +
    guides(
      fill = "none",
      size = "none",
      color = "none"
    ) +
    theme(axis.text = element_text(size = 8)) +
    facet_wrap(. ~ site %>% plot_site_name(with_letter = F),
      nrow = nrow
    ) +
    theme(strip.text = element_text(hjust = 0))

  anim_gainloss_obs <- gg_gainloss_obs +
    gganimate::transition_states(
      year,
      wrap = F
    ) +
    gganimate::enter_grow() +
    gganimate::exit_shrink() +
    gganimate::ease_aes("linear")

  if (is.null(onesite)) {
    f <- str_c(outdir, "gainloss-obs.gif")
    gganimate::anim_save(f, anim_gainloss_obs,
      nframes = 50,
      fps = 10,
      height = 8,
      width = 10,
      units = "in",
      res = 300
    )
    file.copy(from = f, to = str_c("inst/figures/animation/"), recursive = T)
  } else {
    f <- str_c(outdir, "gainloss-obs-", onesite, ".gif")
    gganimate::anim_save(f, anim_gainloss_obs,
      start_pause = 5,
      end_pause = 5,
      nframes = 30,
      height = 4,
      width = 4,
      units = "in",
      res = 300
    )
    file.copy(from = f, to = str_c("inst/figures/animation/"), recursive = T)
  }

  return(f)
}

#' @export
plot_species_gainloss_animate_exp <- function(dat_comm, dat_gl, dat_niche, outdir = "alldata/output/figures/") {
  dat_rel_abun <- dat_comm %>%
    filter(site == "jrgce") %>%
    filter(year != 1998) %>%
    mutate(treat_T = str_sub(treat, start = 1L, end = 1L)) %>%
    mutate(phase = case_when(
      year <= 2002 ~ "Phase I",
      year >= 2010 ~ "Phase III",
      TRUE ~ "Phase II"
    )) %>%
    mutate(phaseyear = paste0(phase, ": ", year)) %>%
    filter(guild != "DUMMY") %>%
    group_by(phaseyear, treat_T, species) %>%
    summarise(abund = sum(abund)) %>%
    ungroup() %>%
    group_by(phaseyear, treat_T) %>%
    mutate(rel_abun = abund / sum(abund)) %>%
    ungroup() %>%
    select(-abund) %>%
    # spread(key = "species", value = "rel_abun", fill = 0.00001) %>% # so that species with 0 rel abun do not disappear
    # gather(key = "species", value = "rel_abun",-phaseyear, -treat_T) %>%
    inner_join(dat_gl %>% select(phaseyear, species, change, complete_change, tmp, ppt), by = c("phaseyear", "species")) %>%
    mutate(treat_T = factor(treat_T, levels = c("_", "T"), labels = c("ambient", "warming"))) %>%
    mutate(phaseyear = factor(phaseyear,
      levels = c(
        "Phase I: 1999",
        "Phase I: 2000",
        "Phase I: 2001",
        "Phase I: 2002",
        " ",
        "  ",
        "   ",
        "Phase II: 2003",
        "Phase II: 2004",
        "Phase II: 2005",
        "Phase II: 2006",
        "Phase II: 2007",
        "Phase II: 2008",
        "Phase II: 2009",
        "Phase III: 2010",
        "Phase III: 2011",
        "Phase III: 2012",
        "Phase III: 2013",
        "Phase III: 2014",
        "    ",
        "     "
      )
    ))

  anim_gainloss_exp <-
    ggplot() +
    geom_point(
      data = dat_niche,
      aes(
        x = tmp_occ_median,
        y = ppt_occ_median
      ), color = "gray", alpha = 0
    ) +
    geom_point(
      data = dat_rel_abun,
      aes(
        x = tmp,
        y = ppt,
        size = rel_abun,
        color = change,
        group = species
      ), alpha = 1, pch = 21, fill = NA
    ) +
    geom_point(
      data = dat_rel_abun %>% filter(change == "increase" | change == "decrease") %>% filter(!is.na(complete_change)),
      aes(
        x = tmp,
        y = ppt,
        size = rel_abun,
        color = change,
        fill = complete_change,
        group = species
      ), alpha = 0.75, pch = 21
    ) +
    scale_color_manual(values = c(increase = "dark green", `no clear change` = "lightgray", decrease = "dark orange")) +
    scale_fill_manual(values = c(new = "dark green", lost = "dark orange")) +
    labs(
      x = "Mean annual temperature (°C)",
      y = "Annual precipitation (mm)",
      title = "Treatment: {closest_state}"
    ) +
    guides(
      fill = "none",
      size = "none",
      color = "none"
    ) +
    theme(axis.text = element_text(size = 8)) +
    facet_wrap(. ~ phaseyear,
      nrow = 3
    ) +
    theme(strip.text = element_text(hjust = 0)) +
    gganimate::transition_states(
      treat_T,
      wrap = F
    ) +
    gganimate::enter_grow() +
    gganimate::exit_shrink() +
    gganimate::ease_aes("linear")

  f <- str_c(outdir, "gainloss-exp.gif")
  gganimate::anim_save(f, anim_gainloss_exp,
    start_pause = 2,
    end_pause = 2,
    nframes = 20,
    fps = 10,
    # duration = 2,
    height = 8,
    width = 12,
    units = "in",
    res = 300
  )
  file.copy(from = f, to = str_c("inst/figures/animation/"), recursive = T)

  return(f)
}
