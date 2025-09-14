# helper function to prep apa dec csv data for ebase
ebsdatprp <- function(pth){

  out <- read.csv(pth) |> 
    select(
      DateTimeStamp = datetimestamp,
      DO_obs = do_mgl,
      DO_nrm = donrm_mgl, 
      Temp = temp_c,
      Sal = sal_ppt,
      PAR = par_wm2,
      WSpd = wspd_ms,
      Depth = depth_m
    ) |> 
    mutate(
      DateTimeStamp = ymd_hms(DateTimeStamp, tz = 'America/Jamaica')
    ) |> 
    arrange(DateTimeStamp) |> 
    filter(lubridate::year(DateTimeStamp) < 2025)
  
  return(out)
  
}

# view ebase result
ebsresplo <- function(obs, dtd){
  
  ylab <- 'mmol O2 m-2 d-1'
  
  toplo <- list(
    Observed = obs, 
    Detided = dtd
  ) |> 
    enframe(name = 'Type') |> 
    unnest(value) |> 
    select(Type, Date, P, R, D) |> 
    mutate(
      NEM = P - R
    ) |> 
    pivot_longer(cols = -matches('Date|Type')) |> 
    summarise(value = mean(value, na.rm = T), .by = c(Type, Date, name)) |> 
    pivot_wider(names_from = name, values_from = value)
  
  p1 <- plot_ly(subset(toplo, Type == 'Observed'), x = ~Date, y = ~P, type = 'scatter', mode = 'lines', name = 'P') |>
    add_trace(y = ~-R, mode = 'lines', name = 'R') |>
    add_trace(y = ~D, mode = 'lines', name = 'D') |>
    add_trace(y = ~NEM, mode = 'lines', name = 'NEM') |>
    layout(xaxis = list(title = ''),
           yaxis = list(title = paste('Observed', ylab)))
  
  p2 <- plot_ly(subset(toplo, Type == 'Detided'), x = ~Date, y = ~P, type = 'scatter', mode = 'lines', name = 'P') |>
    add_trace(y = ~-R, mode = 'lines', name = 'R') |>
    add_trace(y = ~D, mode = 'lines', name = 'D') |>
    add_trace(y = ~NEM, mode = 'lines', name = 'NEM') |>
    layout(xaxis = list(title = ''),
           yaxis = list(title = paste('Detided', ylab)))
  
  out <- subplot(p1, p2, nrows = 2, shareX = T, titleY = T)
  
  return(out)
  
}

# function to compare odum, ebase -------------------------------------------------------------
cmpplo_fun <- function(toplo, dotyp){
  
  
  typs <- c('P', 'R', 'NEM', 'D')
  for(typ in seq_along(typs)) { 
    
    cat(typs[typ], '\t')
    
    flt <- toplo |> 
      filter(dotyp == !!dotyp) |> 
      filter(type == typs[typ]) |> 
      na.omit() |> 
      mutate(
        density = get_density(value.ebase, value.odum, n = 100)
      )
    
    p <- ggplot(flt, aes(x = value.odum, y = value.ebase)) + 
      geom_hline(yintercept = 0, color = 'grey') + 
      geom_vline(xintercept = 0, color = 'grey') + 
      geom_point(size = 1, alpha = 0.7, aes(color = density), show.legend = F) +
      scale_color_viridis_c() +
      facet_grid(name ~ type) +
      geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
      geom_smooth(method = 'lm', formula = y ~ x, se = F) + 
      theme_bw() +  
      theme(
        panel.grid = element_blank(), 
        strip.background = element_blank()
      ) +
      labs(
        x = expression(paste('Odum, mmol'~O[2]~m^2~d^-1)), 
        y = expression(paste('EBASE, mmol'~O[2]~m^2~d^-1))
      )
    
    if(typ != 4)
      p <- p + theme(strip.text.y = element_blank())
    
    if(typ == 1)
      p <- p + labs(title = dotyp)

    assign(paste0('p', typ), p)
    
  }
  
  cat('\n')
  
  p <- p1 + p2 + p3 + p4 + plot_layout(ncol = 4, axis_titles = 'collect')
  
  return(p)
  
}

# function to calculate relative point density in a scatterplot -------------------------------
get_density <- function(x, y, n = 100) {
  # Compute the 2D kernel density estimate
  kde <- MASS::kde2d(x, y, n = n)
  
  # For each point, find the corresponding density value
  density_values <- numeric(length(x))
  for (i in seq_along(x)) {
    # Find the closest grid points
    x_bin <- findInterval(x[i], kde$x)
    y_bin <- findInterval(y[i], kde$y)
    
    # Edge case handling
    x_bin <- min(max(x_bin, 1), length(kde$x) - 1)
    y_bin <- min(max(y_bin, 1), length(kde$y) - 1)
    
    # Assign density
    density_values[i] <- kde$z[x_bin, y_bin]
  }
  
  return(density_values)
}

