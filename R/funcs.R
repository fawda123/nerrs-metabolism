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