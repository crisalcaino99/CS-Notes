# Analisis APV 
# Cris Alcaino

A_1 = 1e5
A_0 = 1e6

r_nominal_anual = 0.07
inflacion_anual = 0.03

n = 50 * 12
Tope = 365e3

real_rate_annual <- function(r_nominal_anual, inflacion_anual){
  (1 + r_nominal_anual) / (1 + inflacion_anual) - 1
}

real_rate_monthly <- function(r_real_anual){
  (1 + r_real_anual)^(1/12) - 1 
}

r_real_anual <- real_rate_annual(r_nominal_anual, inflacion_anual)
i_real <- real_rate_monthly(r_real_anual)

V_A0_n_months <- function(i, A0, n){
  value = (1+i)^n * A0
  return(value)
}

V_A1_n_months <- function(i, A1, n){
  
  if (i == 0) return(A1 * n)
  
  value = ((1+i)^(n-1) - 1)/i * A1
  return(value)
}

B_y <- function(A1, Tope){
  return(min(0.15*12*A1, Tope))
}

B_at_n <- function(n, By, i){
  years <- floor(n/12)
  if (years <= 0) return(0)
  
  total <- 0
  for(y in 1:years){
    months_remaining <- n - 12*y
    total <- total + By*(1+i)^months_remaining
  }
  return(total)
}

Total_Value_Real <- function(n, A0, A1, Tope, r_nominal_anual, inflacion_anual){
  
  r_real_anual <- real_rate_annual(r_nominal_anual, inflacion_anual)
  i_real <- real_rate_monthly(r_real_anual)
  
  val_aporte_inicial <- V_A0_n_months(i_real, A0, n)
  val_aporte_mensual <- V_A1_n_months(i_real, A1, n)
  val_bono <- B_at_n(n, B_y(A1, Tope), i_real)
  
  return (val_aporte_inicial + val_aporte_mensual + val_bono)
}

resultado <- Total_Value_Real(
  n = n,
  A0 = A_0,
  A1 = A_1,
  Tope = Tope,
  r_nominal_anual = r_nominal_anual,
  inflacion_anual = inflacion_anual
)


print(r_real_anual)
print(i_real)
print(resultado)

# =======
library(plotly)
library(dplyr)
library(ggplot2)

A0_seq <- seq(0, 8e6, length.out = 60)
A1_seq <- seq(0, 3e5, length.out = 60)

# Total_Value_Real <- function(n, A0, A1, Tope, r_nominal_anual, inflacion_anual){
z_matrix <- outer(
  A0_seq, A1_seq,
  Vectorize(function(A0, A1){
    Total_Value_Real(n, A0, A1, Tope, r_nominal_anual, inflacion_anual)
  })
)

fig <- plot_ly(
  x = A0_seq,
  y = A1_seq,
  z = z_matrix
) |>
  add_surface(opacity = 0.9) |>
  layout(
    title = 'Valor final real del APV',
    scene = list(
      xaxis = list(title = 'Aporte inicial A0'),
      yaxis = list(title = 'Aporte mensual A1'),
      zaxis = list(title = 'Valor final real')
    )
  )

fig

# Anadimos tope de linea del bono del estado
A1_star <- Tope / (0.15 * 12)

line_df <- data.frame(
  A0 = A0_seq,
  A1 = rep(A1_star, length(A0_seq)),
  Z = sapply(A0_seq, function(a0){
    Total_Value_Real(n, a0, A1_star, Tope, r_nominal_anual, inflacion_anual)
  })
)

# Completar analisis
# Escalar a Dashboard?
fig <- fig |>
  add_trace(
    data = line_df, 
    x = ~A0,
    y = ~A1,
    z = ~Z,
    type = 'scatter3d',
    mode = 'lines',
    line = list(width = 6),
    name = 'Umbral tope bono'
  )

fig
