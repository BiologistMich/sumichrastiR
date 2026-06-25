#' @author Biol. Michell Romero - Coordinador del área de Monitoreo de aves TCT 🦉
#' @title Gráfico de las top 5 especies más abundantes
#' @description
#' Gráfico de barras para las cinco especies más abundantes
#' @param data objeto con la base de datos generada con la función data.top5
#' @param colores vector con la paleta de colores asignada a cada código de sp.
#' @return  gráfico de barras de las cinco especies más abundantes
#' @export
#' @importFrom ggplot2 ggplot aes theme_minimal scale_fill_manual theme element_text element_blank labs 
#' @importFrom ggpattern geom_col_pattern
#' @importFrom ggrepel geom_text_repel
graf.top5 <- function(data, colores) { #función que genera el gráfico con los datos de arriba

graf <- data |> 
  ggplot2::ggplot(ggplot2::aes(x = Especie, y = Individuos, fill = Especie)) +
  ggpattern::geom_col_pattern( 
    pattern = "circle",               # Cada Especie tiene un patrón distinto
    pattern_color = "white",           # Color de las líneas de la textura
    pattern_fill = "white",            # Color del relleno de la textura
    pattern_spacing = 0.05,            # Qué tan juntas están las rayas
    pattern_density = 0.1              # Grosor de la textura
  ) + 
  ggplot2::theme_minimal() + 
  ggplot2::scale_fill_manual(values = colores) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(color = "#3E5D58", size = 14, face = "bold"),
    axis.text.y = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 10, face = "bold", color = "white"),
    legend.position = "none",
    axis.title.x = ggplot2::element_text(color = "black"),  
    panel.grid.major = ggplot2::element_blank(), # Quita las líneas principales
    panel.grid.minor = ggplot2::element_blank()  # Quita las líneas secundarias
  ) +
  ggplot2::labs(x = NULL, y = NULL, fill = "Especie") +
  ggrepel::geom_text_repel(
    ggplot2::aes(label = Individuos),   # Mapeo directo a la columna
    size = 5,
    color = "#3E5D58",
    direction = "y",                    # Ajuste vertical
    nudge_y = 1,                        # Empuja el texto un poco hacia arriba de la barra
    segment.color = "grey50",
    min.segment.length = 0,             # Siempre dibuja la línea si hay movimiento
    fontface = "bold",
    box.padding = 0.2,
    show.legend = FALSE                 # Evita que aparezca una 'a' en la leyenda
  )

return(graf)
}