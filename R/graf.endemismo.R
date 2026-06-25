#' @author Biol. Michell Romero - Coordinador del área de Monitoreo de aves TCT 🦉
#' @title Gráfico de especies por categorias de endemismo
#' @description
#' Gráfico de pie o pastel para el número de especies por categoría de endemismo mediante la información resumida de "resumen.taxo"
#' @param data objeto con la hoja que contiene los datos de spp. por categoría de endemismo (únicas)
#' @param paleta_colores vector con la paleta de colores asignada a cada categoría de endemismo
#' @return  gráfico de pastel con el número de spp. por categoria de endemismo
#' @export
#' @importFrom ggplot2 ggplot scale_fill_manual theme element_text
#' @importFrom ggpie ggpie
graf.endemismo <- function(data, paleta_colores){
endemismo_graf <- ggpie::ggpie(
    data=data,
    group_key = "endemismo",
    count_type = "full",# No hace falta count_type, por defecto cuenta filas
    label_info = c("count", "ratio"),
    label_type = "horizon",
    label_pos = "out",
    label_size = 3,
    label_threshold = 0.07,
     nudge_x = 0.2, # Valores más pequeños acortan la línea.
  nudge_y = 0.2
)+
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.9, vjust = 0.5, size = 15)) +
  ggplot2::scale_fill_manual(name="Endemismos", values =paleta_colores)
  return (endemismo_graf)
}
