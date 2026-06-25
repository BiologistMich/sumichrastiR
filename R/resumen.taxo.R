#' @author Biol. Michell Romero - Coordinador del área de Monitoreo de aves TCT 🦉
#' @title Resumen de taxonomia por categorias de riesgo, estacionalidad y endemismos
#' @description
#' Describe y resume el número de especies protegidas, por categorías de estacionalidad y endemismos.
#' (minúsculas, sin acentos, sin espacios).
#' @param taxo_df listado de especies con sus respectivas categorias
#' @return Resumen del número de especies por cada categoría de riesgo, endemismo y gremios
#' @export
#' @importFrom dplyr group_by summarise n_distinct
#' @importFrom janitor adorn_totals
#' @importFrom openxlsx write.xlsx
resumen_taxo <- function(taxo_df) {

  # Función auxiliar que realiza la agrupación y el resumen (es el código repetido)
  func_resumen <- function(data, columna_agrupar) {
    data |>
      dplyr::group_by({{ columna_agrupar }}) |> # {{}} permite usar la columna sin comillas
      dplyr::summarise(Especies = dplyr::n_distinct(especie), .groups = "drop") |>
      janitor::adorn_totals("row")
  }

  # Crear una lista con todos los resultados
  resultados_finales <- list(
    Estacion = func_resumen(taxo_df, residencia),
    Endemismo = func_resumen(taxo_df, endemismo),
    NOM = func_resumen(taxo_df, nom059),
    UICN = func_resumen(taxo_df, iucn),
    CITES = func_resumen(taxo_df, cites)
  )

resultados <- openxlsx::write.xlsx(
    x = resultados_finales,
    file = "resumen_taxonomia_informe.xlsx",
    overwrite = TRUE)

return(resultados)

}
