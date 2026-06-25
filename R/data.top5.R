#' @author Biol. Michell Romero - Coordinador del área de Monitoreo de aves TCT 🦉
#' @title Cálcular las cinco especies más abundantes por proyecto
#' @description
#' Calcula las cinco especies más abundantes por proyecto listas en la estructura necesaria para graficar
#' @param data objeto con la base de datos
#' @param proyecto objeto con el código del proyecto (BH_PE, BH_LT)
#' @param periodo fechas del periodo que abarca el análisis con formato aaaa-mm-dd hh:mm:ss
#' @return los datos acomodados para la elaboración del gráfico
#' @export
#' @importFrom dplyr filter select group_by summarise mutate arrange n_distinct between
#' @importFrom openxlsx write.xlsx
#' @importFrom forcats fct_relevel fct_reorder fct_lump_n
data.top5 <- function(data, proyecto) {

  resul <- data |>
  dplyr::filter(Proyecto == proyecto, dplyr::between(Fecha_hora, fecha_inicio, fecha_fin)) |>
  dplyr::filter_out(Metodo == "INC") |>
  dplyr::select(Codigo_sp, Individuos) |>
  dplyr::mutate(
    Individuos = as.integer(Individuos),
    Codigo_sp = forcats::fct_lump_n(Codigo_sp, n = 5, w = Individuos, other_level = "Resto de las especies")) |>
  dplyr::group_by(Codigo_sp) |>
  dplyr::summarise(Individuos = sum(Individuos, na.rm = TRUE)) |>
  dplyr::mutate(Codigo_sp = forcats::fct_reorder(Codigo_sp, Individuos, .desc = TRUE), # Ordena todo por número
         Codigo_sp = forcats::fct_relevel(Codigo_sp, "Resto de las especies", after = Inf)) |> # Mueve el resto al final
  dplyr::arrange(Codigo_sp) |>
  dplyr::rename(Especie = Codigo_sp)
  return(resul)
}
