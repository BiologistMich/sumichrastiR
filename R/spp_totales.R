#' @author Biol. Michell Romero - Coordinador del área de Monitoreo de aves TCT 🦉
#' @title Cálculo de especies totales en todos los métodos y proyectos.
#' @description Esta función selecciona la columna Especie de todas las hojas y archivos asignados para regresar un .xlsx con spp. únicas.
#' @param lista_rutas  ruta a la carpeta donde se encuentran las distintas bases de datos (proyectos) en .xlsx
#' @param hojas  hojas que contienen la columna "Especie" para extraer taxonomía.
#' @return Un archivo .xlsx con el inventario de spp. totales
#' @importFrom dplyr filter mutate select group_by summarise arrange desc n_distinct
#' @importFrom janitor adorn_totals
#' @importFrom openxlsx write.xlsx
#' @importFrom writexl write_xlsx 
#' @importFrom purrr map list_rbind
#' @importFrom readxl read_excel
#' @importFrom tools file_path_sans_ext
#' @export
spp.totales <- function(lista_rutas, hojas){

   lapply(lista_rutas, function(archivo) {
     
 spp_totales <- purrr::map(hojas,
    ~{  readxl::read_excel(archivo, sheet = .x) |> 
         dplyr::filter(dplyr::between(Fecha_hora, fecha_inicio, fecha_fin)) |>
         dplyr::select(Proyecto,Especie) |> 
         dplyr::mutate(Especie = as.character(Especie),
                       Proyecto = as.character(Proyecto)) |> 
         dplyr::filter(Especie != "Z sin registro") |>  
         dplyr::arrange(Especie)
    }) |> purrr::list_rbind()
 
spp_final <- spp_totales |> 
dplyr::distinct(Especie) |> 
dplyr::arrange(Especie)

nombre_archivo <- paste0("Spp_totales_de_",
                        unique(spp_totales$Proyecto)[1], ".xlsx")

writexl::write_xlsx(
      list(spp_totales = spp_final),
           path = nombre_archivo
    )
    return(nombre_archivo)
   })
   }