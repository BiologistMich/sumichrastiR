#' @author Biol. Michell Romero - Coordinador del área de Monitoreo de aves TCT 🦉
#' @title Cálcular resultados del método "Conductas de vuelo"
#' @description
#' Aplica calculos de vuelos por categoría de riesgo, número de registros por tipo de vuelo así como por dirección de vuelo.
#' @param CV_lista lista con dataframes de distintos proyectos de una hoja (ej: CV_PEEM; CV_OAX; CV_BH).
#' @return Archivos xlsx con los resultados por proyecto.
#' @export
#' @importFrom dplyr filter select group_by summarise mutate arrange n_distinct across
#' @importFrom janitor adorn_totals
#' @importFrom tidyr pivot_longer pivot_wider drop_na
#' @importFrom openxlsx write.xlsx
#' @importFrom duckplyr as_duckplyr_df
calc.cv <- function(CV_lista) {

  lapply(CV_lista, function(df) {

  #----------------------------------------------------------------
   # Convertir a duckplyr para optimización y velocidad
    # 1. VERIFICAR si ya es un duck frame. Si lo es, usarlo directamente.
    if (inherits(df, "duckplyr_df")) { #función de R verifica si un objeto (df) pertenece a una clase específica
        df_db <- df
    } else {
        # 2. Si es un dataframe normal (la mayoría de las veces), convertirlo.
        df_db <- df %>% duckplyr::as_duckplyr_df() |> 
          dplyr::mutate(
        dplyr::across(
      .cols = c("Percha_Linea", "Ind_percha_linea", "Deslizamiento", "Cicleo", 
                "Directo", "Otro", "Forrajeo", "Registros", "Altura_m", 
                "Distancia_WTG_LT", "Percha_Torre",
                "Percha_Torre"),
      .fns = as.integer  
    ),
    dplyr::across(
      .cols= c("Observaciones","Colision_Electrocu", "DV", "Categoria_RV"),
      .fns = as.character
    )
  )
    }
  #----------------------------------------------------------------
    
    riesgo_CV <- df_db |>
      dplyr::filter(dplyr::between(Fecha_hora, fecha_inicio, fecha_fin)) |>
      dplyr:: filter_out(Metodo == "INC") |> 
      dplyr::select(Especie, Categoria_RV, Registros) |> 
      dplyr:: filter(Especie != "Z sin registro") |> 
  dplyr:: group_by(Especie, Categoria_RV) |> 
  dplyr:: summarise(Interacciones = sum(Registros, na.rm = T)) |> 
  tidyr:: pivot_wider(id_cols= Especie, 
    values_from = Interacciones,
     names_from = Categoria_RV, 
     values_fill = 0) |> 
  janitor:: adorn_totals(where=c("row","col")) 
    
    tipo_vuelo <- df_db |> 
      dplyr::filter(dplyr::between(Fecha_hora, fecha_inicio, fecha_fin)) |>
      dplyr:: filter_out(Metodo == "INC") |> 
      dplyr:: select(Especie, Deslizamiento, Cicleo, Forrajeo, Directo, Otro, Registros) |> 
      dplyr:: filter(Especie != "Z sin registro") |> 
      tidyr:: pivot_longer(cols = c(Deslizamiento, Cicleo, Forrajeo, Directo, Otro),
        names_to = "Tipo_Vuelo",       # Nombre de la nueva columna de categorías
        values_to = "Interacciones") |>
    dplyr::filter(Interacciones > 0) |>
      dplyr:: group_by(Tipo_Vuelo) |> 
      dplyr:: summarise(Interacciones=sum(Interacciones, na.rm = T),
                       No_Especies = dplyr::n_distinct(Especie), .groups = "drop")
    
    direcciones_vuelo <- df_db |> 
  dplyr::filter(dplyr::between(Fecha_hora, fecha_inicio, fecha_fin)) |>
  dplyr:: filter_out(Metodo == "INC") |> 
  dplyr::select(Especie,DV, Registros) |> 
  dplyr:: filter(Especie != "Z sin registro") |> 
  dplyr::group_by(DV) |> 
  dplyr::summarise(interacciones=sum(Registros),
            Especies= dplyr::n_distinct(Especie)) |> 
  tidyr::drop_na(DV) |> 
  dplyr::filter(DV !="-") |> 
  janitor::adorn_totals(where = "row")
    
    nombre_archivo <- paste0("Resul_CV_", unique(df$Proyecto)[1], ".xlsx")

     openxlsx::write.xlsx(
      list(RV = riesgo_CV,
        tipo_vuelo   = tipo_vuelo,
       DV = direcciones_vuelo),
      file = nombre_archivo,
      overwrite = TRUE)

    return(nombre_archivo)
  })
}