.isSingleString <- function(input) {
    is.character(input) && length(input) == 1L
    ## && !is.na(input)
}

#' @title Hacer mean y sd sobre un vector
#'
#' @description Toma un variable numerico y devuelve el promedio `mean` y
#'      desviacion estandar `sd`
#'
#' @param variable Un vector numerico
#'
#' @param na.rm (TRUE) eliminar valores NA cuando resumiendo los datos
#'
#' @param nombre (opcional) sequencia de caracteres para el nombre de el
#'     vector `variable`
#'
#' @param digitos Mostrar este numero de digitos en la salida
#'
#' @importFrom stats sd
#' @examples
#'
#' data(mtcars)
#'
#' meansd(mtcars$mpg)
#'
#' @export
meansd <- function(variable, na.rm = TRUE, nombre = NULL, digitos = 2) {

    if (is.null(nombre)) {
        nombre <- as.character(substitute(variable))
        nombre <- nombre[[length(nombre)]]
    }

    # verificacion de typo
    stopifnot(.isSingleString(nombre))

    variable <- as.numeric(variable)
    m <- round(mean(variable, na.rm = na.rm), digitos)
    stddev <- round(sd(variable, na.rm = na.rm), digitos)

    matrix(
        paste0(m, " (", stddev, ")"), ncol = 1L,
        dimnames = list(nombre, "M (SD)")
    )
}

