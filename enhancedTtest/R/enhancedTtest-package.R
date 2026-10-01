# =============================================================================
# PACKAGE-LEVEL IMPORTS
# File: R/enhancedTtest-package.R
# =============================================================================

#' @keywords internal
"_PACKAGE"

# ---- rlang -------------------------------------------------------------------
#' @importFrom rlang sym
NULL

# ---- stats -------------------------------------------------------------------
#' @importFrom stats t.test wilcox.test shapiro.test var.test
#' @importFrom stats median IQR sd quantile qnorm rnorm na.omit complete.cases
#' @importFrom stats aggregate setNames
NULL

# ---- utils -------------------------------------------------------------------
#' @importFrom utils globalVariables packageVersion
NULL

# ---- dplyr -------------------------------------------------------------------
#' @importFrom dplyr group_by summarise mutate filter arrange n
#' @importFrom dplyr %>%
NULL

# ---- car ---------------------------------------------------------------------
#' @importFrom car leveneTest
NULL

# ---- flextable ---------------------------------------------------------------
#' @importFrom flextable flextable theme_box fontsize font align bold
#' @importFrom flextable add_header_lines add_footer_lines body_add_flextable
#' @importFrom flextable save_as_docx htmltools_value
NULL

# ---- officer -----------------------------------------------------------------
#' @importFrom officer read_docx body_add_par
NULL


# ---- htmltools ---------------------------------------------------------------
#' @importFrom htmltools tags save_html HTML
NULL

# ---- nortest -----------------------------------------------------------------
#' @importFrom nortest ad.test
NULL