# =============================================================================
# EXPORT FUNCTIONS
# File: R/export_functions.R
# =============================================================================

#' Export APA Table to Word
#'
#' Exports an APA-formatted flextable to a Word document with proper
#' formatting. Attempts to use \code{flextable::save_as_docx()} first,
#' with a fallback to \code{officer} for older flextable versions.
#'
#' @param apa_table A flextable object containing the APA-formatted table
#' @param filename Character, output filename (should end in .docx)
#' @param title Character, optional title for the document
#'
#' @return Invisibly returns the path to the created file
#' @export
#'
#' @examples
#' \donttest{
#' result <- complete_ttest(
#'   iris[iris$Species != "virginica", ],
#'   Sepal.Length ~ Species
#' )
#' tmp <- tempfile(fileext = ".docx")
#' export_apa_to_word(result$apa_table, tmp)
#' }
export_apa_to_word <- function(apa_table,
                               filename = "ttest_results.docx",
                               title = "T-Test Results") {
  
  # Input validation
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("The 'flextable' package is required for Word export.", call. = FALSE)
  }
  
  if (!inherits(apa_table, "flextable")) {
    if (is.data.frame(apa_table)) {
      warning("'apa_table' is not a flextable. Converting from data.frame...",
              call. = FALSE)
      apa_table <- flextable::flextable(apa_table)
    } else {
      stop("'apa_table' must be a flextable or data.frame object.",
           call. = FALSE)
    }
  }
  
  if (!is.character(filename) || length(filename) != 1 || !nzchar(filename)) {
    stop("'filename' must be a non-empty character string.", call. = FALSE)
  }
  
  if (!grepl("\\.docx$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".docx")
  }
  
  # Primary method
  primary_result <- tryCatch({
    flextable::save_as_docx(apa_table, path = filename)
    message("\u2713 Word document saved to: ", filename)
    return(invisible(filename))
  }, error = function(e) e)
  
  # Fallback method
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop(
      "Failed to export to Word.\n",
      "  Primary method error: ", primary_result$message, "\n",
      "  Fallback requires 'officer' but it is not installed.",
      call. = FALSE
    )
  }
  
  fallback_result <- tryCatch({
    doc <- officer::read_docx()
    
    if (!is.null(title) && is.character(title) &&
        length(title) == 1 && nzchar(title)) {
      doc <- officer::body_add_par(doc, title, style = "heading 1")
      doc <- officer::body_add_par(doc, "")
    }
    
    doc <- flextable::body_add_flextable(doc, value = apa_table)
    print(doc, target = filename)
    message("\u2713 Word document saved to: ", filename, " (fallback method)")
    invisible(filename)
  }, error = function(e) e)
  
  if (inherits(fallback_result, "error")) {
    stop(
      "Failed to export to Word.\n",
      "  Primary method error:  ", primary_result$message, "\n",
      "  Fallback method error: ", fallback_result$message,
      call. = FALSE
    )
  }
  
  return(invisible(filename))
}


#' Export APA Table to HTML
#'
#' Exports an APA-formatted flextable to an HTML file.
#'
#' @param ft A flextable object containing the APA-formatted table
#' @param filename Character, output filename (should end in .html)
#' @param include_interpretation Logical, include interpretation section
#' @param interpretation_text Character, interpretation text to include
#'
#' @return Invisibly returns the path to the created file
#' @export
#'
#' @examples
#' \donttest{
#' result <- complete_ttest(
#'   iris[iris$Species != "virginica", ],
#'   Sepal.Length ~ Species
#' )
#' tmp <- tempfile(fileext = ".html")
#' export_apa_to_html(result$apa_table, tmp)
#' }
export_apa_to_html <- function(ft,
                               filename = "ttest_results.html",
                               include_interpretation = TRUE,
                               interpretation_text = NULL) {
  
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("The 'flextable' package is required for HTML export.", call. = FALSE)
  }
  
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    stop("The 'htmltools' package is required for HTML export.", call. = FALSE)
  }
  
  if (!inherits(ft, "flextable")) {
    if (is.data.frame(ft)) {
      warning("'ft' is not a flextable. Converting from data.frame...",
              call. = FALSE)
      ft <- flextable::flextable(ft)
    } else {
      stop("'ft' must be a flextable or data.frame object.", call. = FALSE)
    }
  }
  
  if (!is.character(filename) || length(filename) != 1 || !nzchar(filename)) {
    stop("'filename' must be a non-empty character string.", call. = FALSE)
  }
  
  if (!grepl("\\.html$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".html")
  }
  
  html_table <- tryCatch({
    flextable::htmltools_value(ft)
  }, error = function(e) {
    stop("Failed to convert flextable to HTML: ", e$message, call. = FALSE)
  })
  
  html_content <- htmltools::tags$html(
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$title("Statistical Results"),
      htmltools::tags$style(htmltools::HTML("
        body {
          font-family: 'Times New Roman', Times, serif;
          margin: 40px;
          line-height: 1.5;
          color: #333;
        }
        h1 { color: #222; border-bottom: 2px solid #444; padding-bottom: 8px; }
        h2 { color: #333; margin-top: 30px; }
        table {
          width: 100%;
          border-collapse: collapse;
          margin: 20px 0;
          font-size: 12pt;
        }
        th {
          background-color: #f2f2f2;
          font-weight: bold;
          padding: 10px;
          border: 1px solid #ddd;
          text-align: center;
        }
        td {
          padding: 8px;
          border: 1px solid #ddd;
          text-align: center;
        }
        tr:nth-child(even) { background-color: #f9f9f9; }
        .interpretation {
          background-color: #f8f9fa;
          border-left: 4px solid #007bff;
          padding: 15px;
          margin: 20px 0;
        }
        footer {
          margin-top: 40px;
          padding-top: 10px;
          border-top: 1px solid #ddd;
          font-size: 10pt;
          color: #666;
        }
      "))
    ),
    htmltools::tags$body(
      htmltools::tags$h1("Statistical Results"),
      
      if (include_interpretation && !is.null(interpretation_text) &&
          is.character(interpretation_text) && nzchar(interpretation_text)) {
        htmltools::tags$div(
          class = "interpretation",
          htmltools::tags$h2("Interpretation"),
          htmltools::tags$p(interpretation_text)
        )
      },
      
      htmltools::tags$h2("APA-Formatted Table"),
      html_table,
      
      htmltools::tags$footer(
        htmltools::tags$p(
          sprintf("Generated on %s by enhancedTtest v%s",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  as.character(utils::packageVersion("enhancedTtest")))
        )
      )
    )
  )
  
  tryCatch({
    htmltools::save_html(html_content, file = filename)
    message("\u2713 Results exported to HTML: ", filename)
    return(invisible(filename))
  }, error = function(e) {
    stop("Failed to save HTML file: ", e$message, call. = FALSE)
  })
}


#' Create Complete Report in Word
#'
#' Generates a comprehensive Word document containing descriptive
#' statistics, inferential tests, and interpretations for multiple
#' outcome variables.
#'
#' @param data A data frame containing the variables
#' @param formula_list A list of formulas (e.g., \code{list(y1 ~ group, y2 ~ group)})
#' @param group_var Character, name of the grouping variable
#' @param paired Logical, whether to perform paired tests
#' @param title Character, report title
#' @param filename Character, output filename (should end in .docx)
#' @param include_descriptives Logical, include descriptive statistics table
#' @param include_interpretations Logical, include APA interpretations
#'
#' @return Invisibly returns the path to the created file
#' @export
#'
#' @examples
#' \donttest{
#' formulas <- list(
#'   Sepal.Length ~ Species,
#'   Sepal.Width ~ Species
#' )
#' tmp <- tempfile(fileext = ".docx")
#' data_subset <- iris[iris$Species != "virginica", ]
#' data_subset$Species <- droplevels(data_subset$Species)
#' create_complete_report(
#'   data = data_subset,
#'   formula_list = formulas,
#'   group_var = "Species",
#'   filename = tmp
#' )
#' }
create_complete_report <- function(data,
                                   formula_list,
                                   group_var,
                                   paired = FALSE,
                                   title = "Statistical Analysis Report",
                                   filename = "apa_report.docx",
                                   include_descriptives = TRUE,
                                   include_interpretations = TRUE) {
  
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.", call. = FALSE)
  }
  
  if (!is.list(formula_list) || length(formula_list) == 0) {
    stop("'formula_list' must be a non-empty list of formulas.", call. = FALSE)
  }
  
  if (!is.character(group_var) || length(group_var) != 1 ||
      !(group_var %in% names(data))) {
    stop("'group_var' must be a column name in 'data'.", call. = FALSE)
  }
  
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The 'officer' package is required for report generation.",
         call. = FALSE)
  }
  
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("The 'flextable' package is required for report generation.",
         call. = FALSE)
  }
  
  if (!grepl("\\.docx$", filename, ignore.case = TRUE)) {
    filename <- paste0(filename, ".docx")
  }
  
  doc <- officer::read_docx()
  
  doc <- officer::body_add_par(doc, title, style = "heading 1")
  doc <- officer::body_add_par(doc,
                               paste("Generated:", format(Sys.Date(), "%B %d, %Y")))
  doc <- officer::body_add_par(doc, "")
  
  if (include_descriptives) {
    doc <- officer::body_add_par(doc, "Descriptive Statistics",
                                 style = "heading 2")
    
    desc_table <- tryCatch({
      create_descriptive_table(data, formula_list, group_var)
    }, error = function(e) {
      warning("Could not create descriptive table: ", e$message,
              call. = FALSE)
      NULL
    })
    
    if (!is.null(desc_table)) {
      doc <- flextable::body_add_flextable(doc, value = desc_table)
    }
    doc <- officer::body_add_par(doc, "")
  }
  
  doc <- officer::body_add_par(doc, "Inferential Statistics",
                               style = "heading 2")
  
  for (i in seq_along(formula_list)) {
    formula_i <- formula_list[[i]]
    var_name <- tryCatch(all.vars(formula_i)[1], error = function(e) NULL)
    
    if (is.null(var_name)) {
      warning("Skipping formula ", i, ": cannot extract variable name.",
              call. = FALSE)
      next
    }
    
    doc <- officer::body_add_par(
      doc,
      paste0(i, ". Analysis for: ", var_name),
      style = "heading 3"
    )
    
    result <- tryCatch({
      complete_ttest(data, formula_i, paired = paired)
    }, error = function(e) {
      warning("Analysis failed for ", var_name, ": ", e$message,
              call. = FALSE)
      NULL
    })
    
    if (is.null(result)) {
      doc <- officer::body_add_par(doc,
                                   paste("Analysis could not be completed for",
                                         var_name))
      doc <- officer::body_add_par(doc, "")
      next
    }
    
    if (!is.null(result$apa_table)) {
      doc <- flextable::body_add_flextable(doc, value = result$apa_table)
    }
    
    if (include_interpretations) {
      doc <- officer::body_add_par(doc, "Interpretation:",
                                   style = "heading 3")
      
      interpretation <- tryCatch({
        generate_apa_interpretation(result)
      }, error = function(e) {
        paste("Interpretation could not be generated:", e$message)
      })
      
      doc <- officer::body_add_par(doc, interpretation, style = "Normal")
    }
    
    doc <- officer::body_add_par(doc, "")
  }
  
  tryCatch({
    print(doc, target = filename)
    message("\u2713 Complete report exported to: ", filename)
    return(invisible(filename))
  }, error = function(e) {
    stop("Failed to save report: ", e$message, call. = FALSE)
  })
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' Create Descriptive Statistics Table
#'
#' Internal function to build a descriptive statistics table.
#'
#' @param data A data frame
#' @param formula_list A list of formulas
#' @param group_var Character, grouping variable name
#'
#' @return A flextable object
#' @keywords internal
#' @noRd
create_descriptive_table <- function(data, formula_list, group_var) {
  
  var_names <- vapply(formula_list, function(f) {
    all.vars(f)[1]
  }, character(1))
  
  groups <- levels(as.factor(data[[group_var]]))
  
  desc_table <- data.frame(Variable = var_names, stringsAsFactors = FALSE)
  
  for (g in groups) {
    desc_table[[paste0(g, " (M \u00B1 SD)")]] <- vapply(var_names, function(v) {
      subset_data <- data[data[[group_var]] == g, v]
      subset_data <- subset_data[!is.na(subset_data)]
      if (length(subset_data) > 0) {
        m <- mean(subset_data, na.rm = TRUE)
        s <- stats::sd(subset_data, na.rm = TRUE)
        sprintf("%.2f \u00B1 %.2f", m, s)
      } else {
        NA_character_
      }
    }, character(1))
    
    desc_table[[paste0(g, " (n)")]] <- vapply(var_names, function(v) {
      n <- sum(!is.na(data[data[[group_var]] == g, v]))
      as.character(n)
    }, character(1))
  }
  
  ft <- flextable::flextable(desc_table)
  ft <- flextable::theme_box(ft)
  ft <- flextable::fontsize(ft, size = 11)
  ft <- flextable::font(ft, fontname = "Times New Roman")
  ft <- flextable::align(ft, align = "center", part = "body")
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::bold(ft, part = "header")
  
  title <- "Table 1. Descriptive Statistics by Group"
  ft <- flextable::add_header_lines(ft, values = title, top = TRUE)
  ft <- flextable::fontsize(ft, size = 12, part = "header", i = 1)
  ft <- flextable::italic(ft, italic = TRUE, part = "header", i = 1)
  
  ft <- flextable::add_footer_lines(
    ft,
    values = "Note. M = mean; SD = standard deviation; n = sample size."
  )
  ft <- flextable::fontsize(ft, size = 10, part = "footer")
  
  return(ft)
}
