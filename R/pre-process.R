#' Prepare text for analysis
#'
#' This function extracts the pre-processing steps from `pdf_load()` and
#' generalises them. The function can deal with both lines but also a single
#' string representing a whole file.
pre_process <- function(text_input, type = c("lines", "whole_text")) {

  type <- match.arg(type)
  text_lines <- switch(type,
                       lines = paste(text_input, collapse = " "),
                       whole_text = text_input
  )


  text_lines %>%
    paste(collapse = " ") %>%
    tokenizers::tokenize_sentences(simplify = TRUE) %>%
    tolower() %>%
    stringr::str_replace_all(pattern = ",", replacement = "") %>%
    .correct_tokenization()
}

