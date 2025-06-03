#' @importFrom xml2 read_xml write_xml xml_find_first xml_add_child xml_find_all
#' @importFrom purrr map_lgl
NULL

#' Duplicate general feedback into the grader info tag in moodle XML files
#' @export
#' @param file the xml file to read
#' @return NULL
set_grader_info <- function(file) {
  duplicate_feedback <- function(question, from='generalfeedback', to='graderinfo') {
    # check if we have general feedback
    if (question |> xml2::xml_find_first('generalfeedback') |> is.na()) {
      return(FALSE)
    }
    # check if we already have graderinfo
    if (!(question |> xml2::xml_find_first('graderinfo') |> is.na())) {
      message("question '", question |> xml2::xml_child("name") |> xml2::xml_text(), "' already has <graderinfo>\n")
      return(FALSE)
    }

    feedback <- question |> xml2::xml_find_first('generalfeedback') |> xml2::xml_child()
    question |> xml2::xml_add_child(.value='graderinfo')
    question |>
      xml2::xml_find_first('graderinfo') |>
      xml2::xml_add_child(feedback)
    TRUE
  }

  test <- xml2::read_xml(file)
  questions <- test |> xml2::xml_find_all('question[@type!="category"]')
  if(any(map_lgl(questions, duplicate_feedback))) {
    xml2::write_xml(test, file)
  }
  invisible()
}
