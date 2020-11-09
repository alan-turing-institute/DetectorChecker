#' Assert that a file exists and then remove it
#'
#' @param file_path A file path
expect_file_exists <- function(file_path){

  expect_true(file.exists(file_path))
  if (file.exists(file_path)) file.remove(file_path)

}
