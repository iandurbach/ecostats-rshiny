#' @importFrom lubridate hour minute second
#' @importFrom purrr map
#' @importFrom shinyWidgets sliderTextInput
#' @noRd
generate_date_slider <- function(min_date, max_date) {
  # Generate all timestampts from min_date to max_date with step=second.
  time_labels <- seq(min_date, max_date, by="sec")
  # Convert time labels to HH:MM:SS strings.
  time_labels <- time_labels %>% purrr::map(\(t) paste0(hour(t), ':', minute(t), ':', second(t)))

  golem::print_dev(head(time_labels))
  golem::print_dev(tail(time_labels))
  return(
    sliderTextInput(
      "selected_scope_time_range",
      "Select Time Scope: ",
      time_labels,
      selected = c(time_labels[[1]], time_labels[[length(time_labels)]])
    )
  )
}

#' @noRd
checkboxColumn <- function(len) {
  inputs <- character(len)
  for(i in seq_len(len)) {
    inputs[i] <- paste0("<input id=", "'checkb_" , i, "' type='checkbox'>")
  }
  return(inputs)
}

#' Function to encode images as base64
#' @importFrom base64enc base64encode
#' @noRd
encode_image <- function(img_path) {
  ext <- tolower(tools::file_ext(img_path))
  mime <- switch(
    ext,
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    png = "image/png",
    gif = "image/gif",
    "image/png"
  )
  data_uri <- paste0("data:", mime, ";base64,", base64encode(img_path))
  return(data_uri)
}

#' @noRd
spectroImageColumn <- function(len) {
     inputs <- character(len)
    for(i in seq_len(len)) {
      inputs[i] <- paste0("<image id=", "'spectro_" , i, "' height='100' src='' alt='Loading'>")
    }
    return(inputs)
 }

# spectroImageColumn <- function(len, ns) {
#   inputs <- character(len)
#   for(i in seq_len(len)) {
#     inputs[i] <- as.character(imageOutput(ns(paste0("spectro_", i)), inline=TRUE))
#   }
#   return(inputs)
# }
