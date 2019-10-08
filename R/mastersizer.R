
#' @importFrom magrittr "%>%"

read_mastersizer <- function(path) {
  suppressWarnings(
    data <- readr::read_tsv(path) %>%
      dplyr::select(sample = `Sample Name`, d01 = `d (0.1)`, d05 = `d (0.5)`, d09 = `d (0.9)`, notes = `Operator Notes`, dplyr::starts_with("X"))
  )
  data_key <- data %>% dplyr::select(-dplyr::starts_with("X")) %>% dplyr::slice(-1L)
  data_dist <- data %>% dplyr::select(sample, dplyr::starts_with("X"))

  x_values <- data_dist %>%
    dplyr::slice(1L) %>%
    tidyr::gather(key = key, value = xval) %>%
    dplyr::slice(-1L) %>%
    dplyr::mutate(xval = as.numeric(xval))

  data_dist <- data_dist %>%
    dplyr::slice(-1L) %>%
    tidyr::gather(key, value, -sample) %>%
    dplyr::left_join(x_values) %>%
    dplyr::select(sample, size = xval, volume = value)

  return(list(data_key = data_key, data_dist = data_dist))
}


#'
#'

plot_mastersizer <- function(x, xlim = c(1, 2000)) {
  breaks <- 10^(-10:10)
  minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

  x %>%
    ggplot2::ggplot(ggplot2::aes(size, volume, color = sample)) +
      ggplot2::geom_line() +
      ggplot2::scale_x_log10(limits = xlim, labels = scales::comma, breaks = breaks, minor_breaks = minor_breaks) +
      ggplot2::theme_light() +
      ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "top") +
      ggplot2::labs(x = "Particle size (µm)", y = "Volume (%)") +
      ggplot2::annotation_logticks(sides = "b", color = "grey40")
}
