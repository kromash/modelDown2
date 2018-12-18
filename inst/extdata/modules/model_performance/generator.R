library("DALEX")
library(ggplot2)

save_plot_image <- function(file_name, models, options){

  width <- getPlotWidth(options, "mp.plot_width")

  pl <- do.call(plot, models)
  ggsave(file_name, pl, svg, width = width, height = 5, limitsize = TRUE)
}

make_model_performance_plot_model <- function(explainers, img_folder, options) {
  img_filename <- 'model_performance.svg'
  img_box_filename <- 'model_performance_box.svg'
  img_path <- file.path(img_folder, img_filename)
  img_box_path <- file.path(img_folder, img_box_filename)

  models <- lapply(explainers, function(explainer) {
    model_performance(explainer)
  })

  file.create(img_path)
  width <- getPlotWidth(options, "mp.plot_width")

  pl <- do.call(plot, models)
  ggsave(img_path, pl, svg, width = width, height = 5, limitsize = TRUE)
  models$geom <- "boxplot"
  pl_box <- do.call(plot, models)
  ggsave(img_box_path, pl_box, svg, width = width, height = 5, limitsize = TRUE)

  list(img_filename = img_filename,
       img_box_filename = img_box_filename)
}

generator <- function(explainers, options, img_folder) {

  img_filename <- make_model_performance_plot_model(explainers, img_folder, options)
  list(
    display_name='Model Performance',
    name='model_performance',
    data=list(
      img_filename_mp1 = img_filename$img_filename,
      img_filename_mp2 = img_filename$img_box_filename
    )
  )
}
