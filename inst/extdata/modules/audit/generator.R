library(ggplot2)
library(auditor)

make_audit_plot_model <- function(explainers, img_folder, options) {
  residuals_img_filename <- 'audit.svg'
  residuals_img_path <- file.path(img_folder, residuals_img_filename)
  rec_img_filename <- 'rec.svg'
  rec_img_path <- file.path(img_folder, rec_img_filename)

  models <- lapply(explainers, function(explainer) {
    audit(explainer)
  })

  file.create(residuals_img_path)
  file.create(rec_img_path)
  width <- getPlotWidth(options, "a.plot_width")

  pl <- do.call(plot, models)
  ggsave(residuals_img_path, pl, svg, width = width, height = 5, limitsize = TRUE)
  pl <- do.call(plot, c(models, type = "REC"))
  ggsave(rec_img_path, pl, svg, width = width, height = 5, limitsize = TRUE)

  list(residuals_img_filename = residuals_img_filename,
       rec_img_filename = rec_img_filename)
}

generator <- function(explainers, options, img_folder) {

  audit_img_filename <- make_audit_plot_model(explainers, img_folder, options)
  list(
    display_name='Audit',
    name='audit',
    data=list(
      residuals_img_filename = audit_img_filename$residuals_img_filename,
      rec_img_filename = audit_img_filename$rec_img_filename
    )
  )
}
