#' render_all
#' @title Interactively choose, knit, and show R Markdown Drafts.

#' @aliases render_all
#' @keywords render_all

#' @description This function provides non-RStudio users with alternative methods to interactively choose Rmd temrender_alls based upon already-existing packages on system, help users edit their Rmd drafts in a preferable text editor, quickly knit it and show the results simultaneously.

#' @export render_all

#' @details
#' You can use this function to conveniently draft your R Markdown documents based on other temrender_alls provided by the available packages installed on your system.

#' @return invisible()

#' @examples
#' if(interactive()) {
#' # Use rmdplate::render_all(), then follow the instructions.
#' }

#' @author JooYoung Seo, \email{jooyoung@psu.edu}

render_all <- function(input, output_format = "all", ...) {

render_method <- rmarkdown::yaml_front_matter(input = input)$knit
render_method <- ifelse(!is.null(render_method), lazyeval::lazy_eval(render_method), rmarkdown::render)
render_method_args <- methods::formalArgs(render_method)
first_arg <- render_method_args[1]
output_format_arg <- ("output_format" %in% render_method_args)

render_args <- list()
render_args[[first_arg]] <- input
if(output_format_arg) {
render_args[['output_format']] <- output_format
}
elipsis <- list(...)

render_results <- do.call(what = render_method, args = c(render_args, elipsis))

#if(auto_open) {
if(output_format_arg) {
lapply(render_results, utils::browseURL)
#print(render_args)
} else {
return(render_results)
#print(render_args)
}

#} else {
#return(render_results)
#}
}
