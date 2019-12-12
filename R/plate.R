#' plate
#' @title Interactively choose, knit, and show R Markdown Drafts.

#' @aliases plate
#' @keywords plate

#' @description This function provides non-RStudio users with alternative methods to interactively choose Rmd templates based upon already-existing packages on system, help users edit their Rmd drafts in a preferable text editor, quickly knit it and show the results simultaneously.

#' @export plate

#' @details
#' You can use this function to conveniently draft your R Markdown documents based on other templates provided by the available packages installed on your system.

#' @return invisible()

#' @examples
#' if(interactive()) {
#' # Use rmdplate::plate(), then follow the instructions.
#' }

#' @author JooYoung Seo, \email{jooyoung@psu.edu}


# List the template directories that are available for consumption.
list_template_dirs <- function() {

  # check each installed package for templates
  packages <- row.names(utils::installed.packages())
  for (pkg in packages) {

    # check to see if the package includes a template folder
    template_folder <- system.file("rmarkdown", "templates", package = pkg)
    if (fs::dir_exists(template_folder)) {

      # it does; list each template directory within the template folder
      template_dirs <- list.dirs(path = template_folder, recursive = FALSE)
      lapply(template_dirs, function(dir) {
        cat(pkg, "|", dir, "\n", sep = "")
      })
    }
  }
}

plate <- function() {
    if (interactive()) {
        # Making tidy object out of rmarkdown:::list_template_dirs:
        df <- tidyr::separate(tibble::tibble(utils::capture.output(list_template_dirs())), 
            1, c("package", "template"), sep = "[|]")
        
        # Pulling out package column:
        pkg <- tibble::rowid_to_column(dplyr::distinct(dplyr::select(df, package)), 
            "id")
        num_pkg <- nrow(pkg)
        if (num_pkg == 0) {
            stop("No package found for Rmd templates.")
        }
        pkg_message <- if (num_pkg > 1) {
            paste0(" packages have ")
        } else {
            paste0(" package has ")
        }
        user_input_pkg <- ""
        
        while (TRUE) {
            message(paste0(num_pkg, pkg_message, "been found for Rmd templates. Enter the package number to use: "))
            
            for (i in 1:num_pkg) {
                message(paste0(pkg[i, 1], ". ", pkg[i, 2]))
            }
            message("99. Quit")
            
            user_input_pkg <- readLines(n = 1)
            
            if (user_input_pkg == "") {
                next
            } else if(user_input_pkg %in% 1:num_pkg) {
                  pkg_select <- pkg[user_input_pkg, "package"]$package
} else {
if(user_input_pkg == "99") {
message("Good bye!")
return(invisible())
} else if(nrow(pkg[base::grepl(base::tolower(user_input_pkg), base::tolower(pkg$package)),]) == 0) {
stop("No match has been found for the given package name.")
} else {
pkg_select <- pkg[base::grepl(base::tolower(user_input_pkg), base::tolower(pkg$package)),]$package
}
}

                  templ <- tibble::rowid_to_column(dplyr::filter(df, package %in% pkg_select), 
                    "id")
                  num_templ <- nrow(templ)

if (num_templ == 0) {
                    stop("No template found.")
                  }
                  templ_message <- if (num_templ > 1) {
                    paste0(" templates have ")
                  } else {
                    paste0(" template has ")
                  }

pkg_select <- if(length(pkg_select) == 1) {
paste0(pkg_select)
} else if(length(pkg_select) == 2) {
paste0(pkg_select, collapse=" and ")
} else {
paste0(c(paste0(pkg_select[1:length(pkg_select)-1], collapse=", "), pkg_select[length(pkg_select)]), collapse=", and ")
}
                while (TRUE) { 
                  message(paste0(num_templ, templ_message, " been found under ", 
                    pkg_select, ". Enter the template number to use: "))
                  
                  for (i in 1:num_templ) {
                    message(paste0(templ[i, 1], ". ", base::basename(as.character(templ[i, 
                      3]))))
                  }
                  message("99. Quit")
                  message("Press \"B\" to go back to the previous menu.")
                                    
                  user_input_templ <- readLines(n = 1)
               

if (user_input_templ == "") {
                next
} else if (base::tolower(user_input_templ) == "b") {
                    break
                  } else if(user_input_templ == "99") {
message("Good bye!")
return(invisible())
} else if (!(user_input_templ %in% 1:num_templ)) {
message("Out of range; please enter again.")
                    next
                  } else {
                    templ_select <- base::basename(templ[templ$id == user_input_templ, ]$template)

while (TRUE) {
                    message("Enter the draft file name to use: ")
                    user_template <- readLines(n = 1)
                    
if(user_template == "") {
message("You must enter your file name to use.")
next
} else {
break
}
}

                    editor <- getOption("editor")

message(paste0("Your default text editor is set to: ", 
                      editor, ". Do you want to keep using it? "))

                    editor_option <- base::tolower(readLines(n = 1))

if(!(editor_option %in% c("", "yes", "true", "t"))) {
                      message("Choose your preferable text editor: ")
                      editor_path <- enc2native(file.choose())
                      options(editor = editor_path)
                    }

target_pkg <- templ[templ$id == user_input_templ, ]$package

                    file <- rmarkdown::draft(user_template, templ_select, target_pkg, 
                      create_dir = TRUE, edit = TRUE)
                    
                    while (TRUE) {
message("Do you want to render the Rmd file? (Press \"B\" to go back to the previous menu.)")
                      render_option <- base::tolower(readLines(n = 1))
                      
                      if (render_option %in% c("", "yes", "true", "t")) {
#                        setwd(base::dirname(file))

#file <- base::basename(file)

rendered_output <- lapply(rmarkdown::all_output_formats(file), function(x) {if(grepl("^(pagedown|xaringan|posterdown)", x)) {xaringan::infinite_moon_reader(file); message("Save your Rmd file; then the browser will automatically update its changed content accordingly."); return("servr")} else {utils::browseURL(rmarkdown::render(input = file, output_format = x)); return(x)}
})

if(unlist(rendered_output) %in% c("servr")) {
message("Good bye!")
                        return(invisible())
}

#                        setwd("..")
                      } 
else if(render_option == "b") {
break }
else {
message("Good bye!")
                        return(invisible())
                      }
                    }

                }  # while ends.
                # end
            }
        }
       
        
    } else {
        warning("This function is meant for use in interactive mode only.\n")
    }
    return(invisible(NULL))
}
# end
            
        
       