try(source("1.R/2.libraries.r"))
source("1.R/3.utils.R")

t <- list(family = "Panton", size = 14, color = 'white')

theme_dt <- function (color = "black") {

  if(color == "black") color2 <- "white" else color2 <- "black"

  theme(      text              = element_text (family = "Panton",
                                                color  = color2   ),
              rect              = element_rect (fill   = color    ),
              line              = element_line (color  = color    ),
              title             = element_text (
                face   = "bold"   ),

              legend.position   = "bottom",
              legend.title      = element_text (
                color = color2,
                face  = "bold"    ),
              legend.key        = element_rect (fill  = color     ),
              legend.background = element_rect (fill  = color     ),
              legend.text       = element_text (
                color = color2,
                face  = "bold"    ),

              panel.background  = element_rect (fill = color),
              panel.grid        = element_blank(),


              axis.text         = element_text (
                color = color2    ),
              plot.caption      = element_text (
                face  = "italic",
                color = color2)

  )
}

path_this_project <- strsplit(gsub("\\", "/", fileSnapshot()$path, fixed=TRUE), '/')[[1]]
num_main_dir_project <- which(str_detect(path_this_project,'\\d\\d\\d'))
  
# path_targets <- path_this_project
# path_targets[num_main_dir_project] <- "104.Targets"
# path_targets <- paste0(path_targets[1:num_main_dir_project], collapse = '/')

path_221 <- path_this_project
path_221[num_main_dir_project] <- "221.INDEX.EEB.DATA"
path_221 <- paste0(path_221[1:num_main_dir_project], collapse = '/')

# readr::write_file(str_replace(readr::read_file(pt <- "_targets.yaml"),
#                               "store:*.*211.*/_targets",
#                               paste0("store: ", str_remove(getwd(), "211.*"), "220.Electron.data/_targets")),
#                   file = pt)
