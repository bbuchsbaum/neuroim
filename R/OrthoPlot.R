
## TODO
## proper selection of foreground layer
## proper color bar
## cross hair
## layout options ('triangle', '1row')
## reusable components
## break out into neuroviz package

create_overlay <- function(...) {
  
  vlist <- list(...)
  
  axial_overlay <- do.call(Overlay$new, lapply(vlist, Layer$new, view="LPI"))
  coronal_overlay <- do.call(Overlay$new, lapply(vlist, Layer$new, view="LIP"))
  sagittal_overlay <- do.call(Overlay$new, lapply(vlist, Layer$new, view="AIL"))
  
  gen_el <- function(overlay) {
    vspace <- overlay$view_space
    range <- bounds(vspace)[3,]
    list(
      overlay=overlay,
      zrange=range,
      start_slice= median(seq(range[1], range[2])),
      vrange=c(1, overlay$zdim())
    )
      
  }
  
  list(
    axial=gen_el(axial_overlay),
    coronal=gen_el(coronal_overlay),
    sagittal=gen_el(sagittal_overlay)
  )
}

ortho_plot <- function(...) {
  overlay_set <- create_overlay(...)
  axial_overlay <- overlay_set$axial$overlay
  
  height <- 300
  #width <- "100%"
  gen_slice_box <- function(title, id, view, sid) {
    box(title, plotOutput(id, height=height, click = "plot_click"), 
        sliderInput(sid, "Slice:", 
                    view$vrange[1],
                    view$vrange[2],
                    ticks=FALSE,
                    median(round(c(view$vrange[1], 
                                   view$vrange[2])))), width=4, 
        solidHeader=TRUE, status="primary", background="black", align="center")

  }
  
  color_map <- ColorMaps$new()
  
  gen_background_panel <- function(vol) {
    maxval <- max(vol)
    minval <- min(vol)
    menuItem("Background",icon=icon("adjust"), startExpanded=TRUE,
             sliderInput("background_range", "Intensity Range", ticks=FALSE, 
                         min=minval, max=maxval, value=c(minval, maxval)),
             div(style="display: inline-block;vertical-align:top; width: 130px;",
                 selectInput("background_col", "Color Map:",color_map$get_map_names(), "grayscale")),
             div(style="display: inline-block;vertical-align:top; width: 100px;",
                 numericInput(inputId="background_col_size", label="Size: ", value = 256, min=2, max=256))
          
    )
                 
  }
  
  wrap_slider <- function(...) {
    div(style = "height: 85px; padding: 0px 0px; font-size: 12px;",  
       sliderInput(...))
  }
  
  gen_foreground_panel <- function() {
    
    if (axial_overlay$length() > 1) {
      vol <- axial_overlay$layers[[2]]$vol
      maxval <- max(vol)
      minval <- min(vol)
      menuItem("Foreground",icon=icon("adjust"),
                 
             wrap_slider(inputId="foreground_range", label="Intensity Range", ticks=FALSE, 
                         min=minval, max=maxval, value=c(minval, maxval)),
             wrap_slider(inputId="foreground_threshold", label="Threshold", ticks=FALSE, 
                         min=minval, max=maxval, value=c((minval+maxval)/2, (minval+maxval)/2)),
             wrap_slider(inputId="foreground_opacity", label="Opacity", ticks=FALSE, 
                         min=0, max=1, value=1),
             div(style="display: inline-block;vertical-align:top; width: 130px;",
                 selectInput("foreground_col", "Color Map:",color_map$get_map_names(), "rainbow")),
             div(style="display: inline-block;vertical-align:top; width: 100px;",
                 numericInput(inputId="foreground_col_size", label="Size: ", value = 20, min=2, max=256)),
             
             plotOutput("foreground_colorbar", width="100%", height=50))
    } else {
      NULL
    }
  }
  
  gen_layer_selection <- function() {
    if (axial_overlay$length() > 2) {
      selectInput("layer_selection", "Overlay Image: ", axial_overlay$names(), 
                  selected=overlay_set$axial$overlay$names()[1])
    } else {
      NULL
    }
    
  }
  
  body <- dashboardBody(
    fluidRow(
      gen_slice_box("Axial", "axial_plot", overlay_set$axial, "ax_slider"),
      gen_slice_box("Coronal", "coronal_plot", overlay_set$coronal, "cor_slider"),
      gen_slice_box("Sagittal", "sagittal_plot", overlay_set$sagittal, "sag_slider")

    )
  )
  
  ui <- dashboardPage(
    dashboardHeader(title = "Column layout"),
    dashboardSidebar(
      sidebarMenu(
        gen_background_panel(axial_overlay$layers[[1]]$vol),
        gen_foreground_panel(),
        gen_layer_selection()
      )),
    body
  )
  
  
  server <- function(input, output, session) {

    axial_slice <- reactiveVal()
    coronal_slice <- reactiveVal()
    sagittal_slice <- reactiveVal()
    
    observeEvent(input$plot_click, {
      message("click x", input$plot_click$x)
      message("click y", input$plot_click$y)
     
    })
    
    
    gen_render_plot <- function(view, slider_id, rval, plot_id) {
      vspace=view$overlay$view_space
      
      renderPlot({
        
        #browser()
        width <- session$clientData[[paste0("output_", plot_id, "_width")]]
        height <- session$clientData[[paste0("output_", plot_id, "_height")]]
        
        ind <- input[[slider_id]]
       
        zpos <- view$overlay$get_zpos(ind)
        
        view$overlay$set_irange(1, input[["background_range"]])
        csize1 <- input[["background_col_size"]]
        cols1 <- color_map$get_colors(input[["background_col"]], as.numeric(csize1))
        view$overlay$set_color_map(1, cols1)
        
        if (view$overlay$length() > 1) {
        
          view$overlay$set_irange(2, input[["foreground_range"]])
          view$overlay$set_threshold(2, input[["foreground_threshold"]])
        
          csize2 <- input[["foreground_col_size"]]
          cols2 <- color_map$get_colors(input[["foreground_col"]], as.numeric(csize2))
  
          view$overlay$set_color_map(2, cols2)
          view$overlay$set_alpha(2, input[["foreground_opacity"]])
        }
        
        slice <- view$overlay$render_slice(zpos, 1:view$overlay$length(), width, height)
        rval(slice)
        slice$draw()
      })
    }
      
    output$axial_plot <- gen_render_plot(overlay_set$axial, "ax_slider", axial_slice, "axial_plot")
    output$coronal_plot <- gen_render_plot(overlay_set$coronal, "cor_slider", coronal_slice, "coronal_plot")
    output$sagittal_plot <- gen_render_plot(overlay_set$sagittal, "sag_slider", sagittal_slice, "sagittal_plot")

    
    output$foreground_colorbar <- renderPlot({
      width <- session$clientData[[paste0("output_foreground_colorbar_width")]]
      height <- session$clientData[[paste0("output_foreground_colorbar_height")]]
      color_bar(rainbow(25), width, height)
    })

  }
  
  # Preview the UI in the console
  shinyApp(ui = ui, server = server)
}

# Function to plot color bar
color_bar <- function(lut, width=100, height=20) {
  print("color bar")
  print(width)
  print(height)
  
  #outfile=tempfile(fileext='.png')
  #png(outfile, width=width, height=height)
  grid.newpage()
  grid.rect(x=0, y=0, width=unit(width, "points"), height=unit(height, "points"), gp=gpar(fill="black"))
  
  strip_w <- width/(length(lut)-1)
  
  for (i in 1:length(lut)) {
     grid.rect(x= unit((i-1)*strip_w, "points"), y=unit(.5,"npc"), width=unit(strip_w, "points"), height=unit(1, "npc"), gp=gpar(fill=lut[i]))
  }
  
  #dev.off()
  #list(src=outfile, alt="color bar")
  
}



