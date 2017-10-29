
## TODO
## proper selection of foreground layer
# proper color bar (looks ugly)
## cross hair
# layout options ('triangle', '1row')
## reusable components
## break out into neuroviz package

color_map <- ColorMaps$new()

wrap_slider <- function(...) {
  div(style = "height: 85px; padding: 0px 0px; font-size: 12px;",  
      sliderInput(...))
}

background_panel <- function(vol) {
  maxval <- signif(max(vol),3)
  minval <- signif(min(vol),3)
  
  menuItem("Background",icon=icon("adjust"), startExpanded=TRUE,
           wrap_slider("background_range", "Intensity Range", ticks=FALSE, 
                       min=minval, max=maxval, value=c(minval, maxval)),
           div(style="display: inline-block;vertical-align:top; width: 130px;",
               selectInput("background_col", "Color Map:",color_map$get_map_names(), "grayscale")),
           div(style="display: inline-block;vertical-align:top; width: 100px;",
               numericInput(inputId="background_col_size", label="Size: ", value = 256, min=2, max=256))
         #plotOutput("background_colorbar", width="100%", height=50)
           
  )
}

foreground_panel <- function(vol) {
    maxval <- signif(max(vol),3)
    minval <- signif(min(vol),3)
    
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
                 numericInput(inputId="foreground_col_size", label="Size: ", value = 20, min=2, max=256)))
}



slice_box <- function(title, id, slice_range, sid, height=300, width=4) {
  box(title=title, plotOutput(id, height=height, click = paste0(id, "_click")), 
      sliderInput(sid, "Slice:", 
                  slice_range[1],
                  slice_range[2],
                  ticks=FALSE,
                  median(round(c(slice_range[1], 
                                 slice_range[2])))), width=width, 
      
      solidHeader=TRUE, status="primary", background="black", align="center")
  
}


create_overlay <- function(...) {
  
  vlist <- list(...)
  
  axial_overlay <- do.call(Overlay$new, lapply(vlist, Layer$new, view="LPI"))
  coronal_overlay <- do.call(Overlay$new, lapply(vlist, Layer$new, view="LIP"))
  sagittal_overlay <- do.call(Overlay$new, lapply(vlist, Layer$new, view="PIL"))
  
  gen_el <- function(overlay, vname, num) {
    vspace <- overlay$view_space
    range <- bounds(vspace)[3,]
    list(
      overlay=overlay,
      zrange=range,
      start_slice= median(seq(range[1], range[2])),
      vrange=c(1, overlay$zdim()),
      view_name=vname,
      view_num=num
    )
  }
  
  list(
    axial=gen_el(axial_overlay, "axial", 3),
    coronal=gen_el(coronal_overlay, "coronal", 2),
    sagittal=gen_el(sagittal_overlay, "sagittal", 1)
  )
}

ortho_plot <- function(..., height=300) {
  
  overlay_set <- create_overlay(...)
  axial_overlay <- overlay_set$axial$overlay
  
  gen_layer_selection <- function() {
    if (axial_overlay$length() > 2) {
      selectInput("layer_selection", "Overlay Image: ", axial_overlay$names(), 
                  selected=overlay_set$axial$overlay$names()[1])
    } 
  }
  
 
  body <- dashboardBody(
    
    fluidRow(
      slice_box("Axial", "axial_plot", overlay_set$axial$vrange, "ax_slider", width=6),
      slice_box("Sagittal", "sagittal_plot", overlay_set$sagittal$vrange, "sag_slider", width=6)
    ),
    fluidRow(
      slice_box("Coronal", "coronal_plot", overlay_set$coronal$vrange, "cor_slider",width=6),
      box(title="Color", width=6, solidHeader=TRUE, status="primary", background="black", align="center",
          column(5,
            #box(title="Color", width=2, solidHeader=TRUE, status="primary", background="black", align="center",
            plotOutput("colorbar")),
          column(7, offset=0,
            #box(title="Info", width=4, solidHeader=TRUE, status="primary", background="black", align="center",
             div(style="text-align:left; padding:0px;width:100%;", 
               verbatimTextOutput("crosshair_loc"),
               verbatimTextOutput("voxel_loc"),
               verbatimTextOutput("bg_val"),
               verbatimTextOutput("fg_val")))
      )
      
    )
  )
  
  ui <- dashboardPage(
    
    dashboardHeader(title = "Ortho Plot"),
    
    dashboardSidebar(
      sidebarMenu(
        background_panel(axial_overlay$layers[[1]]$vol),
        if (length(axial_overlay$layers) > 1) foreground_panel(axial_overlay$layers[[2]]$vol),
        if (length(axial_overlay$layers) > 2) gen_layer_selection()
      )),
    body
    
  )
  
  
  server <- function(input, output, session) {
    
    rvs <- reactiveValues(
      axial_slice = NULL,
      coronal_slice = NULL,
      sagittal_slice = NULL,
      axial_plot_click = NULL,
      coronal_plot_click = NULL,
      sagittal_plot_click = NULL,
      axial_frame=NULL,
      sagittal_frame=NULL,
      coronal_frame=NULL,
      crosshair=c(0,0,0),
      voxel=c(1,1,1),
      fg_voxel=c(1,1,1),
      bg_colormap=NULL,
      fg_colormap=NULL
    )
    
    click_to_z <- function(x,y, d, ov_source, ov_dest) {
      vox <- c(x,y)  * d
      
      ## convert from view_space to voxel space 
      gg_native <- gridToGrid(ov_source$view_space, matrix(c(vox,0), ncol=3))
    
      ## convert to view space of destination
      gg_coord <- gridToCoord(ov_dest$view_space, gg_native)
      
      ## convert back to voxel space of destination
      gg_vox <- coordToGrid(ov_dest$view_space, gg_coord)
      
      ## get the coordinate of the z-axis
      gg_vox[which_dim(ov_dest$view_space, ov_dest$layers[[1]]$view_axes@k)]
    }
    
    convert_click <- function(x,y, cfun, slice) {
      xy <- cfun$convert_xy(x,y)
      d <- dim(slice)
      list(x=xy[1],y=xy[2], d=d)
    }
    
    #observeEvent(input$foreground_col, {
    #  rvs$fg_colormap  
    #  
    #})
    
    observeEvent(input$axial_plot_click, {
      print(paste("X:", input$axial_plot_click$x))
      print(paste("Y:", input$axial_plot_click$y))
      xyd <- convert_click(input$axial_plot_click$x, input$axial_plot_click$y, 
                           rvs$axial_frame, rvs$axial_slice$slices[[1]]$slice)
    
      z_sag <- click_to_z(xyd$x,xyd$y,xyd$d,overlay_set$axial$overlay, overlay_set$sagittal$overlay)
      z_cor <- click_to_z(xyd$x,xyd$y,xyd$d,overlay_set$axial$overlay, overlay_set$coronal$overlay)
      
      updateSliderInput(session, "sag_slider", value = z_sag)
      updateSliderInput(session, "cor_slider", value = z_cor)
    })
    
    observeEvent(input$coronal_plot_click, {
      xyd <- convert_click(input$coronal_plot_click$x, input$coronal_plot_click$y,
                           rvs$coronal_frame, rvs$coronal_slice$slices[[1]]$slice)
      
      z_ax <- click_to_z(xyd$x,xyd$y,xyd$d,overlay_set$coronal$overlay, overlay_set$axial$overlay)
      z_sag <- click_to_z(xyd$x,xyd$y,xyd$d,overlay_set$coronal$overlay, overlay_set$sagittal$overlay)
      
      updateSliderInput(session, "ax_slider", value = z_ax)
      updateSliderInput(session, "sag_slider", value = z_sag)
    })
    
    observeEvent(input$sagittal_plot_click, {
      xyd <- convert_click(input$sagittal_plot_click$x, input$sagittal_plot_click$y,
                           rvs$sagittal_frame, rvs$sagittal_slice$slices[[1]]$slice)
      
      z_ax <- click_to_z(xyd$x,xyd$y,xyd$d,overlay_set$sagittal$overlay, overlay_set$axial$overlay)
      z_cor <- click_to_z(xyd$x,xyd$y,xyd$d,overlay_set$sagittal$overlay, overlay_set$coronal$overlay)
     
      updateSliderInput(session, "ax_slider", value = z_ax)
      updateSliderInput(session, "cor_slider", value = z_cor)

    })
    
    gen_render_plot <- function(view, slider_id, rval, plot_id) {
      vspace=view$overlay$view_space
      
      renderPlot({
      
        width <- session$clientData[[paste0("output_", plot_id, "_width")]]
        height <- session$clientData[[paste0("output_", plot_id, "_height")]]
        
        ## the voxel index of background volume to display
        ind <- input[[slider_id]]
        
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
        
        ## ind is in grid space of RPI, need to convert to view_space
        dnum <- which_dim(space(view$overlay$layers[[1]]$vol), view$overlay$view_axes@k)
        vox <- rvs[["voxel"]]
        vox[dnum] <- ind
        rvs[["voxel"]] <- vox
        
        coord <- gridToCoord(view$overlay$view_space, vox)
        zpos <- coord[3]
        rvs$crosshair[view$view_num] <- zpos
        
        slice <- view$overlay$render_slice(zpos, 1:view$overlay$length(), width, height)
        rvs[[paste0(view$view_name, "_slice")]] <- slice
        
        if (length(slice$slices) > 1) {
          fgvol = view$overlay$layers[[2]]$vol
          vox <- round(coordToGrid(space(fgvol), rvs$crosshair))
          rvs[["fg_voxel"]] <- vox
        }
         
        info <- slice$draw(marker_pos=rvs$crosshair)
        rvs[[paste0(view$view_name, "_frame")]] <- info

        
      })
    }
      
    output$axial_plot <- gen_render_plot(overlay_set$axial, "ax_slider", axial_slice, "axial_plot")
    
    output$coronal_plot <- gen_render_plot(overlay_set$coronal, "cor_slider", coronal_slice, "coronal_plot")
    
    output$sagittal_plot <- gen_render_plot(overlay_set$sagittal, "sag_slider", sagittal_slice, "sagittal_plot")
    
    output$colorbar <- if (length(axial_overlay$layers) > 1) {
      renderPlot({
        width <- session$clientData[[paste0("output_foreground_colorbar_width")]]
        height <- session$clientData[[paste0("output_foreground_colorbar_height")]]
        cmap <- color_map$get_colors(input[["foreground_col"]], input[["foreground_col_size"]])
        color_bar(cmap, input[["foreground_range"]])
      })
    } else {
      renderPlot({
        width <- session$clientData[[paste0("output_foreground_colorbar_width")]]
        height <- session$clientData[[paste0("output_foreground_colorbar_height")]]
        cmap <- color_map$get_colors(input[["background_col"]], input[["background_col_size"]])
        color_bar(cmap, input[["background_range"]])
      })
    }
    
    output$crosshair_loc <- renderText({ paste0("[xyz]:", 
                                               "(", round(rvs$crosshair[1]),
                                               ",", round(rvs$crosshair[2]),
                                               ",", round(rvs$crosshair[3]), ")") })
    
    output$voxel_loc <- renderText({ paste0("[ijk]:", 
                                                "(", rvs$voxel[1],
                                                ",", rvs$voxel[2],
                                                ",", rvs$voxel[3], ")") })
    
    output$bg_val <- renderText({ paste0("[bg]:", axial_overlay$layers[[1]]$vol[rvs$voxel[1], rvs$voxel[2], rvs$voxel[3]]) })
    
    output$fg_val <- if (length(axial_overlay$layers) > 1) {
                        renderText({ paste0("[fg]:", 
                                         signif(axial_overlay$layers[[2]]$vol[rvs$fg_voxel[1], rvs$fg_voxel[2], rvs$fg_voxel[3]],3)) })
    } else { renderText({ paste0("[fg]: --") }) }
  }
  
  shinyApp(ui = ui, server = server)
}

# Function to plot color bar
color_bar <- function(lut, yrange=c(0,100)) {
  
  par(bg="gray6")
  #par(mar = c(5.1,4.1,4.1,2.1))
  par(mar = c(2,2.5,2.5,1.4))
  plot.new()
  plot.window(xlim=c(0,1), ylim=yrange, xaxs="i", yaxs="i") 
  axis(2, signif(seq(yrange[1],yrange[2], length.out=4),3), col="white", col.axis="white", cex.axis=1)
  
  strip_h <- (yrange[2] - yrange[1])/length(lut) 
  
  for (i in 1:length(lut)) {
    ymin <- yrange[1] + ((i-1) * strip_h)
    rect(xleft=0, ybottom=ymin, ytop=ymin+strip_h, xright=1, col=lut[i], border=lut[i])
     #grid.rect(x= unit((i-1)*strip_w, "points"), y=unit(.5,"npc"), width=unit(strip_w, "points"), height=unit(1, "npc"), gp=gpar(fill=lut[i]))
  }
  
  #dev.off()
  #list(src=outfile, alt="color bar")
  
}

# output$foreground_colorbar <- renderPlot({
#   width <- session$clientData[[paste0("output_foreground_colorbar_width")]]
#   height <- session$clientData[[paste0("output_foreground_colorbar_height")]]
#   color_bar(rainbow(25), range(axial_overlay$layers[[2]]$vol))
# })
# 
# output$background_colorbar <- renderPlot({
#   width <- session$clientData[[paste0("output_background_colorbar_width")]]
#   height <- session$clientData[[paste0("output_background_colorbar_height")]]
#   color_bar(axial_overlay$layers[[1]]$get_color_map(), range(axial_overlay$layers[[1]]$vol))
# })

