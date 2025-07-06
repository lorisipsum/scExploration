#' @importFrom magrittr %>%

# Helper Functions for Common UI Components ----

create_sidebar_inputs <- function(marker_choices, default_markers, assay_type = "RNA") {
  list(
    shiny::selectInput("Marker_1",
                       paste("Select", assay_type, "#1"),
                       marker_choices,
                       selected = default_markers$marker1),
    shiny::selectInput("Marker_2",
                       paste("Select", assay_type, "#2"),
                       marker_choices,
                       selected = default_markers$marker2),
    shiny::selectInput("color_by",
                       "Select Marker to Color By:",
                       marker_choices,
                       selected = default_markers$color_by),
    shiny::selectInput("daughter_plot",
                       "Show daughter plot",
                       c("Yes", "No"),
                       selected = "No"),
    shiny::selectInput("daughter_table",
                       "Show table based on daughter plot",
                       c("Yes", "No"),
                       selected = "No"),
    shiny::selectInput("Marker_3",
                       paste("Select", assay_type, "#1 for daughter plot"),
                       marker_choices,
                       selected = default_markers$marker3),
    shiny::selectInput("Marker_4",
                       paste("Select", assay_type, "#2 for daughter plot"),
                       marker_choices,
                       selected = default_markers$marker4),
    shiny::selectInput("color_by_1",
                       "Select marker to color for daughter plot by:",
                       marker_choices,
                       selected = default_markers$color_by_1),
    shiny::actionButton("close", "Close App")
  )
}

create_cluster_sidebar_inputs <- function(marker_choices, default_markers, assay_type = "RNA") {
  list(
    shiny::selectInput("Marker_1",
                       paste("Select", assay_type, "#1"),
                       marker_choices,
                       selected = default_markers$marker1),
    shiny::selectInput("Marker_2",
                       paste("Select", assay_type, "#2"),
                       marker_choices,
                       selected = default_markers$marker2),
    shiny::selectInput("color_by",
                       "Select Marker to Color By:",
                       marker_choices,
                       selected = default_markers$color_by),
    shiny::selectInput("daughter_plot_1",
                       "Show daughter plot",
                       c("Yes", "No"),
                       selected = "No"),
    shiny::selectInput("daughter_table_1",
                       "Show table based on first biplot",
                       c("Yes", "No"),
                       selected = "No"),
    shiny::selectInput("daughter_table_2",
                       "Show table based on daughter plot",
                       c("Yes", "No"),
                       selected = "No"),
    shiny::selectInput("Marker_3",
                       paste("Select", assay_type, "#1 for daughter plot"),
                       marker_choices,
                       selected = default_markers$marker3),
    shiny::selectInput("Marker_4",
                       paste("Select", assay_type, "#2 for daughter plot"),
                       marker_choices,
                       selected = default_markers$marker4),
    shiny::selectInput("color_by_1",
                       "Select marker to color for daughter plot by:",
                       marker_choices,
                       selected = default_markers$color_by_1),
    shiny::actionButton("close", "Close App")
  )
}

create_main_panel_biplot <- function() {
  shiny::mainPanel(
    plotly::plotlyOutput("distPlot"),
    shiny::br(),
    shiny::br(),
    shiny::conditionalPanel(
      condition = "input$daughter_plot == 'Yes'",
      plotly::plotlyOutput("distPlot_3")
    ),
    shiny::br(),
    shiny::br(),
    shiny::plotOutput("umap_plot"),
    shiny::br(),
    shiny::conditionalPanel(
      condition = "typeof output.cell_annotations !== 'undefined'",
      shiny::tableOutput("table")
    ),
    shiny::br(),
    shiny::actionButton("export_cells", "Export Selected Cells", class = "btn-primary"),
    shiny::br(),
    shiny::br(),
    shiny::verbatimTextOutput("export_status")
  )
}

create_main_panel_cluster <- function() {
  shiny::mainPanel(
    plotly::plotlyOutput("umap_plot_2"),
    shiny::br(),
    shiny::br(),
    plotly::plotlyOutput("distPlot_2"),
    shiny::br(),
    shiny::conditionalPanel(
      condition = "input$daughter_plot_1 == 'Yes'",
      plotly::plotlyOutput("distPlot_4")
    ),
    shiny::br(),
    shiny::conditionalPanel(
      condition = "typeof input.cell_annotations !== 'undefined'",
      shiny::tableOutput("table_2")
    ),
    shiny::br(),
    shiny::actionButton("export_cells_2", "Export Selected Cells", class = "btn-primary"),
    shiny::br(),
    shiny::br(),
    shiny::verbatimTextOutput("export_status_2")
  )
}

create_dashboard_ui <- function(title, marker_choices, default_markers, assay_type = "RNA") {
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = title),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("BiPlot Selection", tabName = "biplot", icon = shiny::icon("magnifying-glass-chart")),
        shinydashboard::menuItem("Cluster Selection", tabName = "cluster", icon = shiny::icon("diagram-project"))
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        shinydashboard::tabItem(
          tabName = "biplot",
          shiny::fluidRow(
            shiny::sidebarLayout(
              shiny::sidebarPanel(create_sidebar_inputs(marker_choices, default_markers, assay_type)),
              create_main_panel_biplot()
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "cluster",
          shiny::fluidRow(
            shiny::sidebarLayout(
              shiny::sidebarPanel(create_cluster_sidebar_inputs(marker_choices, default_markers, assay_type)),
              create_main_panel_cluster()
            )
          )
        )
      )
    )
  )
}

# Helper Functions for Server Logic ----

create_scatter_plot <- function(data, x_var, y_var, color_var, source_id = "plotly_select") {
  if (is.null(data) || nrow(data) == 0) {
    return(plotly::plotly_empty())
  }
  
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x_var, y_var, color = color_var,
                                                 size = color_var, alpha = color_var)) +
    ggplot2::scale_size_continuous(range = c(0.25, 1.75)) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.01, height = 0.01)) +
    viridis::scale_color_viridis()
  
  fig <- plotly::ggplotly(p, tooltip = "cell_barcodes", source = source_id)
  plotly::event_register(fig, event = "plotly_selected")
  return(fig)
}

create_umap_plot <- function(object, umap_object) {
  df <- methods::slot(object@reductions[[umap_object]], "cell.embeddings")
  df <- as.data.frame(df)
  colnames(df) <- c("umap_1", "umap_2")
  df$cell_barcodes <- rownames(df)
  
  p <- ggplot2::ggplot(df, ggplot2::aes(umap_1, umap_2)) + 
    ggplot2::geom_point(color = "#3C96E0", alpha = 1, size = 0.25)
  
  fig <- plotly::ggplotly(p, tooltip = "cell_barcodes", source = "plotly_select_2")
  plotly::event_register(fig, event = "plotly_selected")
  return(fig)
}

create_highlight_plot <- function(object, selected_rows, umap_object, highlight_color = "#a62f03") {
  if (is.null(selected_rows) || length(selected_rows) == 0) {
    full_meta <- object@meta.data %>% 
      dplyr::mutate(target_cells = "No Cells Selected")
    highlight_val <- "No Cells Selected"
    color <- "#aaaaaa"
  } else {
    full_meta <- object@meta.data %>% 
      dplyr::mutate(target_cells = dplyr::case_when(
        rownames(object@meta.data) %in% selected_rows ~ "Selected Cells", 
        TRUE ~ "No"
      ))
    highlight_val <- "Selected Cells"
    color <- highlight_color
  }
  
  rownames(full_meta) <- rownames(object@meta.data)
  object@meta.data <- full_meta
  
  return(scCustomize::Meta_Highlight_Plot(
    seurat_object = object, 
    meta_data_column = "target_cells",
    meta_data_highlight = highlight_val, 
    highlight_color = color,
    background_color = "#aaaaaa", 
    reduction = umap_object
  ))
}

create_summary_table <- function(object, selected_rows, cell_annotations) {
  # Handle empty or null selections
  if (is.null(selected_rows) || length(selected_rows) == 0) {
    return(data.frame(Status = "No cells selected"))
  }
  
  # Check if cell_annotations column exists
  if (!cell_annotations %in% colnames(object@meta.data)) {
    return(data.frame(Error = paste("Column", cell_annotations, "not found in metadata")))
  }
  
  # Create target cells column
  full_meta <- object@meta.data %>% 
    dplyr::mutate(target_cells = dplyr::case_when(
      rownames(object@meta.data) %in% selected_rows ~ "Selected Cells", 
      TRUE ~ "No"
    ))
  
  # Filter selected cells
  selected_meta <- full_meta %>% 
    dplyr::filter(target_cells == "Selected Cells")
  
  # Check if any cells were actually found
  if (nrow(selected_meta) == 0) {
    return(data.frame(Status = "Selected cell IDs not found in object"))
  }
  
  # Create summary by cell annotations
  tryCatch({
    summary_table <- selected_meta %>% 
      dplyr::group_by(.data[[cell_annotations]]) %>% 
      dplyr::summarise(n = dplyr::n(), .groups = 'drop')
    
    if (nrow(summary_table) == 0) {
      return(data.frame(Status = "No cells found for selected annotations"))
    }
    
    subset_total_count <- sum(summary_table$n)
    cell_type_list <- unique(summary_table[[cell_annotations]])
    
    # Get full counts for comparison
    full_cells_summary <- object@meta.data %>% 
      dplyr::filter(.data[[cell_annotations]] %in% cell_type_list) %>% 
      dplyr::group_by(.data[[cell_annotations]]) %>% 
      dplyr::summarise(n = dplyr::n(), .groups = 'drop')
    
    total_count <- sum(full_cells_summary$n)
    
    # Combine results
    summary_table <- dplyr::bind_cols(summary_table, full_cells_summary)
    colnames(summary_table) <- c("celltype_subset", "n_subset", "celltype_total", "n_total")
    
    # Add totals row
    summary_table <- summary_table %>%
      dplyr::bind_rows(tibble::tibble(
        celltype_subset = "Total # of Cells in Subset", 
        n_subset = subset_total_count,
        celltype_total = "Total # of Cells in Full Object", 
        n_total = total_count
      ))
    
    return(summary_table)
    
  }, error = function(e) {
    return(data.frame(Error = paste("Table generation failed:", e$message)))
  })
}

handle_plotly_selection <- function(event_data, data_reactive) {
  if (is.null(event_data)) return(NULL)
  
  selected_indices <- event_data$pointNumber + 1
  selected_data <- data_reactive() %>% 
    dplyr::filter(dplyr::row_number() %in% selected_indices)
  
  return(selected_data)
}

# Export Functions ----

export_selected_cells <- function(object, selected_rows, export_name = "selected_cells") {
  tryCatch({
    if (is.null(selected_rows) || length(selected_rows) == 0) {
      return(list(success = FALSE, message = "No cells selected for export"))
    }
    
    # Subset the Seurat object
    subset_object <- object[, selected_rows]
    
    # Assign to global environment with unique name
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    object_name <- paste0(export_name, "_", timestamp)
    
    assign(object_name, subset_object, envir = .GlobalEnv)
    
    # Return success message
    message <- paste0("Successfully exported ", length(selected_rows), " cells to '", 
                      object_name, "' in global environment")
    
    return(list(success = TRUE, message = message, object_name = object_name, n_cells = length(selected_rows)))
    
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Export failed:", e$message)))
  })
}

# Data Preparation Functions ----

prepare_rna_data <- function(object, rna_assay) {
  tryCatch({
    # Handle Seurat v5 vs v4 differences
    if (packageVersion("Seurat") >= "5.0.0") {
      # Check for multiple data layers
      data_layers <- SeuratObject::Layers(object, assay = rna_assay, search = "data")
      
      if (length(data_layers) > 1) {
        # Multiple data layers detected, join them
        object <- SeuratObject::JoinLayers(object, assay = rna_assay)
      }
      # Now extract data from the consolidated (or single) data layer
      object_data <- Seurat::GetAssayData(object = object, assay = rna_assay, layer = "data")
    } else {
      # For Seurat v4
      object_data <- Seurat::GetAssayData(object = object, slot = "data", assay = rna_assay)
    }
    
    summ <- Matrix::summary(object_data)
    summ <- data.frame(
      gene = rownames(object_data)[summ$i],
      barcode = colnames(object_data)[summ$j],
      Expression = summ$x
    )
    
    summ <- summ %>% 
      tidyr::pivot_wider(names_from = gene, values_from = Expression, values_fill = 0)
    
    list(
      data = summ,
      marker_choices = list(RNA = sort(colnames(summ)[-1])),
      success = TRUE
    )
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

prepare_adt_data <- function(object, rna_assay, adt_assay) {
  tryCatch({
    # Handle Seurat v5 vs v4 differences for ADT data
    if (packageVersion("Seurat") >= "5.0.0") {
      # Check for multiple data layers
      data_layers_adt <- SeuratObject::Layers(object, assay = adt_assay, search = "data")
      
      if (length(data_layers_adt) > 1) {
        # Multiple data layers detected, join them
        object <- SeuratObject::JoinLayers(object, assay = adt_assay)
      }
      # Now extract data from the consolidated (or single) data layer
      adt_data <- Seurat::GetAssayData(object = object, assay = adt_assay, layer = "data")
    } else {
      # For Seurat v4
      adt_data <- Seurat::GetAssayData(object = object, slot = "data", assay = adt_assay)
    }
    
    adt_data <- t(adt_data)
    adt_data <- as.data.frame(adt_data)
    colnames(adt_data) <- gsub("[^a-zA-Z0-9 ]", "_", colnames(adt_data))
    colnames(adt_data) <- gsub(" ", "_", colnames(adt_data))
    colnames(adt_data) <- paste0(colnames(adt_data), "_ADT")
    
    # Handle Seurat v5 vs v4 differences for RNA data
    if (packageVersion("Seurat") >= "5.0.0") {
      # Check for multiple data layers
      data_layers_RNA <- SeuratObject::Layers(object, assay = rna_assay, search = "data")
      
      if (length(data_layers_RNA) > 1) {
        # Multiple data layers detected, join them
        object <- SeuratObject::JoinLayers(object, assay = rna_assay)
      }
      # Now extract data from the consolidated (or single) data layer
      rna_data <- Seurat::GetAssayData(object = object, assay = rna_assay, layer = "data")
    } else {
      # For Seurat v4
      rna_data <- Seurat::GetAssayData(object = object, slot = "data", assay = rna_assay)
    }
    
    summ <- Matrix::summary(rna_data)
    summ <- data.frame(
      gene = rownames(rna_data)[summ$i],
      barcode = colnames(rna_data)[summ$j],
      Expression = summ$x
    )
    
    summ <- summ %>% 
      tidyr::pivot_wider(names_from = gene, values_from = Expression, values_fill = 0) %>%
      tibble::column_to_rownames("barcode")
    
    # Merge data
    combined_data <- merge(adt_data, summ, by = "row.names")
    
    list(
      data = combined_data,
      marker_choices = list(
        ADT = colnames(adt_data),
        RNA = sort(colnames(summ))
      ),
      success = TRUE
    )
  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}

#' @title Analyze scRNA-seq Data
#'
#' @param object Seurat Object
#' @param RNA_assay Assay name
#' @param umap_object Name of the UMAP or TSNE object
#' @param cell_annotations Name of the cell annotation column from meta.data
#'
#' @return A shiny dashboard with interactive plots and export functionality
#' @details The dashboard includes:
#' - Interactive biplot and UMAP selection
#' - Export selected cells as subset Seurat objects to global environment
#' - Summary tables of selected cell populations
#' @export
RNA_Checker <- function(object = NULL, RNA_assay = "RNA", umap_object = "umap", cell_annotations = "orig.ident") {
  
  # Validate inputs
  if (is.null(object)) {
    stop("Seurat object is required")
  }
  
  if (!RNA_assay %in% names(object@assays)) {
    stop(paste("Assay", RNA_assay, "not found in object"))
  }
  
  if (!umap_object %in% names(object@reductions)) {
    stop(paste("Reduction", umap_object, "not found in object"))
  }
  
  if (!cell_annotations %in% colnames(object@meta.data)) {
    stop(paste("Column", cell_annotations, "not found in meta.data"))
  }
  
  # Prepare data
  data_prep <- prepare_rna_data(object, RNA_assay)
  if (!data_prep$success) {
    stop(paste("Error preparing data:", data_prep$error))
  }
  
  summ <- data_prep$data
  marker_choices <- data_prep$marker_choices
  
  # Default marker selections
  default_markers <- list(
    marker1 = if ("CD3E" %in% marker_choices$RNA) "CD3E" else marker_choices$RNA[1],
    marker2 = if ("CD4" %in% marker_choices$RNA) "CD4" else marker_choices$RNA[2],
    color_by = if ("CD8A" %in% marker_choices$RNA) "CD8A" else marker_choices$RNA[3],
    marker3 = if ("CD3E" %in% marker_choices$RNA) "CD3E" else marker_choices$RNA[1],
    marker4 = if ("CD4" %in% marker_choices$RNA) "CD4" else marker_choices$RNA[2],
    color_by_1 = if ("CD8A" %in% marker_choices$RNA) "CD8A" else marker_choices$RNA[3]
  )
  
  # Create UI
  ui <- create_dashboard_ui("RNA Checker", marker_choices, default_markers, "RNA")
  
  # Server function
  server <- function(input, output, session) {
    
    # Reactive values
    selected_data <- shiny::reactiveVal(NULL)
    selected_data_1 <- shiny::reactiveVal(NULL)
    selected_data_3 <- shiny::reactiveVal(NULL)
    selected_data_daughter <- shiny::reactiveVal(NULL)
    selected_data_daughter_1 <- shiny::reactiveVal(NULL)
    selected_data_daughter_2 <- shiny::reactiveVal(NULL)
    object_p <- shiny::reactiveVal(summ)
    object_p1 <- shiny::reactiveVal(NULL)
    
    # Main scatter plot
    output$distPlot <- plotly::renderPlotly({
      create_scatter_plot(object_p(), input$Marker_1, input$Marker_2, input$color_by, "plotly_select")
    })
    
    # Handle main plot selection
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "plotly_select"), {
      event_data <- plotly::event_data("plotly_selected", source = "plotly_select")
      selected_data(handle_plotly_selection(event_data, object_p))
    })
    
    # Daughter plot
    output$distPlot_3 <- plotly::renderPlotly({
      if (input$daughter_plot == "Yes" && !is.null(selected_data())) {
        create_scatter_plot(selected_data(), input$Marker_3, input$Marker_4, input$color_by_1, "plotly_selected_daughter")
      } else {
        plotly::plotly_empty()
      }
    })
    
    # Handle daughter plot selection
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "plotly_selected_daughter"), {
      event_data <- plotly::event_data("plotly_selected", source = "plotly_selected_daughter")
      selected_data_daughter(handle_plotly_selection(event_data, selected_data))
    })
    
    # UMAP plot
    output$umap_plot <- shiny::renderPlot({
      selected_rows <- NULL
      
      if (!is.null(selected_data()) && input$daughter_plot == "No") {
        selected_rows <- selected_data() %>% dplyr::pull(barcode)
      } else if (!is.null(selected_data_daughter()) && input$daughter_plot == "Yes") {
        selected_rows <- selected_data_daughter() %>% dplyr::pull(barcode)
      }
      
      create_highlight_plot(object, selected_rows, umap_object)
    })
    
    # Summary table
    output$table <- shiny::renderTable({
      selected_rows <- NULL
      
      if (!is.null(selected_data()) && input$daughter_table == "No") {
        selected_rows <- selected_data() %>% dplyr::pull(barcode)
      } else if (!is.null(selected_data_daughter()) && input$daughter_table == "Yes") {
        selected_rows <- selected_data_daughter() %>% dplyr::pull(barcode)
      }
      
      create_summary_table(object, selected_rows, cell_annotations)
    })
    
    # Second tab - UMAP plot
    output$umap_plot_2 <- plotly::renderPlotly({
      create_umap_plot(object, umap_object)
    })
    
    # Handle UMAP selection
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "plotly_select_2"), {
      event_data <- plotly::event_data("plotly_selected", source = "plotly_select_2")
      if (!is.null(event_data)) {
        df <- methods::slot(object@reductions[[umap_object]], "cell.embeddings")
        df <- as.data.frame(df)
        df$cell_barcodes <- rownames(df)
        
        selected_indices <- event_data$pointNumber + 1
        selected_data_1(df[selected_indices, ])
      }
    })
    
    # Second tab scatter plot
    output$distPlot_2 <- plotly::renderPlotly({
      if (!is.null(selected_data_1())) {
        filtered_data <- object_p() %>% 
          dplyr::filter(barcode %in% rownames(selected_data_1()))
        object_p1(filtered_data)
        create_scatter_plot(filtered_data, input$Marker_1, input$Marker_2, input$color_by, "plotly_select_3")
      } else {
        plotly::plotly_empty()
      }
    })
    
    # Handle second scatter plot selection
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "plotly_select_3"), {
      event_data <- plotly::event_data("plotly_selected", source = "plotly_select_3")
      selected_data_3(handle_plotly_selection(event_data, object_p1))
    })
    
    # Second tab daughter plot
    output$distPlot_4 <- plotly::renderPlotly({
      if (input$daughter_plot_1 == "Yes" && !is.null(selected_data_3())) {
        create_scatter_plot(selected_data_3(), input$Marker_3, input$Marker_4, input$color_by_1, "plotly_selected_daughter_2")
      } else {
        plotly::plotly_empty()
      }
    })
    
    # Handle second daughter plot selection
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "plotly_selected_daughter_2"), {
      event_data <- plotly::event_data("plotly_selected", source = "plotly_selected_daughter_2")
      selected_data_daughter_2(handle_plotly_selection(event_data, selected_data_3))
    })
    
    # Second tab table
    output$table_2 <- shiny::renderTable({
      selected_rows <- NULL
      
      if (!is.null(selected_data_1()) && input$daughter_table_1 == "No") {
        selected_rows <- selected_data_1() %>% dplyr::pull(cell_barcodes)
      } else if (!is.null(selected_data_3()) && input$daughter_table_1 == "Yes" && input$daughter_table_2 == "No") {
        selected_rows <- selected_data_3() %>% dplyr::pull(barcode)
      } else if (!is.null(selected_data_daughter_2()) && input$daughter_table_2 == "Yes") {
        selected_rows <- selected_data_daughter_2() %>% dplyr::pull(barcode)
      }
      
      create_summary_table(object, selected_rows, cell_annotations)
    })
    
    # Export cells functionality
    shiny::observeEvent(input$export_cells, {
      selected_rows <- NULL
      
      if (!is.null(selected_data()) && input$daughter_plot == "No") {
        selected_rows <- selected_data() %>% dplyr::pull(barcode)
      } else if (!is.null(selected_data_daughter()) && input$daughter_plot == "Yes") {
        selected_rows <- selected_data_daughter() %>% dplyr::pull(barcode)
      }
      
      result <- export_selected_cells(object, selected_rows, "RNA_selected_biplot")
      
      output$export_status <- shiny::renderText({
        if (result$success) {
          paste("✓", result$message)
        } else {
          paste("✗", result$message)
        }
      })
    })
    
    shiny::observeEvent(input$export_cells_2, {
      selected_rows <- NULL
      
      if (!is.null(selected_data_1()) && input$daughter_table_1 == "No") {
        selected_rows <- selected_data_1() %>% dplyr::pull(cell_barcodes)
      } else if (!is.null(selected_data_3()) && input$daughter_table_1 == "Yes" && input$daughter_table_2 == "No") {
        selected_rows <- selected_data_3() %>% dplyr::pull(barcode)
      } else if (!is.null(selected_data_daughter_2()) && input$daughter_table_2 == "Yes") {
        selected_rows <- selected_data_daughter_2() %>% dplyr::pull(barcode)
      }
      
      result <- export_selected_cells(object, selected_rows, "RNA_selected_cluster")
      
      output$export_status_2 <- shiny::renderText({
        if (result$success) {
          paste("✓", result$message)
        } else {
          paste("✗", result$message)
        }
      })
    })
    
    # Close app
    shiny::observeEvent(input$close, {
      gc()
      shiny::stopApp()
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}

#' @title Analyze CITE-seq Data
#'
#' @param object Seurat Object
#' @param RNA_assay Assay name (RNA)
#' @param ADT_assay Assay name (ADT)
#' @param umap_object Name of the UMAP or TSNE object
#' @param cell_annotations Name of the cell annotation column from meta.data
#'
#' @return A shiny dashboard with interactive plots and export functionality
#' @details The dashboard includes:
#' - Interactive biplot and UMAP selection for both RNA and ADT data
#' - Export selected cells as subset Seurat objects to global environment
#' - Summary tables of selected cell populations
#' @export
ADT_Checker <- function(object = NULL, RNA_assay = "RNA", ADT_assay = "ADT", umap_object = "umap", cell_annotations = "orig.ident") {
  
  # Validate inputs
  if (is.null(object)) {
    stop("Seurat object is required")
  }
  
  if (!RNA_assay %in% names(object@assays)) {
    stop(paste("Assay", RNA_assay, "not found in object"))
  }
  
  if (!ADT_assay %in% names(object@assays)) {
    stop(paste("Assay", ADT_assay, "not found in object"))
  }
  
  if (!umap_object %in% names(object@reductions)) {
    stop(paste("Reduction", umap_object, "not found in object"))
  }
  
  if (!cell_annotations %in% colnames(object@meta.data)) {
    stop(paste("Column", cell_annotations, "not found in meta.data"))
  }
  
  # Prepare data
  data_prep <- prepare_adt_data(object, RNA_assay, ADT_assay)
  if (!data_prep$success) {
    stop(paste("Error preparing data:", data_prep$error))
  }
  
  combined_data <- data_prep$data
  marker_choices <- data_prep$marker_choices
  
  # Default marker selections
  default_markers <- list(
    marker1 = if ("CD3_ADT" %in% marker_choices$ADT) "CD3_ADT" else marker_choices$ADT[1],
    marker2 = if ("CD4_ADT" %in% marker_choices$ADT) "CD4_ADT" else marker_choices$ADT[2],
    color_by = if ("CD8_ADT" %in% marker_choices$ADT) "CD8_ADT" else marker_choices$ADT[3],
    marker3 = if ("CD3_ADT" %in% marker_choices$ADT) "CD3_ADT" else marker_choices$ADT[1],
    marker4 = if ("CD4_ADT" %in% marker_choices$ADT) "CD4_ADT" else marker_choices$ADT[2],
    color_by_1 = if ("CD8_ADT" %in% marker_choices$ADT) "CD8_ADT" else marker_choices$ADT[3]
  )
  
  # Create UI
  ui <- create_dashboard_ui("ADT Checker", marker_choices, default_markers, "ADT")
  
  # Server function
  server <- function(input, output, session) {
    
    # Reactive values
    selected_data <- shiny::reactiveVal(NULL)
    selected_data_1 <- shiny::reactiveVal(NULL)
    selected_data_daughter <- shiny::reactiveVal(NULL)
    selected_data_daughter_1 <- shiny::reactiveVal(NULL)
    selected_data_daughter_2 <- shiny::reactiveVal(NULL)
    object_p <- shiny::reactiveVal(combined_data %>% dplyr::mutate(cell_barcodes = rownames(combined_data)))
    object_p1 <- shiny::reactiveVal(NULL)
    
    # Main scatter plot
    output$distPlot <- plotly::renderPlotly({
      create_scatter_plot(object_p(), input$Marker_1, input$Marker_2, input$color_by, "plotly_select")
    })
    
    # Handle main plot selection
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "plotly_select"), {
      event_data <- plotly::event_data("plotly_selected", source = "plotly_select")
      selected_data(handle_plotly_selection(event_data, object_p))
    })
    
    # Daughter plot
    output$distPlot_3 <- plotly::renderPlotly({
      if (input$daughter_plot == "Yes" && !is.null(selected_data())) {
        create_scatter_plot(selected_data(), input$Marker_3, input$Marker_4, input$color_by_1, "plotly_selected_daughter")
      } else {
        plotly::plotly_empty()
      }
    })
    
    # Handle daughter plot selection
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "plotly_selected_daughter"), {
      event_data <- plotly::event_data("plotly_selected", source = "plotly_selected_daughter")
      selected_data_daughter(handle_plotly_selection(event_data, selected_data))
    })
    
    # UMAP plot
    output$umap_plot <- shiny::renderPlot({
      selected_rows <- NULL
      
      if (!is.null(selected_data()) && input$daughter_plot == "No") {
        selected_rows <- selected_data() %>% dplyr::pull(Row.names)
      } else if (!is.null(selected_data_daughter()) && input$daughter_plot == "Yes") {
        selected_rows <- selected_data_daughter() %>% dplyr::pull(Row.names)
      }
      
      create_highlight_plot(object, selected_rows, umap_object)
    })
    
    # Summary table
    output$table <- shiny::renderTable({
      selected_rows <- NULL
      
      if (!is.null(selected_data()) && input$daughter_table == "No") {
        selected_rows <- selected_data() %>% dplyr::pull(Row.names)
      } else if (!is.null(selected_data_daughter()) && input$daughter_table == "Yes") {
        selected_rows <- selected_data_daughter() %>% dplyr::pull(Row.names)
      }
      
      create_summary_table(object, selected_rows, cell_annotations)
    })
    
    # Second tab - UMAP plot
    output$umap_plot_2 <- plotly::renderPlotly({
      create_umap_plot(object, umap_object)
    })
    
    # Handle UMAP selection
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "plotly_select_2"), {
      event_data <- plotly::event_data("plotly_selected", source = "plotly_select_2")
      if (!is.null(event_data)) {
        df <- methods::slot(object@reductions[[umap_object]], "cell.embeddings")
        df <- as.data.frame(df)
        df$cell_barcodes <- rownames(df)
        
        selected_indices <- event_data$pointNumber + 1
        selected_data_1(df[selected_indices, ])
      }
    })
    
    # Second tab scatter plot
    output$distPlot_2 <- plotly::renderPlotly({
      if (!is.null(selected_data_1())) {
        filtered_data <- object_p() %>% 
          dplyr::filter(Row.names %in% rownames(selected_data_1()))
        object_p1(filtered_data)
        create_scatter_plot(filtered_data, input$Marker_1, input$Marker_2, input$color_by, "plotly_selected_daughter_1")
      } else {
        plotly::plotly_empty()
      }
    })
    
    # Handle second scatter plot selection
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "plotly_selected_daughter_1"), {
      event_data <- plotly::event_data("plotly_selected", source = "plotly_selected_daughter_1")
      selected_data_daughter_1(handle_plotly_selection(event_data, object_p1))
    })
    
    # Second tab daughter plot
    output$distPlot_4 <- plotly::renderPlotly({
      if (input$daughter_plot_1 == "Yes" && !is.null(selected_data_daughter_1())) {
        create_scatter_plot(selected_data_daughter_1(), input$Marker_3, input$Marker_4, input$color_by_1, "plotly_selected_daughter_2")
      } else {
        plotly::plotly_empty()
      }
    })
    
    # Handle second daughter plot selection
    shiny::observeEvent(plotly::event_data("plotly_selected", source = "plotly_selected_daughter_2"), {
      event_data <- plotly::event_data("plotly_selected", source = "plotly_selected_daughter_2")
      selected_data_daughter_2(handle_plotly_selection(event_data, selected_data_daughter_1))
    })
    
    # Second tab table
    output$table_2 <- shiny::renderTable({
      selected_rows <- NULL
      
      if (!is.null(selected_data_1()) && input$daughter_table_1 == "No") {
        selected_rows <- selected_data_1() %>% dplyr::pull(cell_barcodes)
      } else if (!is.null(selected_data_daughter_1()) && input$daughter_table_1 == "Yes" && input$daughter_table_2 == "No") {
        selected_rows <- selected_data_daughter_1() %>% dplyr::pull(Row.names)
      } else if (!is.null(selected_data_daughter_2()) && input$daughter_table_2 == "Yes") {
        selected_rows <- selected_data_daughter_2() %>% dplyr::pull(Row.names)
      }
      
      create_summary_table(object, selected_rows, cell_annotations)
    })
    
    # Export cells functionality
    shiny::observeEvent(input$export_cells, {
      selected_rows <- NULL
      
      if (!is.null(selected_data()) && input$daughter_plot == "No") {
        selected_rows <- selected_data() %>% dplyr::pull(Row.names)
      } else if (!is.null(selected_data_daughter()) && input$daughter_plot == "Yes") {
        selected_rows <- selected_data_daughter() %>% dplyr::pull(Row.names)
      }
      
      result <- export_selected_cells(object, selected_rows, "ADT_selected_biplot")
      
      output$export_status <- shiny::renderText({
        if (result$success) {
          paste("✓", result$message)
        } else {
          paste("✗", result$message)
        }
      })
    })
    
    shiny::observeEvent(input$export_cells_2, {
      selected_rows <- NULL
      
      if (!is.null(selected_data_1()) && input$daughter_table_1 == "No") {
        selected_rows <- selected_data_1() %>% dplyr::pull(cell_barcodes)
      } else if (!is.null(selected_data_daughter_1()) && input$daughter_table_1 == "Yes" && input$daughter_table_2 == "No") {
        selected_rows <- selected_data_daughter_1() %>% dplyr::pull(Row.names)
      } else if (!is.null(selected_data_daughter_2()) && input$daughter_table_2 == "Yes") {
        selected_rows <- selected_data_daughter_2() %>% dplyr::pull(Row.names)
      }
      
      result <- export_selected_cells(object, selected_rows, "ADT_selected_cluster")
      
      output$export_status_2 <- shiny::renderText({
        if (result$success) {
          paste("✓", result$message)
        } else {
          paste("✗", result$message)
        }
      })
    })
    
    # Close app
    shiny::observeEvent(input$close, {
      gc()
      shiny::stopApp()
    })
  }
  
  shiny::shinyApp(ui = ui, server = server)
}