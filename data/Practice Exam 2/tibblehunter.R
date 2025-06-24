# Function to detect and convert tibbles to data.frames
start_tibble_hunter <- function() {
  
  # Check if tibble package is available
  if (!requireNamespace("tibble", quietly = TRUE)) {
    message("tibble package not found - tibble hunter not started")
    return(invisible(NULL))
  }
  
  # Store original print methods to restore later
  if (!exists(".tibble_hunter_env", envir = .GlobalEnv)) {
    assign(".tibble_hunter_env", new.env(), envir = .GlobalEnv)
  }
  
  hunter_env <- get(".tibble_hunter_env", envir = .GlobalEnv)
  
  # Store original methods if not already stored
  if (is.null(hunter_env$original_print_tbl)) {
    # Try to get the original method, but handle case where it doesn't exist yet
    tryCatch({
      hunter_env$original_print_tbl <- getS3method("print", "tbl_df")
    }, error = function(e) {
      # If tibble isn't loaded, we'll store it later when first encountered
      hunter_env$original_print_tbl <- "not_loaded"
    })
  }
  
  # Custom print method for tibbles that converts to data.frame
  print.tbl_df <- function(x, ...) {
    # Store the original method if we haven't already and tibble is now loaded
    if (exists(".tibble_hunter_env", envir = .GlobalEnv)) {
      hunter_env <- get(".tibble_hunter_env", envir = .GlobalEnv)
      if (identical(hunter_env$original_print_tbl, "not_loaded")) {
        # Try to get the original method now that tibble might be loaded
        tryCatch({
          # Temporarily remove our method to get the original
          if (exists("print.tbl_df", envir = .GlobalEnv)) {
            temp_method <- get("print.tbl_df", envir = .GlobalEnv)
            rm("print.tbl_df", envir = .GlobalEnv)
          }
          hunter_env$original_print_tbl <- getS3method("print", "tbl_df")
          # Restore our method
          if (exists("temp_method")) {
            assign("print.tbl_df", temp_method, envir = .GlobalEnv)
          }
        }, error = function(e) {
          # If still not available, use default data.frame print
          hunter_env$original_print_tbl <- function(x, ...) {
            print(as.data.frame(x), ...)
          }
        })
      }
    }
    
    message("Converting tibble output to data.frame")
    df <- as.data.frame(x)
    print(df, ...)
  }
  
  # Register the custom print method
  registerS3method("print", "tbl_df", print.tbl_df)
  
  # Function to check and convert tibbles in the global environment
  convert_tibbles <- function() {
    # Get all objects in global environment
    obj_names <- ls(envir = .GlobalEnv)
    
    for (name in obj_names) {
      # Skip our hunter environment
      if (name == ".tibble_hunter_env") next
      
      obj <- get(name, envir = .GlobalEnv)
      
      # Check if object is a tibble
      if (inherits(obj, "tbl_df")) {
        # Convert to data.frame and reassign
        converted <- as.data.frame(obj)
        assign(name, converted, envir = .GlobalEnv)
        message(paste("Converted tibble", name, "to data.frame"))
      }
    }
  }
  
  # Create a task callback that runs after each top-level task
  callback_id <- addTaskCallback(function(expr, value, ok, visible) {
    if (ok) {
      # Use tryCatch to avoid breaking R if something goes wrong
      tryCatch({
        convert_tibbles()
      }, error = function(e) {
        # Silently handle errors to avoid breaking the callback system
        invisible(NULL)
      })
    }
    return(TRUE)  # Keep the callback active
  }, name = "tibble_hunter")
  
  message("Tibble hunter started! Tibbles will be automatically converted to data.frames.")
  message("This affects both console output and saved objects.")
  message(paste("Callback ID:", callback_id))
  
  # Return the callback ID so it can be removed later
  invisible(callback_id)
}

# Function to stop the tibble hunter
stop_tibble_hunter <- function(callback_id = NULL) {
  # Restore original print method
  if (exists(".tibble_hunter_env", envir = .GlobalEnv)) {
    hunter_env <- get(".tibble_hunter_env", envir = .GlobalEnv)
    
    if (!is.null(hunter_env$original_print_tbl) && 
        !identical(hunter_env$original_print_tbl, "not_loaded")) {
      tryCatch({
        registerS3method("print", "tbl_df", hunter_env$original_print_tbl)
        message("Restored original tibble print method.")
      }, error = function(e) {
        message("Could not restore original print method (tibble may not be loaded)")
      })
    }
    
    # Clean up the hunter environment
    rm(.tibble_hunter_env, envir = .GlobalEnv)
  }
  
  # Remove the task callback
  if (is.null(callback_id)) {
    # Remove by name if no ID provided
    result <- removeTaskCallback("tibble_hunter")
  } else {
    # Remove by ID
    result <- removeTaskCallback(callback_id)
  }
  
  if (result) {
    message("Tibble hunter stopped.")
  } else {
    message("No active tibble hunter found.")
  }
  
  invisible(result)
}

# Function to check if tibble hunter is running
is_tibble_hunter_active <- function() {
  callbacks <- getTaskCallbackNames()
  "tibble_hunter" %in% callbacks
}

