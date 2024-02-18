Seventh_Project = function() {
  valid_choice = FALSE
  
  while (!valid_choice) {
    cat("Choose a data frame to work with:\n")
    cat("1. Cereal\n")
    cat("2. Grades\n")
    
    choice = as.character(readline("Enter 1 or 2: "))
    
    if (choice %in% c("1", "2")) {
      valid_choice = TRUE
      choice = as.integer(choice)
      
      if (choice == 1) {
        cereal_path = "C:/Users/Yousef/Desktop/stats project/cereal/cereal.csv"  
        df = read.csv(cereal_path)
        df_name = "cereal"  
      } else if (choice == 2) {
        grades_path = "C:/Users/Yousef/Desktop/stats project/grades/grades.csv"  
        df = read.csv(grades_path)
        df_name = "grades"  
      }
      
      cat("Selected dataframe: ", df_name, "\n")
      
      cat("Columns in the selected dataframe:\n")
      for (i in 1:length(colnames(df))) {
        cat("Press", i, "for column:", colnames(df)[i], "\n")
      }
      
      selected_columns = c()
      while (TRUE) {
        col_num = as.integer(readline("Enter the number of a column to select (0 to finish): "))
        if (col_num == 0) {
          break
        } else if (col_num > 0 && col_num <= length(colnames(df))) {
          if (!(colnames(df)[col_num] %in% selected_columns)) {
            selected_columns = c(selected_columns, colnames(df)[col_num])
          } else {
            cat("Column already selected. Try again.\n")
          }
        } else {
          cat("Invalid column number. Try again.\n")
        }
      }
      
      if (length(selected_columns) == 0) {
        cat("No columns selected. Exiting.\n")
        return()
      }
      
      cat("Selected columns:\n")
      for (col_name in selected_columns) {
        cat(col_name, "\n")
      }
      
      cat("Choose calculations for selected columns:\n")
      calc_options = c()
      cat("1. Count\n")
      cat("2. Mean\n")
      cat("3. Standard Deviation\n")
      cat("4. Minimum\n")
      cat("5. 25th Percentile (1st Quartile)\n")
      cat("6. Median (50th Percentile, 2nd Quartile)\n")
      cat("7. 75th Percentile (3rd Quartile)\n")
      cat("8. Maximum\n")
      cat("9. All Calculations\n")
      
      while (TRUE) {
        calc_option = as.integer(readline("Enter the number for the calculation (0 to finish): "))
        if (calc_option == 0) {
          break
        } else if (calc_option < 1 || calc_option > 9) {
          cat("Invalid calculation option. Try again.\n")
        } else if (calc_option == 9) {
          calc_options = 1:8
          break
        } else if (!(calc_option %in% calc_options)) {
          calc_options = c(calc_options, calc_option)
        } else {
          cat("Calculation already selected. Try again.\n")
        }
      }
      
      if (length(calc_options) == 0) {
        cat("No calculations selected. Exiting.\n")
        return()
      }
      
      cat("Selected calculations for each column:\n")
      for (col_name in selected_columns) {
        cat("Calculations for column:", col_name, "\n")
        
        for (calc_option in calc_options) {
          switch(calc_option,
                 Count = {
                   result = sum(!is.na(df[, col_name]))
                   if (result > 0) cat("Count for", col_name, ": ", result, "\n") else cat("No data to count for ", col_name, "\n")
                 },
                 Mean = {
                   if (all(is.numeric(df[, col_name]))) {
                     result = mean(df[, col_name])
                     cat("Mean for", col_name, ": ", result, "\n")
                   } else {
                     cat("Mean: cannot calculate mean for", col_name, "\n")
                   }
                 },
                 `Standard Deviation` = {
                   if (all(is.numeric(df[, col_name]))) {
                     result = sd(df[, col_name])
                     cat("Standard Deviation for", col_name, ": ", result, "\n")
                   } else {
                     cat("Standard Deviation: cannot calculate standard deviation for ", col_name, "\n")
                   }
                 },
                 Minimum = {
                   if (all(is.numeric(df[, col_name]))) {
                     result = min(df[, col_name])
                     cat("Minimum for", col_name, ": ", result, "\n")
                   } else {
                     cat("Minimum: cannot calculate minimum for", col_name, "\n")
                   }
                 },
                 `25th Percentile` = {
                   if (all(is.numeric(df[, col_name]))) {
                     result = quantile(df[, col_name], 0.25)
                     cat("25th Percentile for", col_name, ": ", result, "\n")
                   } else {
                     cat("25th Percentile: cannot calculate 25th Percentile for", col_name, "\n")
                   }
                 },
                 Median = {
                   if (all(is.numeric(df[, col_name]))) {
                     result = median(df[, col_name])
                     cat("Median for", col_name, ": ", result, "\n")
                   } else {
                     cat("Median: cannot calculate median for", col_name, "\n")
                   }
                 },
                 `75th Percentile` = {
                   if (all(is.numeric(df[, col_name]))) {
                     result = quantile(df[, col_name], 0.75)
                     cat("75th Percentile for", col_name, ": ", result, "\n")
                   } else {
                     cat("75th Percentile: cannot calculate 75th Percentile for", col_name, "\n")
                   }
                 },
                 Maximum = {
                   if (all(is.numeric(df[, col_name]))) {
                     result = max(df[, col_name])
                     cat("Maximum for", col_name, ": ", result, "\n")
                   } else {
                     cat("Maximum: cannot calculate maximum for", col_name, "\n")
                   }
                 }
          )
        }
        cat("\n")
      }
    } else {
      cat("Invalid choice. Please enter 1 or 2.\n")
    }
  }
}

Seventh_Project()
