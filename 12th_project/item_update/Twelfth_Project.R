Twelfth_Project = function() {
  valid_choice = FALSE
  
  while (!valid_choice) {
    cat("Choose a data frame to work with:\n")
    cat("1. Item Price\n")
    cat("2. Item Update\n")
    
    choice = as.character(readline("Enter 1 or 2: "))
    
    if (choice %in% c("1", "2")) {
      valid_choice = TRUE
      choice = as.integer(choice)
      
      if (choice == 1) {
        db_path = "C:\\Users\\Yousef\\Desktop\\stats project\\12th_project\\item_price\\ItemPrice.csv"
        df = read.csv(db_path)
        df_name = "Item Price"  
      } else if (choice == 2) {
        db_path = "C:\\Users\\Yousef\\Desktop\\stats project\\12th_project\\item_update\\items_Update.csv"
        df = read.csv(db_path)
        df_name = "Item Update"  
      }
      
      cat("Selected database: ", df_name, "\n")
      
      selected_items = data.frame(Item = integer(), Quantity = integer())
      while (TRUE) {
        cat("Enter an item number between 1 and 50 (or 0 to finish): ")
        item_num = as.integer(readline())
        
        if (item_num == 0) {
          break
        } else if (item_num >= 1 && item_num <= 50) {
          if (item_num %in% selected_items$Item) {
            cat("Item ", item_num, " is already selected. Please choose another item.\n")
          } else {
            quantity = as.integer(readline(paste("Enter quantity for Item ", item_num, ": ")))
            selected_items = rbind(selected_items, data.frame(Item = item_num, Quantity = quantity))
          }
        } else {
          cat("Invalid item number. Try again.\n")
        }
      }
      
      if (nrow(selected_items) == 0) {
        cat("No items selected. Exiting.\n")
        return()
      }
      
      cat("\nSelected items:\n")
      cat("Item   |   Quantity\n")
      cat("--------------------\n")
      for (i in 1:nrow(selected_items)) {
        cat(formatC(selected_items$Item[i], width = 4, flag = "0"), "   |   ", selected_items$Quantity[i], "\n")
      }
      
      total_quantity = sum(selected_items$Quantity)
      total_price = sum(df$Price[df$Item %in% selected_items$Item] * selected_items$Quantity)
      min_price = min(df$Price[df$Item %in% selected_items$Item])
      max_price = max(df$Price[df$Item %in% selected_items$Item])
      mean_price = mean(df$Price[df$Item %in% selected_items$Item])
      
      cat("\nSummary:\n")
      cat("----------------------\n")
      cat("Total items selected: ", nrow(selected_items), "\n")
      cat("Total quantity: ", total_quantity, "\n")
      cat("Total price: $", total_price, "\n")
      cat("Minimum price: $", min_price, "\n")
      cat("Maximum price: $", max_price, "\n")
      cat("Mean price: $", mean_price, "\n")
      cat("Required to pay: $", total_price, "\n")
      cat("----------------------\n")
      
      if (df_name == "Item Update") {
        cat("\nUpdating quantities in the Item Update database:\n")
        for (i in 1:nrow(selected_items)) {
          item_num = selected_items$Item[i]
          prev_quantity = df$Quantity[df$Item == item_num]
          new_quantity = prev_quantity - selected_items$Quantity[i]
          
          cat("Item ", formatC(item_num, width = 4, flag = "0"), ": Quantity decremented from ", prev_quantity, " to ", new_quantity, "\n")
          
          # Update the quantity in the dataframe
          df$Quantity[df$Item == item_num] = new_quantity
        }
        
        # Save the updated dataframe back to the CSV file
        write.csv(df, db_path, row.names = FALSE)
      }
      
    } else {
      cat("Invalid choice. Please enter 1 or 2.\n")
    }
  }
}

Twelfth_Project()
