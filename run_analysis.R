## Main function. Create a tidy dataset.
## path variable is path to file where dataset will be saved.
get_data <- function(path = "dataset.txt") {
    ## Read train data
    train_data <- read_data("train")
    ## Read test data
    test_data <- read_data("test")
    ## Merge data
    data <- rbind(train_data, test_data)
    data[["Activity"]] <- name_activity(data[["Activity"]])
    len <- ncol(data)
    ## Divide into group by activities and subjects
    factors_list <- list(data[["Activity"]], data[["Subject"]])
    new_data <- split(data, factors_list)
    ## Calculate averages 
    new_data <- lapply(new_data, function(x) {
                        x <- x[1 : (len - 2)] 
                        colMeans(x)
                        })
    ## Convert a list of data.frames to one data.frame
    new_data <- data.frame(do.call("rbind", new_data))
    ## Add activity and subject columns
    ans_data <- cbind(data.frame(do.call("rbind", 
                                         strsplit(row.names(new_data), ".", fixed = TRUE))), 
                      new_data)
    names(ans_data)[1] <- "Subject"
    names(ans_data)[2] <- "Activity"
    ## Write data
    write.table(ans_data, path, row.name = FALSE, sep = ",")
    

}

## Read ans subsets data
## dataset should be "train" or "test" either.
read_data <- function(dataset) {
    ##Read data
    x_data <- read.table(paste(dataset, "/X_", dataset, ".txt", sep = ""), 
                         header = FALSE, sep = "")
    y_data <- read.table(paste(dataset, "/y_", dataset, ".txt", sep = ""), 
                         header = FALSE, sep = "")
    subject_data <- read.table(paste(dataset, "/subject_", dataset, ".txt", sep = ""), 
                               header = FALSE)
    features <- read.table("features.txt", header = FALSE, sep = "")[[2]]
    names(x_data) <- features
    ## Subset data. List of column has been made manually. 
    x_data <- x_data[c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214, 215, 227:228, 
                     240:241, 253:254, 266:271, 294:296, 345:350, 373:375, 424:429, 452:454, 
                     503:504, 513, 516:517, 526, 529, 530, 539, 542, 543, 552)]
    data <- cbind(x_data, y_data, subject_data)
    names(data) <- c(names(x_data), "Activity", "Subject")
    data    

}

activity_vector <- c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", 
                     "SITTING", "STANDING", "LAYING")

## Name activity
name_activity <- function(x) unlist(lapply(x, function(y) activity_vector[y]))
       
    

