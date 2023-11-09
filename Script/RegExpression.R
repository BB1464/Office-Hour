
file_names <- c("./../data/folder_a/folder_b/folder 1/a.csv","./../data/folder_a/folder_b/folder 2/a.csv","./../data/folder_a/folder_c/folder 3/a.csv","./../data/folder_d/folder_f/folder 4/.csv")


stringr::str_extract(string = file_names,
                     pattern = "folder \\d+")

# Check the pattern
stringr::str_view(string = file_names,pattern = "folder \\w+")

basename(dirname("./../data/folder_a/folder_b/folder 1/a.csv"))


stringr::str_extract(file_names, "(?<=/)[^/]+(?=/[^/]+$)") # grab stuff between penultimate and ultimate slash:

# Check the pattern

stringr::str_view(string = file_names,pattern = "(?<=/)[^/]+(?=/[^/]+$)")


