# {{folder_name}}

## Symbolic Link to Secure Data
{{symbolic_link}}

## Package Version Control
- Enable package version control for this project by running `renv::init()`. A list of packages and versions will be added to the `lockfile`. This only needs to be run once per project. 
- Run `renv::snapshot()` to update with packages not previously added to `lockfile`.
- Use `renv::restore()` to install packages as declared in the lockfile.  Visit https://rstudio.github.io/renv/ for details.

## Data Version Control
- Save data in a secure folder on a network drive with a subfolder indicating date data was received.
  - e.g. "H:/ ... /Project Folder/secure_data/{{Sys.Date()}}/data_set.csv"
- The file `data_date.txt` refers to the date folder in the `"secure_data"` folder. When new data arrives, save it in a new date folder and update `data_date.txt`.

## Project Log
**{{Sys.Date()}}**  
Created project folder
