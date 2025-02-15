# 1 List all of the csv files
csv_files <- list.files('Data/', pattern='*.csv')

# 2 Find number of csv files
length(csv_files)

# 3 create wingspan_vs_mass.csv dataframe
df <- read.csv('Data/wingspan_vs_mass.csv')

# 4 Inspect the first 5 lines
head(df)

# 5 Find any files that begin with the letter "b" in the Data Directory
b_files <- list.files('Data/', pattern='^b', recursive=TRUE)
b_files

# 6 Display the first line of each of those b files

for (val in b_files) {
  thing <- read.table(file = paste('Data/', val, sep=''), header = F,nrows = 1)
  print(thing)
}


#7 Do the same thing for files that end in .csv
b_files <- list.files('Data/', pattern='*.csv', recursive=TRUE)
b_files

for (val in b_files) {
  thing <- read.csv(file = paste('Data/', val, sep=''), header = F,nrows = 1)
  print(thing)
}

