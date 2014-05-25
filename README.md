##How the script works

*I read in the dataset
*I reformat the headers to lower case, without hyphens or brackets
*I reassign the formatted headers
*I use the grep function to find the column indexes of columns containing mean or std, subset them, and create a new data frame
*Then I create a new data frame with the means of the columns for each subject and each activity
*Label the activities with descriptive names instead of the number codes