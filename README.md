## Table of contents
* [Example Global Code](#global_code)
* [Example Canadian Code](#canadian_code)
* [Generating Lambdas](#lambdas)

## Example Global Code

The file [test_script.Rmd](test_script.Rmd) is a markdown file showing how to create the global LPI based upon the publically available data [data/LPR2020data_public.csv](data/LPR2020data_public.csv). It contains some helper functions that convert a table of population rates [data/Global_public_pops_poplevel_lambda.csv](data/Global_public_pops_poplevel_lambda.csv) of change (one population per row) into a global index using the LPI methodology.
	
## Example Canadian Code

The file [test_script_canadian.Rmd](test_script_canadian.Rmd) is a markdown file showing how to create the Canadian Species Index (using the LPI method) based upon the candadian data in [data/CIEE_LPI_dataset.csv](data/CIEE_LPI_dataset.csv). It modifies the code from the above global example to account for some slight differences in how the CSI is made (e.g. no weightings and no realm data).

	
## Generating Lambdas

tbd

