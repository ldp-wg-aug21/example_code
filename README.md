## Table of contents
* [Example Global Code](#global_code)
* [Example Canadian Code](#canadian_code)
* [Generating Lambdas](#lambdas)

<a name="global_code"/>
## Example Global Code

The file [test_script.Rmd](test_script.Rmd) is a markdown file showing how to create the global LPI based upon the publically available data [data/LPR2020data_public.csv](data/LPR2020data_public.csv). It contains some helper functions that convert a table of population rates [data/Global_public_pops_poplevel_lambda.csv](data/Global_public_pops_poplevel_lambda.csv) of change (one population per row) into a global index using the LPI methodology.

The script has three sections (after the helper functions):

* Create the global index
* Generate 'sampled' global indices by resamping (with replacement) populations
* Generate 'sampled' global indices by resamping (with replacement) species

The latter two sections save generated sampled indices to two helper files (pop_sampled_lpis.csv & sp_sampled_lpis.csv) that are then loaded and plotted.
	
<a name="canadian_code"/>
## Example Canadian Code

The file [test_script_canada.Rmd](test_script_canada.Rmd) is a markdown file showing how to create the Canadian Species Index (using the LPI method) based upon the candadian data in [data/CIEE_LPI_dataset.csv](data/CIEE_LPI_dataset.csv). It modifies the code from the above global example to account for some slight differences in how the CSI is made (e.g. no weightings and no realm data).

As above the script has three sections (after the helper functions):

* Create the global index
* Generate 'sampled' global indices by resamping (with replacement) populations
* Generate 'sampled' global indices by resamping (with replacement) species

The latter two sections save generated sampled indices to two helper files (pop_sampled_lpis.csv & sp_sampled_lpis.csv) that are then loaded and plotted.
	
<a name="lambdas"/>
## Generating Lambdas

The scripts above work by averaging pre-calculated rates of change for each population. To create these rates of change (lambdas) we're using the rlpi package. An example of doing this is shown in the [generating_lambdas_canada.Rmd](generating_lambdas_canada.Rmd)

