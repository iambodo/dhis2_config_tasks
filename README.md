# dhis2_config_tasks
Scripts to support quick configuration of dhis2, mostly trackers

For each task, use auth.json to store DHIS2 credentials.

## [Program Rule Variables from Program Data Elements](https://github.com/iambodo/dhis2_config_tasks/tree/main/prv_from_dataElements)
This routine produces an export file of program rule variables (PRV) based on all data elements in a given DHIS2 program.
* Outputs a JSON file and CSV file of PRV types
* CSV can be manually modified to change e.g. PRV name
* Program UID entered in script or passed as an argument with `Rscript` in command line
