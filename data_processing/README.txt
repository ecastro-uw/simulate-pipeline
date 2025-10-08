Step 0: Create a crosswalk of Safegraph place_id  to top_category
- name: prep_poi_crosswalk.R
- purpose: creates a mapping between place_id and other POI metadata, including top_category and county, allowing users to identify which POIs are restaurants or bars (or any other top category) and in what county they are located.
- inputs: /ihme/limited_use/.../core_poi-part1 through core_poi-part5
- outputs: poi_crosswalk.csv

Step 1: Process Safegraph data
- name: prep_safegraph.R
- overview: Intakes the Safegraph Weekly Patterns dataset, which is expressed at the POI-week level and summarizes the number of
visits to restaurants and bars (separately) by US county and day (i.e. top category-count-day level).
- inputs: (1) weekly patterns data are stored as compressed csv files (.csv.gz) within the following directory /ihme/limited_use/LIMITED_USE/PROJECT_FOLDERS/COVID19/SAFEGRAPH/weekly_patterns/ 
	