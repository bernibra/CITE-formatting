### `data/databases` files

This app will generate a yaml file, which will instructions for downloading and loading a single dataset. There are multiple data sources&mdash;GEOquery, ArrayExpress, direct downloads, etc&mdash;and there is essential information that one needs to provide to work with each data source. 

Each dataset/experiment is defined by a unique identifier that defines in turn the name of the corresponding yaml file. For some datasets this is obtained from the accession key. Other datasets, however, will use the alias as a unique identifier.

