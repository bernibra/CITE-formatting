### Loading GEO data

There are several parameters that matter when deciding how to process the data coming from GEOquery. It all depends on how files are organized. If protein, RNA and HTO data come all together under one `geo_accession` key, one can define a keyword to identify the right file in each case. If different file types come under different `geo_accession` keys, one has to sort this out in the previous step. 

If different data types are processed differently, one can specify this here by checking the corresponding box and specifying the protocol for each file type accordingly.
