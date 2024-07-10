### Loading GEO data

There are several parameters that matter when deciding how to process the data coming from GEOquery. It all depends on how files are organized. If protein, RNA and HTO data come all together under one `geo_accession` key, one can define a keyword to identify the right file in each case. If different file types come under different `geo_accession` keys, one has to sort this out in the previous step. 

If different data types are processed differently, one can specify this here by checking the corresponding box and specifying the protocol for each file type accordingly.

The options 'transpose' and 'merge different files together' are important because the pipeline is naive regarding how the data is structured. In terms of the first parameter, the pipeline assumes that rows in the expression matrix correspond to genes/proteins. One should select 'transpose' if that is not the case. Regarding 'merge different files together', this is a parameter that will tell the pipline to try to merge the different samples/files. For example, if samples came in as separate files, the pipeline will attempt to merge them together after processing.  You should unselect 'merge different files together' if you don't want this to happen.

If protein, RNA and HTO data come all together (or multiple if files are separated by sample or tissue), one can define 'search file by pattern', a keyword which will be used by regex to identify the right file in each case (this will happen if the data was compressed as a tar file and one can only dowload everything together). If protein, RNA and HTO data come in different files, one can ignore the keyword argument or use it to identify the right file (in cases where other miscellaneous files are in the folder).

Another interesting parameter is 'rows moved to colData'. This parameter allows you to remove any rows found in the expression matrix that should be elsewhere. This is important because some datasets add rows such as "library size" to the expression matrix, which can cause issues later on in the data processing pipeline. If multiple rows need to be removed, these can be specified together, separated by semicolons. The removed rows will be added to the colData.

