### GEO download

GEO data sources are tricky because there are many variables in how the files are structured and formatted.

The first important thing is providing a valid GEO id. This will also be used as the datafile id, so it has to be unique. Then, one can download GEO data using the R package GEOquery or by directly providing the relevant url links. 

If you choose using the GEOquery software, the app will download the metadata and will attempt to filter out the different files.

If the GEO data source is too difficult to process using GEOquery, you can also specify the files that need to be downloaded manually. To do so, you can use the option 'direct download', which will provide you a freeformat way to download the data, provided that you have the links for the direct download. With this option, every file needs to be downloaded directly. Provide a unique id to the dataset, and upload the necessary files. Metadata files are optional. Protein, RNA and HTO data are provided separately. If all files come in a single file, use only the protein data field. Note that one important variable when specifying the files to be downloaded is the 'grouping factor'. This allows you to specify files that need to be processed together. Imagine for example, that samples come separately in different files, and that each sample has three files&mdash;e.g. 'matrix.mtx', 'featres.csv', and 'barcodes.csv'. The grouping factor, allows you to match the files based on the name you gave them. For example, when entering the files for say 'sample 1', you can specify the names as: 'S1_matrix.mtx', 'S1_featres.csv', and 'S1_barcodes.csv'. Then, you can use 'S1' as your grouping factor to specify that these three names need to be processed together. 
