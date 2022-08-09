# CITE-formatting

This is a app designed to complement the [CITE-wrangling](https://github.com/bernibra/CITE-wrangling) pipeline, simplifying the process of adding new datasets.

### 1. Requirements

Check that the software and hardware requirements have been met.
* Docker 20.10

### 2. Clone the repo

Download or clone the repo: 
```
git clone https://github.com/bernibra/CITE-formatting.git
```

### 3. Build Docker image and run container

Type the following commands in the working directory (you might need [sudo rights](https://docs.docker.com/engine/install/linux-postinstall/)):
```
docker build --rm --force-rm -t cite-formatting .
docker run -p 3838:3838 cite-formatting
```

### 4. Introduce a new dataset

Open a web browser and type the url: `http://0.0.0.0:3838`

Fill in the form as accurately as possible. Answering the questions regarding how to read the data might require you to manually download some of the files. Unfortunately, there is no way around it. The [CITE-wrangling](https://github.com/bernibra/CITE-wrangling) will do its best to read the data as is; however, the more information, the more likely it is for it to run smoothly.

Once the form is completed, download the file and add it to [`./data/databases`](https://github.com/bernibra/CITE-wrangling/tree/main/data/databases). The file name will be the `id` of the new dataset with the corresponding `yaml` extension. If another dataset has the same `id`, you must change it for the new dataset, as the pipeline relies on those being unique.

If other information, regarding the samples for example, needs to be added, one can store it as a csv to [`./data/xtra_metadata`](https://github.com/bernibra/CITE-wrangling/tree/main/data/xtra_metadata). The file name must be the `id` of the new dataset with the corresponding `csv` extension.
