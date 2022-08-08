# CITE-formatting

This is a app designed to complement the CITE-wrangling pipeline, simplifying the process of adding new datasets to it.

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
docker run -p 4000:3838 cite-formatting
```

