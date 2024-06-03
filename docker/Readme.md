# Build docker image

Docker image to build the paper.

## Create the image

```
docker build -t joviart .
```

## Run

In the parent folder:
```
docker run -i -t -d --name=joviart_con --mount type=bind,source=.,target=/mnt/jovi joviart
```

## Connect

```
docker exec -it joviart_con /bin/bash
cd /mnt/jovi
quarto render index.qmd --to html
```