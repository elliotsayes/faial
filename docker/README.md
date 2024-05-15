# About

- `base.Dockerfile` is a shared layer used by the following two containers;
   it installs all the required dependencies to compile Faial.

- `ci.Dockerfile` is used by the CI/CD server

- `dev.Dockerfile` demos downloading and building the toolchain from
  scratch

- `user.Dockerfile` a Docker container with the `faial` toolchain installed.


Hello, world:
```
$ docker-compose build user
$ docker-compose run user
# faial-drf saxpy.cu
```

If you want to mount a directory without editing `docker-compose.yaml`, then:
```
$ docker-compose build user
$ docker run -it --mount type=bind,source=path/to/cuda-files,target=/home/faial user
```
