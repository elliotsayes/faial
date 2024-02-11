# About

- `base.Dockerfile` is a shared layer used by the following two containers;
   it installs all the required dependencies to compile Faial.

- `ci.Dockerfile` is used by the CI/CD server

- `dev.Dockerfile` demos downloading and building the toolchain from
  scratch

- `user.Dockerfile` a Docker container with the `faial` toolchain installed.


Example:

```
$ docker-compose build user
$ docker-compose run user
# faial-drf saxpy.cu
```
