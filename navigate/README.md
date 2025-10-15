# navigate

New telescope control tool for the Gemini Observatory

# Deployment

A Docker image is automatically built and deployed to Dockerhub as `noirlab/gpp-nav-server` when a PR is merged into the `main` branch.

# Locally desting deployment

To build `navigate-server` Docker image in your local installation, run in `sbt`:

```
navigate_deploy/docker:publishLocal
```

# Running in Test and Production

## Configuration

Deployment needs configuration that can be shared in the repos, like the TLS certificate and its key. For this, the server expects a directory called `conf/local` to be mounted in the container. A local directory must be [bind mounted](https://docs.docker.com/storage/bind-mounts/) into the container, providing a local `app.conf` and other needed files.

For example, assuming you have a local directory `/opt/navigate/local` with a file `app.conf` with the following content:

```
web-server {
    tls {
        key-store = "conf/local/cacerts.jks.dev"
        key-store-pwd = "passphrase"
        cert-pwd = "passphrase"
    }
}

etc...
```

You can run the container with the following command:

```
docker run -p 443:9090 --mount type=bind,src=/opt/navigate/local,dst=/opt/docker/conf/local noirlab/gpp-nav-server:latest
```

NOTE: The image is created with a self-signed TLS certificate in `conf/local` for testing purposes. Please remember to mount a local volume instead to use the correct testing or production certificate.
