# navigate

New telescope control tool for the Gemini Observatory

# Deployment

A Docker image is automatically built and deployed to Dockerhub as `noirlab/gpp-nav-server` when a PR is merged into the `main` branch.

# Locally desting deployment

To build `navigate-server` Docker image in your local installation, run in `sbt`:

```
navigate_deploy/docker:publishLocal
```

# Running locally with HTTPS

A Navigate server started for development (`navigate_web_server/reStart`) serves plain HTTP on port 9090, because `base.conf` has no TLS configuration. Deployed servers use HTTPS, so to reproduce a deployment-like setup locally (e.g. to test a client like Observe connecting over HTTPS) the server has to be given a TLS configuration.

The server reads `conf/<SITE>/site.conf` from the directory above its classes, falling back to `base.conf`. When started with `reStart` that directory is `navigate/web/server/target/conf`, and `SITE` defaults to `develop`. To serve HTTPS with the self-signed development certificate in `navigate/deploy/conf/local`:

1. From the repository root, create the site configuration (it lives in `target`, so it is not committed, and `sbt clean` deletes it):

   ```
   mkdir -p navigate/web/server/target/conf/local
   cat > navigate/web/server/target/conf/local/site.conf <<EOF
   web-server {
       tls {
           key-store = "$PWD/navigate/deploy/conf/local/cacerts.jks.dev"
           key-store-pwd = "passphrase"
           cert-pwd = "passphrase"
       }
   }
   EOF
   ```

2. Start the server with that site:

   ```
   SITE=local sbt navigate_web_server/reStart
   ```

   The start-up log lists the configuration files it loads, and should show `conf/local/site.conf (present: true)`. The server then serves HTTPS on `https://localhost:9090`.

The certificate is self-signed for `local.lucuma.xyz`, so clients must not verify it. Observe does not verify Navigate's certificate, nor Navigate Observe's, as they run in the same closed network. See the Observe README to point Observe at a Navigate server over HTTPS.

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
