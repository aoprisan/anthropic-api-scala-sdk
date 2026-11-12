# Releasing

Artifacts are published to Maven Central as
`io.github.aoprisan %% anthropic-api-scala-sdk` (and its `-cats-effect`, `-zio`, `-monix`, `-kyo` and `-ox` modules) by
[sbt-ci-release](https://github.com/sbt/sbt-ci-release), driven by
`.github/workflows/release.yml`.

Every module is cross-built for Scala 3 and Scala 2.13 (`crossScalaVersions` in
`build.sbt`), and sbt-ci-release publishes with `+publishSigned`, so each release
puts a `_3` and a `_2.13` artifact of every module on Central.

The Ox and Kyo modules are the exception: they are built for Scala 3.9 only (a `_3` artifact), and
are part of the build only on a JDK that can compile them (21 for Ox, 25 for Kyo). The release
workflow therefore runs on JDK 25; on an older JDK a release would silently leave them out.

The version comes from git tags via sbt-dynver, so there is no version to bump
in `build.sbt` or in the code (sbt-buildinfo hands it to the core, where the
`User-Agent` reads it):

- push to `main` → a `-SNAPSHOT` is published to the snapshot repository;
- push a tag `vX.Y.Z` → `X.Y.Z` is published as a release.

```sh
git tag -a v0.1.0 -m "v0.1.0"
git push origin v0.1.0
```

Releases land in the Central Portal staging area and are then published;
they show up on https://repo1.maven.org/maven2/io/github/aoprisan/ within
roughly 10–30 minutes.

## One-time setup

This has to be done once by the repository owner; the workflow cannot
publish until all four secrets exist.

### 1. Claim the `io.github.aoprisan` namespace

Sign in to https://central.sonatype.com with the GitHub account `aoprisan`
and add the namespace `io.github.aoprisan`. Signing in with GitHub verifies
the namespace automatically.

### 2. Generate a user token

Central Portal → *View Account* → *Generate User Token*. This yields a
username/password pair — these are **not** the portal login credentials.

### 3. Create a PGP key

```sh
gpg --gen-key            # real name + email, note the passphrase
gpg --list-keys          # copy the long key id
gpg --keyserver keyserver.ubuntu.com --send-keys $LONG_ID
gpg --armor --export-secret-keys $LONG_ID | base64 -w0
```

The key must be on a public keyserver or Central will reject the signatures.

### 4. Add the GitHub Actions secrets

Repository → *Settings* → *Secrets and variables* → *Actions*:

| Secret              | Value                                              |
| ------------------- | -------------------------------------------------- |
| `SONATYPE_USERNAME` | user token username from step 2                     |
| `SONATYPE_PASSWORD` | user token password from step 2                     |
| `PGP_SECRET`        | base64 armored secret key from step 3               |
| `PGP_PASSPHRASE`    | passphrase for that key                             |

## Publishing from a laptop instead

Not recommended — CI has the credentials — but `sbt +publishSigned` works
with the same PGP key and a `~/.sbt/1.0/sonatype.sbt` holding the token
credentials. For local testing against another project, `sbt +publishLocal`
is enough; CI runs it on every push, so a packaging problem (POM, Scaladoc)
shows up before a tag does.
