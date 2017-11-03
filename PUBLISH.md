### PGP Signatures

Make sure that you have your personal PGP key to sign the code (typically it is located at `.gnugp` if you have
used the `gpg` tool to generate it).

Your PGP needs to be distributed to the keyserver pool (e.g. [MIT PGP Public Key Server](http://pgp.mit.edu:11371/)).

### Sonatype integration

You need to specify your Sonatype account credentials in your private SBT configuration (i.e. not in this repository).
Just add the following to the `~/.sbt/1.0/sonatype.sbt` file:

```scala
credentials += Credentials("Sonatype Nexus Repository Manager",
                           "oss.sonatype.org",
                           "<your username>",
                           "<your password>")
```

### Publish snapshot version

Make sure that you have specified some snapshot version for Scavenger (e.g. `0.2.1-SNAPSHOT`). Then open the SBT shell
and type the `publishSigned` command to publish Scavenger to
[Sonatype Snapshot repository](https://oss.sonatype.org/content/repositories/snapshots/org/aossie/).

### Publish release version

Make sure that you have specified some release version for Scavenger (e.g. `0.2`). Then open the SBT shell
and type the `publishSigned` command to publish Scavenger to the staging repository. You can (and should) check
if everything is OK with your release by visiting
[Nexus Repository Manager](https://oss.sonatype.org/#stagingRepositories). If you think that your release is ready
to be available for everyone just type the `sonatypeRelease` command to publish Scavenger to
[Sonatype Release repository](https://oss.sonatype.org/content/repositories/releases/org/aossie/).
