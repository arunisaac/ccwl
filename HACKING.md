# Set up development environment

Drop into a development environment using `guix shell`.
```
guix shell -Df guix.scm
```

# Make a release
## Tag a release
Tag a release `vx.x.x` putting news into the tag message.
## Create a release tarball, test it, and sign it
```
cp $(guix build -L .guix -f .guix/ccwl-distribution.scm) ccwl-x.x.x.tar.lz
guix build --with-source=ccwl=ccwl-x.x.x.tar.lz -f guix.scm
make distsign
```
## Build guix pack, docker and singularity images
```
guix pack --with-source=ccwl=ccwl-x.x.x.tar.lz --file=guix.scm
guix pack -f docker -S /bin=bin --with-source=ccwl=ccwl-x.x.x.tar.lz --file=guix.scm
guix pack -f squashfs --with-source=ccwl=ccwl-x.x.x.tar.lz bash --file=guix.scm
```
## Publish release tarball
Add release tarball and signature to website. Publish release tarball, guix pack, docker and singularity images to GitHub.
## Update Guix package
## Publicize
Publicize on the ccwl@systemreboot.net and guix-science@gnu.org mailing lists, and on the [CWL Discourse forum](https://cwl.discourse.group/).
