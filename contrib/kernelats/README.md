# kernelats

The freestanding-ATS floor: a prelude of C macro implementations
(`prelude/CATS/`), the compilation-scheme headers (`ccomp/`), and the
`staloadall.hats` that binds the prelude templates -- everything
needed to compile ATS2 with no runtime, no allocation, and no libc,
which is how the Overture runtime and firmware stubs are built.

Vendored verbatim from ATS-Postiats-contrib
(github.com/githwxi/ATS-Postiats-contrib, `contrib/kernelats`); each
file carries its original license header. Pinned here because the
released contrib tarballs predate these files, and this floor must
stay stable for the life of the project.
