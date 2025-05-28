# Copyright Spack Project Developers. See COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

from spack.package import *


class Dummy(CMakePackage):
    #homepage = "https://github.com/RMeli/"
    #git = "https://github.com/RMeli/"

    maintainers("RMeli")

    license("BSD-3-Clause")

    version("main", branch="main")

    generator("ninja")

    depends_on("fortran", type="build")
    depends_on("c", type="build")

    depends_on("cmake@3.22:", type="build")

    depends_on("mpi")
    depends_on("lapack")
    depends_on("scalapack")

    def cmake_args(self):
        args = []
        return args
