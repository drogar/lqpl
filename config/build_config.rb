require 'etc'

EXCLUDE_FROM_SOURCE_DIST=["./out", "./.git", "./.gitignore", "./dist", "./.DS_Store", "./.", "./.."]


SERVER_PROGRAMS=["lqpl-emulator", "lqpl", "lqpl-compiler-server"]

HASKELL_BIN_DIRS=["out/bin","/usr/local/bin", "#{Etc.getpwuid.dir}/.cabal/bin"]

DIST_INCLUDES=["LICENCE","README", "doc/lqplManual.pdf", "lqplcode"]

