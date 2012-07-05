require 'etc'

LQPL_EMULATOR_DIR="../Emulator"
LQPL_COMPILER_DIR="../Compiler"
LQPL_COMMON_DIR="../Compiler"
HASKELL_SOURCE_DIRS=[LQPL_COMMON_DIR, LQPL_COMPILER_DIR, LQPL_EMULATOR_DIR]

SERVER_PROGRAMS=["lqpl-emulator", "lqpl", "lqpl-compiler-server"]

HASKELL_BIN_DIRS=["out/bin","/usr/local/bin", "#{Etc.getpwuid.dir}/.cabal/bin"]

DIST_INCLUDES=["../LICENCE","README", "../doc/lqplManual.pdf", "../lqplcode"]

GUI_EXCLUDE_FROM_SOURCE = [".", "..", "out", ".git", ".gitignore", "rawr_build_config.rb", "build.xml", "lqpl_gui.properties", "generated_build.xml", "TODO.txt"]

SERVER_EXCLUDE_FROM_SOURCE = [".", "..", "dist"]