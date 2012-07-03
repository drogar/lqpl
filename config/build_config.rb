require 'etc'

LQPL_SERVER_DIR="#{Etc.getpwuid.dir}/programming/haskell/lqpl/"

HASKELL_BIN_DIRS=["/usr/local/bin", "#{Etc.getpwuid.dir}/.cabal/bin"]

BIN_INCLUDES=["LICENCE","README", LQPL_SERVER_DIR+"/distribution/doc/lqplManual.pdf", LQPL_SERVER_DIR+"/distribution/doc/examplecode"]

GUI_EXCLUDE_FROM_SOURCE = [".", "..", "out", ".git", ".gitignore", "rawr_build_config.rb", "build.xml", "lqpl_gui.properties", "generated_build.xml", "TODO.txt"]

SERVER_EXCLUDE_FROM_SOURCE = [".", "..", "dist"]