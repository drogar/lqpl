require 'ant'
require 'src/version'
require 'etc'

directory "out/production"
directory "out/production/lqpl_gui"

build = namespace :build do
  desc 'Copy JRuby files in preparation for JAR'
  task :copy_jruby => "out/production/lqpl_gui" do
    ant.copy :todir => "out/production/lqpl_gui" do
      fileset :dir => "src", :excludes => "**/*.java"
    end
  end
  desc 'Compile java files in preparation for JAR'
  task :compile => "out/production/lqpl_gui" do
    ant.javac :srcdir => "src", :destdir => "out/production/lqpl_gui", :includeAntRuntime => "no",
      :classpath => "lib/java/forms_rt.jar:lib/java/jruby-complete.jar:lib/java/monkeybars-1.1.1.jar"
  end
  desc 'Make a jar'
  task :jar => ["out/production", "Manifest", :compile, :copy_jruby] do
    ant.jar :destfile => "out/production/lqpl_gui.jar", :basedir => "out/production/lqpl_gui", :manifest => "Manifest"
  end

end



directory "out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-apple-darwin/bin"
directory "out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-apple-darwin/lib/java"
directory "out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-linux/bin"
directory "out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-linux/lib/java"
directory "out/lqpl-gui-#{LQPL_GUI_VERSION}-source"

dist = namespace :dist do
  desc "Make a mac binary"
  task :macbin => ["out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-apple-darwin/bin",
    "out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-apple-darwin/lib/java",build[:jar]] do
      sh "cp out/production/lqpl_gui.jar out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-apple-darwin/"
      sh "cp lib/java/* out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-apple-darwin/lib/java/"
      sh "cp LICENCE README  out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-apple-darwin/"
      if File.file?("/usr/local/bin/lqpl")
        sh "cp /usr/local/bin/lqpl* out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-apple-darwin/bin/"
      elsif File.file?("#{Etc.getpwuid.dir}/.cabal/bin/lqpl")
        sh "cp #{Etc.getpwuid.dir}/.cabal/bin/lqpl* out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-apple-darwin/bin/"
      else
        abort "No lqpl executables found!!!"
      end
      sh "tar --disable-copyfile -czf out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-apple-darwin.tgz out/lqpl-#{LQPL_GUI_VERSION}-bin-x86_64-apple-darwin"
  end
end
