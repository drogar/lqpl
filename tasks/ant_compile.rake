require 'ant'

directory "out/production"
directory "out/production/lqpl_gui"

namespace :build do
  desc 'Copy JRuby files in preparation for JAR'
  task :copy_jruby => "out/production/lqpl_gui" do
    ant.copy :todir => "out/production/lqpl_gui" do
      fileset :dir => "src", :excludes => "**/*.java"
    end
  end
  desc 'Compile java files in preparation for JAR'
  task :compile => "out/production/lqpl_gui" do
    ant.javac :srcdir => "src", :destdir => "out/production/lqpl_gui"
  end
  desc 'Make a jar'
  task :jar => ["out/production", "Manifest", :compile, :copy_jruby] do
    ant.jar :destfile => "out/production/lqpl_gui.jar", :basedir => "out/production/lqpl_gui", :manifest => "Manifest"
  end

end