require 'ant'
require 'src/version'
require 'config/build_config'

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
mac=false
case Config::CONFIG["host_os"]
when /darwin/i # OSX specific code
  tech="x86_64-apple-darwin"
  tar_options = "--disable-copyfile"
  mac=true
when /^win|mswin/i # Windows specific code
  tech="x86_32-mswin"
  tar_options = ""
when /linux/i # Linux specific code
  tech="x86_64-linux"
  tar_options = ""
end

directory "out"
directory "out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/bin"
directory "out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/lib/java"
directory "out/lqpl-gui-#{LQPL_GUI_VERSION}-source"
directory "out/lqpl-server-#{LQPL_GUI_VERSION}-source"

directory "out/LQPLEmulator.app/Contents/MacOS"
directory "out/LQPLEmulator.app/Contents/PkgInfo"
directory "out/LQPLEmulator.app/Contents/Resources/Java/bin"
directory "out/LQPLEmulator.app/Contents/Resources/Java/lib/java"

dist = namespace :dist do
  desc "Make a #{tech} binary distribution"
  task :binary => ["out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/bin", "out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/lib/java",build[:jar]] do
    sh "cp -a out/production/lqpl_gui.jar out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/"
    sh "cp -a lib/java/* out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/lib/java/"
    BIN_INCLUDES.each do |f|
      sh "cp -a #{f}  out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/" if File.file?(f)
      sh "cp -aR #{f}  out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/" if File.directory?(f)
    end
    copy_server_bin "out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}/bin/"
    $stdout << "Creating tar file: out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}.tgz"
    sh "tar #{tar_options} -czf out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}.tgz out/lqpl-#{LQPL_GUI_VERSION}-bin-#{tech}"
  end
  desc "Make a gui source distribution"
  task :gui_source =>["out/lqpl-gui-#{LQPL_GUI_VERSION}-source"] do
    Dir.foreach(".") do |gsource|
      sh "cp -aR #{gsource} out/lqpl-gui-#{LQPL_GUI_VERSION}-source" if not GUI_EXCLUDE_FROM_SOURCE.include? gsource
    end
    $stdout << "Creating tar file: out/lqpl-gui-#{LQPL_GUI_VERSION}-source.tgz"
    sh "tar #{tar_options} -czf out/lqpl-gui-#{LQPL_GUI_VERSION}-source.tgz out/lqpl-gui-#{LQPL_GUI_VERSION}-source"
  end
  desc "Make a server source distribution"
  task :server_source =>["out/lqpl-server-#{LQPL_GUI_VERSION}-source"] do
    Dir.foreach(LQPL_SERVER_DIR+"distribution") do |ssource|
      sh "cp -aR #{LQPL_SERVER_DIR}distribution/#{ssource} out/lqpl-server-#{LQPL_GUI_VERSION}-source" if not SERVER_EXCLUDE_FROM_SOURCE.include? ssource
    end
    $stdout << "Creating tar file: out/lqpl-server-#{LQPL_GUI_VERSION}-source.tgz"
    sh "tar #{tar_options} -czf out/lqpl-server-#{LQPL_GUI_VERSION}-source.tgz out/lqpl-server-#{LQPL_GUI_VERSION}-source"
  end
  if mac
    task :mac_dirs => ["out/LQPLEmulator.app/Contents/MacOS", "out/LQPLEmulator.app/Contents/PkgInfo","out/LQPLEmulator.app/Contents/Resources/Java/bin","out/LQPLEmulator.app/Contents/Resources/Java/lib/java"]
    desc "make a mac app"
    task :mac_app => [:mac_dirs,build[:jar]] do
      sh "cp -a /System/Library/Frameworks/JavaVM.framework/Versions/A/Resources/MacOS/JavaApplicationStub out/LQPLEmulator.app/Contents/MacOS/"
      sh "cp -a config/Info.plist out/LQPLEmulator.app/Contents/"
      sh "cp -a icons/lqpl.icns out/LQPLEmulator.app/Contents/Resources"
      sh "cp -a out/production/lqpl_gui.jar out/LQPLEmulator.app/Contents/Resources/Java"
      sh "cp -a lib/java/* out/LQPLEmulator.app/Contents/Resources/Java/lib/java/"
      copy_server_bin "out/LQPLEmulator.app/Contents/Resources/Java/bin/"
    end
  end
end

def copy_server_bin(to_dir)
  bin_copied = false
  HASKELL_BIN_DIRS.each do |d|
    if File.file?("#{d}/lqpl") and not bin_copied
      sh "cp -a #{d}/lqpl* #{to_dir}"
      bin_copied=true
    end
  end
  abort "No lqpl executables found!!!" if not bin_copied
end