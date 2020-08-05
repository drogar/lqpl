# class to help choos a file fixture
class JFileChooserFixture
  def project_sub_directory(dir_string)
    cdir = Dir.getwd
    dirs = dir_string.split('/')
    dirs.each do |d|
      cdir = java.io.File.new(cdir, d)
      set_current_directory(cdir)
      sleep 0.1
    end
    cdir
  end

  def select_file_in_project_directory(dir_string, file_name)
    pdir = project_sub_directory(dir_string)
    select_file(java.io.File.new(pdir, file_name))
  end
end

# monkey patch file for cucumber only
class File
  def self.file_in_project_subdir(subdir, file)
    File.join(Dir.getwd, subdir, file)
  end
end
