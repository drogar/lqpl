# encoding: utf-8
require 'pry'

module Monkeybars
  # Resolver in the monkeybars, for loading code files
  class Resolver
    IN_FILE_SYSTEM = :in_file_system
    IN_JAR_FILE = :in_jar_file

    MONKEYBARS_JAR_LOCATIONS = %w(/../lib/java/*.jar /../../lib/java/*.jar /../../../lib.java/*.jar).freeze

    attr_reader :location

    def initialize(location: __FILE__)
      @location = location || ''
    end

    # Returns a const value indicating if the currently executing code is being run from the
    # file system or from within a jar file.
    def run_location
      @run_location ||= (File.expand_path(location) =~ /\.jar\!/ ? IN_JAR_FILE : IN_FILE_SYSTEM)
    end

    def in_file_system?
      run_location == IN_FILE_SYSTEM
    end

    def monkeybars_jar(path)
      Dir.glob(path).find { |f| f =~ /(monkeybars-)(.+).jar$/ }
    end

    def bare_path
      return location_path[5, location_path.length - 18] if in_file_system?
      location_path
    end

    def location_path
      File.expand_path File.dirname(location)
    end

    def add_monkeybars_jar_when_in_file_system
      return unless in_file_system?
      add_monkeybars_jar
    end

    private

    def add_monkeybars_jar
      # ===================================================================
      # Monkeybars requires, this pulls in the requisite libraries needed
      # for Monkeybars to operate.
      #    npath = here + '/../lib/java/*.jar'
      #    puts npath
      MONKEYBARS_JAR_LOCATIONS.each do |mb_path|
        pathing_add_to_classpath(monkeybars_jar(location_path + mb_path))
        #    puts "resulting mbj=#{mbj}"
        #     puts "or is #{mbj}"
      end
    end

    def pathing_add_to_classpath(path)
      return if path.nil? || path.empty?
      Pathing.add_to_classpath(path, file: location)
    end
  end
end
