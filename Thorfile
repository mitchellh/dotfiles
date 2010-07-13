class Dotfiles < Thor
  include Thor::Actions

  def self.source_root
    File.dirname(__FILE__)
  end

  desc "install [NAME]", "Installs the dotfiles of the given type into your home directory."
  def install(name=nil)
    copy = []
    Dir["**/*"].each do |f|
      next if %W[Thorfile README.md].include?(f)
      next if File.directory?(f)
      next if name && !(f =~ /^#{name}/)

      file = home_file(f)

      # Deal with the file existing. We ignore directories and files which
      # are identical in content.
      next if File.exist?(file) && file_equal?(file, f)

      # Add the file to the proper copy. We don't do anything right away
      # so that its possibly to cancel at any point.
      copy << f
    end

    # Finally copy all the files over
    copy.each do |file|
      copy_file(file, home_file(file))
    end
  end

  protected

  def home_file(f)
    File.join(ENV["HOME"], ".#{f}")
  end

  def file_equal?(a, b)
    # Compares the contents of each file in memory, since the dotfiles are all
    # pretty small.
    File.open(a, "r") do |fa|
      File.open(b, "r") do |fb|
        return fb.read == fa.read
      end
    end
  end
end
