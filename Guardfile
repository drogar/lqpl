# A sample Guardfile
# More info at https://github.com/guard/guard#readme

guard :rspec do
  watch(/^spec\/.+_spec\.rb$/)
  watch(/^lib\/(.+)\.rb$/)     { |m| "spec/lib/#{m[1]}_spec.rb" }
  watch('spec/spec_helper.rb')  { 'spec' }

  # Rails example
  watch(%r{^GUI/src/(.+)\.rb$})                       { |m| "spec/#{m[1]}_spec.rb" }
  watch(%r{^spec/support/(.+)\.rb$})                  { 'spec' }
  watch(%r{^GUI/src/application(.+)\.rb})             { 'spec' }

  # Capybara features specs
  # watch(%r{^app/views/(.+)/.*\.(erb|haml|slim)$})     { |m| "spec/features/#{m[1]}_spec.rb" }

  # Turnip features and steps
  # watch(%r{^spec/acceptance/(.+)\.feature$})
end

guard :rubocop do
  watch(/^GUI\/.+\.rb$/) { |m| File.dirname(m[0]) }
  watch(/^features\/.+\.rb$/) { |m| File.dirname(m[0]) }
  watch(/^spec\/.+\.rb$/) { |m| File.dirname(m[0]) }
  watch(/^rake.+\.rb$/) { |m| File.dirname(m[0]) }
  watch(/^tasks\/.+\.rb$/) { |m| File.dirname(m[0]) }
  watch(/(?:.+\/)?\.rubocop\.yml$/) { |m| File.dirname(m[0]) }
end

notification :tmux,
             display_message: true,
             timeout: 5, # in seconds
             default_message_format: '%s >> %s',
             # the first %s will show the title, the second the message
             # Alternately you can also configure *success_message_format*,
             # *pending_message_format*, *failed_message_format*
             line_separator: ' > ', # since we are single line we need a separator
             color_location: 'status-left-bg' # to customize which tmux element will change color
