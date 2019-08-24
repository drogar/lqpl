require_relative 'pathing'
Pathing.requires_and_imports_for_lqpl(self)

# for swinging
# rubocop:disable Style/MixinUsage
include Swingtown::Core
# rubocop:enable Style/MixinUsage

# end for swinging
Pathing.lqpl_requires
