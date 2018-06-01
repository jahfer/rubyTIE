module Foo
  include ActiveSupport::Concern 

  included do |base|
    perform_extensions(base)
  end

  def self.perform_extensions(base)
    base.extend(SomeOtherRandomModule)
  end

  module ClassMethods
    # I give up
  end
end
