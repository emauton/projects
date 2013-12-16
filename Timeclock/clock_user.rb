class ClockUser < ActiveRecord::Base
  has_many :time_slots, :order => 'finish'
end
