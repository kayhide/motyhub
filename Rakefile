require 'standalone_migrations'
StandaloneMigrations::Tasks.load_tasks

namespace :db do
  desc 'Creates postgres user for all environments.'
  task :create_user => :environment do
    Rails.configuration.database_configuration.map do |env, conf|
      conf['username']
    end.uniq.compact.each do |username|
      `createuser -e -s -d #{username}`
    end
  end
end
