class CreateBlogs < ActiveRecord::Migration[5.1]
  def change
    create_table :blogs do |t|
      t.string :host_url
      t.string :username
      t.string :password
      t.string :url
      t.string :title

      t.timestamps
    end
  end
end
