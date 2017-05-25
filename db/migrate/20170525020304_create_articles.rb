class CreateArticles < ActiveRecord::Migration[5.1]
  def change
    create_table :articles do |t|
      t.references :blog, foreign_key: true
      t.string :title
      t.text :body
      t.string :basename

      t.timestamps
    end
  end
end
