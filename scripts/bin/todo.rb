#!/usr/bin/env ruby
require 'yaml'

# Represents a single todo item. An Item contains just a text and
# a context.
#
class Item
  attr_accessor :context, :text

  # Creates a new Item instance in-memory.
  #
  # value - The text of the Todo. Context is extracted if exists.
  #
  # Returns the unpersisted Item instance.
  def initialize(value)
    @context = value.scan(/@[A-Z0-9.+-]+/i).last || '@next'
    @text    = value.gsub(context, '').strip
  end

  # Overide: Quick and simple way to print Items
  #
  # Returns String for this Item
  def to_s
    "#{@text}: #{@context}"
  end
end

# The Todo contains many Items. They exist as buckets in which to categorize
# individual Items. The relationship is maintained in a simple array.
#
class Todo

  # Creates a new Todo instance in-memory.
  #
  # Returns the persisted Todo instance.
  def initialize(options = {})
    @options, @items = options, []
    bootstrap
    load_items
  end

  # The main todos in the user's home directory
  FILE = File.expand_path('.todos')

  # Allow to items to be accessible from the outside
  attr_accessor :items

  # Creates a new todo
  #
  # Example:
  #   @todo.add('lorem epsim etc @work')
  #
  # Returns the add todo Item
  def add(todo)
    @items << Item.new(todo)
    save
    @items.last
  end

  # Removes the todo
  #
  # Example:
  #   @todo.delete(1)
  #
  # Returns the deleted todo Item
  def delete(index)
    todo = @items.delete_at(index.to_i-1)
    save
    todo
  end

  # Marks a todo as done
  #
  # Example:
  #   @todo.done(1)
  #
  # Returns the done todo Item
  def done(index)
    item = @items[index.to_i-1]
    item.context = '@done'
    save
    item
  end

  # Prints all the active todos in a nice neat format
  #
  # Examples:
  #   @todo.list @work
  #
  # Returns nothing
  def list
    longest = @items.map(&:text).max_by(&:length) || 0
    @items.each_with_index do |todo, index|
      if @items.length > 9
        printf "%2s: %-#{longest.size+5}s %s\n", index+1, todo.text, todo.context
      else
        printf "%s: %-#{longest.size+5}s %s\n", index+1, todo.text, todo.context
      end
    end
  end

  # Prints all the used tags
  #
  # Examples:
  #   @todo.tags
  #
  def tags
    puts to_hash.keys.sort
  end

  # Moves a todo up or down in priority
  #
  # Example:
  #   @todo.bump(2, +1)
  #
  def bump(index, position = 1)
    @items.insert(position-1, @items.delete_at(index.to_i-1))
    save
    @items[position.to_i-1]
  end

  # Accessor for the todo list file
  #
  # Returns String file path
  def file
    @file ||= File.exist?(FILE) ? FILE : "#{ENV['HOME']}/.todos"
  end

  # Formats the current set of todos
  #
  # Returns a lovely hash
  def to_hash
    @items.group_by(&:context).inject({}) do |h,(k,v)|
      h[k.to_sym] = v.map(&:text); h
    end
  end

  # Loads the yaml todos file and creates a hash
  #
  # Returns the items loaded from the file
  def load_items
    YAML.load_file(file).each do |key, texts|
      texts.each do |text|
        if key.to_s == @options[:filter] || @options[:filter].nil?
          @items << Item.new("#{text} #{key}") if key.to_s != '@done'
        end
      end
    end
    @items
  end

  # Implodes all the todo items save an empty file
  #
  # Returns nothing
  def clear!
    @items.clear
    save
  end

  private

  # Saves the current list of todos to disk
  #
  # Returns nothing
  def save
    File.open(file, "w") {|f| f.write(to_hash.to_yaml) }
  end

  # Creates a new todo file if none is present
  #
  # Returns nothing
  def bootstrap
    return if File.exist?(file)
    save
  end

end

if __FILE__ == $0
  case ARGV[0]
  when 'list','ls'
    Todo.new(:filter => ARGV[1]).list
  when 'add','a'
    puts "Added: #{Todo.new.add(ARGV[1..-1].join(' '))}"
  when 'delete', 'del', 'd'
    puts "Deleted: #{Todo.new.delete(ARGV[1])}"
  when 'done', 'do'
    puts "Done: #{Todo.new.done(ARGV[1])}"
  when 'edit', 'e'
    system("`echo $EDITOR` #{Todo.new.file} &")
  when 'clear', 'c'
    puts "All #{Todo.new.clear!} todos cleared! #{Todo.new.clear!}"
  when 'bump', 'b'
    puts "Bump: #{Todo.new.bump(ARGV[1])}"
    Todo.new.list
  when 'tags', 't'
    puts "All tags:"
    Todo.new.tags
  else
    puts "\nUsage: todo [options] COMMAND\n\n"
    puts "Commands:"
    puts "  add TODO        Adds a todo"
    puts "  delete NUM      Removes a todo"
    puts "  done NUM        Completes a todo"
    puts "  list [CONTEXT]  Lists all active todos"
    puts "  tags            Lists all the used tags"
    puts "  bump NUM        Bumps priority of a todo"
    puts "  edit            Opens todo file"
  end
end
