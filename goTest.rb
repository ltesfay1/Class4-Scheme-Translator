#!/usr/bin/env ruby

# tests scheme.ml for public inputs

tests = [ "parse1", "parse2", "expr", "builtin", "lists", "define", 
          "lambda", "dynamic" ]

tests.each do
        |test|

        puts "TESTING: #{test}"
        system("ocaml public_#{test}.ml")
end

