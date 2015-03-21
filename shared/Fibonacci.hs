module Shared.Fibonacci where

fibs a b = a : b : fibs' a b
  where fibs' a b = (a+b) : fibs' b (a+b)
