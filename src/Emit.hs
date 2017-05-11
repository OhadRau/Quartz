module Emit where

class Emit t where
  emit :: t -> String
