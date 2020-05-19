-- from https://github.com/nodew/hlines/blob/master/src/HLines/Type.hs

{-# LANGUAGE DeriveDataTypeable #-}

module CopyrightHeader.LanguageTypes where

import Protolude

import Data.Data
import Data.ByteString as BS

data Language
  = ActionScript
  | Ada
  | Agda
  | AmbientTalk
  | Asp
  | AspNet
  | Assembly
  | Autoconf
  | Awk
  | Batch
  | BourneShell
  | C
  | CCppHeader
  | CMake
  | CSharp
  | CShell
  | Clojure
  | CoffeeScript
  | ColdFusion
  | ColdFusionScript
  | Coq
  | Cpp
  | Css
  | CUDA
  | CUDAHeader
  | D
  | Dart
  | DeviceTree
  | Docker
  | Elixir
  | Elm
  | Erlang
  | Forth
  | FortranLegacy
  | FortranModern
  | FSharp
  | Gherkin
  | Glsl
  | Go
  | Groovy
  | Handlebars
  | Haskell
  | Hex
  | Html
  | INI
  | Idris
  | IntelHex
  | Isabelle
  | Jai
  | Java
  | JavaScript
  | Json
  | Jsx
  | Julia
  | Kotlin
  | Less
  | LinkerScript
  | Lean
  | Lisp
  | Lua
  | Make
  | Makefile
  | Markdown
  | Mustache
  | Nim
  | Nix
  | OCaml
  | ObjectiveC
  | ObjectiveCpp
  | OpenCl
  | Oz
  | Pascal
  | Perl
  | PHP
  | Polly
  | PowerShell
  | Prolog
  | Protobuf
  | Puppet
  | PureScript
  | Pyret
  | Python
  | Qcl
  | Qml
  | R
  | Razor
  | ReStructuredText
  | Ruby
  | RubyHtml
  | Rust
  | SaltStack
  | Sass
  | Scala
  | Sml
  | Sql
  | Stylus
  | Swift
  | Tcl
  | Terraform
  | Tex
  | Text
  | Toml
  | TypeScript
  | Tsx
  | UnrealScript
  | VimScript
  | Wolfram
  | XML
  | Yacc
  | Yaml
  | Zig
  | Zsh
  | Haxe
  | Unknown
  deriving (Show, Data, Eq, Ord)

type SingleComment = Text
type SingleComments = [SingleComment]

type MultiComment = (Text, Text)
type MultiComments = [MultiComment]

data Comment = Comment
  { single :: SingleComments
  , multi :: MultiComments
  }
  deriving (Show, Eq)
