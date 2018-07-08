Introduction
============

What is ``Foreign``?
--------------------

In PureScript, untyped JS values are typed as ``Foreign`` and are defined in the Foreign_ library. Usually when you define FFI functions, you should define the results of the functions as ``Foreign`` and then decode them to a type if you want to ensure safety in your program.

.. _Foreign: https://pursuit.purescript.org/packages/purescript-foreign

For example, this library exposes the method parseJSON_ with the type

.. code-block:: haskell

  parseJSON :: String -> F Foreign

We'll visit what this ``F`` failure type is later, since you won't need to use it most of the time when you use this library.

.. _parseJSON: https://pursuit.purescript.org/packages/purescript-simple-json/4.0.0/docs/Simple.JSON#v:parseJSON

How you should use this library
-------------------------------

Generally, you should try to separate your transport types from your domain types such that you never try to tie down the model used in your program to whatever can be represented in JS. For example, a sum type

.. code-block:: haskell

  data IsRegistered
    = Registered DateString
    | NotRegistered

is the correct model to use in your program, while the transport may be defined

.. code-block:: haskell

  type RegistrationStatus =
    { registrationDate :: Maybe DateString
    }

While you `could` use ``Maybe DateString`` all over your application, this type suffers in that there is just not much information for your users to take from this type. If you used a newtype of this, the actual matching usages would still suffer the same problem.

.. note:: Many users complain that Simple-JSON should provide automatic serialization of sum types, but you'll find that preferred encodings for sum types are like opinions -- everyone has one. Instead of giving you a default that wouldn't make sense in the scope of Simple-JSON as providing decoding for JS-representable types, we'll go over how PureScript's Generics-Rep work and how easy it is for you to work with sum types with your preferred methods.
