PureScript-Simple-JSON Guide and Docs
=====================================

This is a guide for the PureScript library `Simple-JSON <https://github.com/justinwoo/purescript-simple-json>`_, which provides an easy way to decode either ``Foreign`` (JS) values or JSON ``String`` values with the most "obvious" representation. This guide will also try to guide you through some of the details of how PureScript the language works, as you may be new to PureScript or not know its characteristics.

Overall, this library provides you largely automatic ways to get decoding, but does not try to decode any types that do not actually have a JS representation. This means that this library does not provide you with automatic solutions for decoding and encoding Sum or Product types, but there is a section below on how to use Generics in PureScript to achieve the encoding of Sum and Product types that you want.

.. tip:: If you are coming from Elm, you can think of this library as providing the automatic encoding/decoding of ports, but actually giving you explicit control of the results and allowing you to define encodings as you need.

.. note:: If there is a topic you would like more help with that is not in this guide, open a issue in the Github repo for it to request it.

Pages
==================

.. toctree::

  intro
  quickstart
  inferred-record-types
  with-affjax
  generics-rep
  faq
