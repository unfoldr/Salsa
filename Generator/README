Salsa Binding Generator
-----------------------

Generates a Haskell module (called 'Bindings') that contains the binding code
required to interface with specific .NET classes and their members.  An import
file provides a list of the classes and their assemblies.

Usage:
  Generator.exe Project.imports

where 'Project.imports' is a file that lists (on separate lines):

- The classes to import (optionally followed by a comma-separated list of the
  members from the class to import).  For example:

    System.Environment                  (binds all members of the Environment
                                         class)

    System.Console: Write, WriteLine    (binds only the Write and WriteLine
                                         methods of the Console class)

- The assemblies that hold the imported classes.  For example:

    reference C:\Windows\Microsoft.NET\Framework\v2.0.50727\System.dll

      (to make a reference to the .NET 2.0 System assembly)

Building:
  To build Generator.exe, just run 'msbuild' in the 'Generator' directory.

--
Andrew Appleyard <andrew.appleyard@gmail.com>
