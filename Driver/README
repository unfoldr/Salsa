
Execute the following commands to build the Salsa driver assembly:

  1. csc -filealign:512 -optimize+ -out:Salsa.dll -target:library Driver.cs

  2. runhaskell Embed Salsa.dll > ../Salsa/Driver.hs

Note: these steps are only required if changes have been made to 'Driver.cs'
      because a prebuilt Salsa.dll binary is already included in the
      distribution (and embedded in 'Driver.hs').

