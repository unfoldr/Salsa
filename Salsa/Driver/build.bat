csc -filealign:512 -optimize+ -out:Salsa.dll -target:library Driver.cs
runhaskell Embed Salsa.dll > ../Salsa/Driver.hs
