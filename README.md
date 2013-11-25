Haskell PDF Tools(hpdft)
========================

Tools to poke PDF files using Haskell. 


Example
-------
main.hs is a typical application of grubbing a text from a PDF file whose stream is decoded in /FlateDecode. 

    runghc main.hs test.pdf > text.txt

If you want to customize the character sets and encodings other than the Appendix D of the PDF specification, you could modify PdfCharDict.hs. For exaple, you can use ASCII single quote (') instead of U+2019 by modifying the entry for /quoteright.
