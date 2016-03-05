Haskell PDF Tools(hpdft)
========================

Tools to poke PDF files using Haskell. 


Example
-------
[`Sample.hs`](https://github.com/k16shikano/hpdft/blob/master/data/sample/Sample.hs) has some functions showing how to use hpdft. 

If you want to customize the character sets and encodings other than the Appendix D of the PDF specification, you could modify `PdfCharDict.hs`. For example, you can use ASCII single quote (`U+0027`) instead of `U+2019` by modifying the entry for `/quoteright`.
