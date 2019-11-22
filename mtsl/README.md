# Monad Transformer Stack Library

This is an alternative to the [Monad Transformer Library](http://hackage.haskell.org/package/mtl)
and [transformers](http://hackage.haskell.org/package/transformers) that takes advantage of 
modern GHC features to implement first class monad transformer stacks. The advantage of this
is that you can retain polymorphism while doing things like ordering your transformers. We
also get a notion of base monad which is a lot more principled, in my opinion. 

This is not full featured and is really a first draft or a proof of concept. I want to add a lot
more transformers and deal with as many of the instances as possible, as it is a bit more painful
to write these than it is to write those for the mtl.

Pull requests and issues of any kind are welcome, though I cannot promise I will accept or work
on them. 
